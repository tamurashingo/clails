(in-package #:cl-user)
(defpackage #:clails-test/model/pessimistic-lock/lock-mysql
  (:use #:cl
        #:rove
        #:clails/model)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:initialize-table-information)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread))

(defpackage #:clails-test/model/db/pessimistic-lock
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/pessimistic-lock/lock-mysql)

(setup
  (clrhash clails/model/base-model::*table-information*)

  (defmodel <book> (<base-model>)
    (:table "pessimistic_lock_books"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config*
        `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0008" "/app/test/data/0008-pessimistic-lock-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool)
  (initialize-table-information))

(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (shutdown-connection-pool))

(defun cleanup-books ()
  "Delete all records from pessimistic_lock_books table.

   @return [null] Always returns NIL
   "
  (let ((connection (clails/model/connection:get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM pessimistic_lock_books") '())))


(deftest test-with-lock-for-update
  (testing "with-lock FOR UPDATE locks record"
    (cleanup-books)

    (let ((book (make-record '<book> :title "Test Book" :price 1000)))
      (save book)


      (with-locked-transaction (locked-book
                                (query <book> :as :b :where (:= (:b :id) :book-id))
                                `(:book-id ,(ref book :id))
                                :mode :for-update)
        (ok (not (null locked-book)))
        (ok (string= "Test Book" (ref (first locked-book) :title)))))))


(deftest test-with-lock-for-share
  (testing "with-lock FOR SHARE locks record"
    (cleanup-books)

    (let ((book (make-record '<book> :title "Shared Book" :price 2000)))
      (save book)

      (with-locked-transaction (locked-book
                                (query <book> :as :b :where (:= (:b :id) :book-id))
                                `(:book-id ,(ref book :id))
                                :mode :for-share)
        (ok (not (null locked-book)))
        (ok (string= "Shared Book" (ref (first locked-book) :title)))))))

(deftest test-with-lock-nowait
  (testing "with-lock with NOWAIT option"
    (cleanup-books)

    (let ((book (make-record '<book> :title "NOWAIT Test" :price 3000)))
      (save book)

      (with-locked-transaction (locked-book
                                (query <book> :as :b :where (:= (:b :id) :book-id))
                                `(:book-id ,(ref book :id))
                                :mode :for-update
                                :nowait t)
        (ok (not (null locked-book)))
        (ok (string= "NOWAIT Test" (ref (first locked-book) :title)))))))

(deftest test-with-lock-skip-locked
  (testing "with-lock with SKIP LOCKED option"
    (cleanup-books)

    (let ((book (make-record '<book> :title "SKIP LOCKED Test" :price 4000)))
      (save book)

      (with-locked-transaction (locked-book
                                (query <book> :as :b :where (:= (:b :id) :book-id))
                                `(:book-id ,(ref book :id))
                                :mode :for-update
                                :skip-locked t)
        (ok (not (null locked-book)))
        (ok (string= "SKIP LOCKED Test" (ref (first locked-book) :title)))))))


(deftest test-unsupported-lock-mode
  (testing "Unsupported lock modes raise error"
    (cleanup-books)

    (let ((book (make-record '<book> :title "Error Test" :price 5000)))
      (save book)

      (ok (signals
           (with-locked-transaction (locked-book
                                     (query <book> :as :b :where (:= (:b :id) :book-id))
                                     `(:book-id ,(ref book :id))
                                     :mode :for-no-key-update)
             locked-book)
           'error)
          "FOR NO KEY UPDATE should raise error in MySQL"))))

(deftest test-lock-contention-waits
  (testing "Lock contention: main thread waits and sees updated value"
    (cleanup-books)

    (let* ((book (make-record '<book> :title "Contention Test" :price 1000))
           (lock-acquired nil)
           (update-completed nil))
      (save book)
      (let* ((book-id (ref book :id))
             (bg-thread (bt:make-thread
                         (lambda ()
                           (handler-case
                               (with-locked-transaction (locked-book
                                                         (query <book> :as :b :where (:= (:b :id) :book-id))
                                                         `(:book-id ,book-id)
                                                         :mode :for-update)
                                 (setf lock-acquired t)
                                 (sleep 3)
                                 (setf (ref (first locked-book) :price) 2000)
                                 (save (first locked-book))
                                 (setf update-completed t))
                             (error (e)
                               (format t "Background thread error: ~A~%" e))))
                         :name "lock-bg-thread")))

        (sleep 1)
        (ok lock-acquired "Background thread should acquire lock")
        (ok (not update-completed) "Background thread should not complete yet")

        (let ((start-time (get-universal-time)))
          (with-locked-transaction (locked-book
                                    (query <book> :as :b :where (:= (:b :id) :book-id))
                                    `(:book-id ,book-id)
                                    :mode :for-update)
            (let ((elapsed (- (get-universal-time) start-time)))
              (ok (>= elapsed 2) "Main thread should wait at least 2 seconds")
              (ok update-completed "Background thread should complete before main thread acquires lock")
              (ok (= 2000 (ref (first locked-book) :price))
                  "Main thread should see updated value from background thread in SELECT FOR UPDATE"))))

        (bt:join-thread bg-thread)))))

(deftest test-lock-contention-nowait                                                                                  [1/1912]
  (testing "Lock contention: NOWAIT raises error immediately"
    (cleanup-books)

    (let* ((book (make-record '<book> :title "NOWAIT Contention Test" :price 1000))
           (lock-acquired nil))
      (save book)
      (let* ((book-id (ref book :id))
             (bg-thread (bt:make-thread
                         (lambda ()
                           (handler-case
                               (with-locked-transaction (locked-book
                                                         (query <book> :as :b :where (:= (:b :id) :book-id))
                                                         `(:book-id ,book-id)
                                                         :mode :for-update)
                                 (setf lock-acquired t)
                                 (sleep 3))
                             (error (e)
                               (format t "Background thread error: ~A~%" e))))
                         :name "nowait-bg-thread")))

        (sleep 1)
        (ok lock-acquired "Background thread should acquire lock")

        (ok (signals (with-locked-transaction (locked-book
                                               (query <book> :as :b :where (:= (:b :id) :book-id))
                                               `(:book-id ,book-id)
                                               :mode :for-update
                                               :nowait t)
                       (ok nil "Should not reach here"))
                     'error)
            "NOWAIT should raise error when lock is held")

        (bt:join-thread bg-thread)))))

(deftest test-lock-contention-skip-locked
  (testing "Lock contention: SKIP LOCKED returns unlocked rows only"
    (cleanup-books)

    (let* ((book1 (make-record '<book> :title "Skip Test 1" :price 1000))
           (book2 (make-record '<book> :title "Skip Test 2" :price 2000))
           (lock-acquired nil))
      (save book1)
      (save book2)
      (let* ((book1-id (ref book1 :id))
             (book2-id (ref book2 :id))
             (bg-thread (bt:make-thread
                         (lambda ()
                           (handler-case
                               (with-locked-transaction (locked-book
                                                         (query <book> :as :b :where (:= (:b :id) :book-id))
                                                         `(:book-id ,book1-id)
                                                         :mode :for-update)
                                 (setf lock-acquired t)
                                 (sleep 3))
                             (error (e)
                               (format t "Background thread error: ~A~%" e))))
                         :name "skip-locked-bg-thread")))

        (sleep 1)
        (ok lock-acquired "Background thread should acquire lock on book1")

        (with-locked-transaction (locked-books
                                  (query <book> :as :b :where (:in (:b :id) :book-ids))
                                  `(:book-ids (,book1-id ,book2-id))
                                  :mode :for-update
                                  :skip-locked t)
          (ok (= 1 (length locked-books))
              "SKIP LOCKED should return only unlocked book")
          (ok (= book2-id (ref (first locked-books) :id))
              "SKIP LOCKED should return book2 (unlocked)"))

        (bt:join-thread bg-thread)))))
