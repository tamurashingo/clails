(in-package #:cl-user)
(defpackage #:clails-test/model/pessimistic-lock/lock-sqlite3
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
  (:import-from #:clails/environment
                #:*sqlite3-busy-timeout*)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread)
  (:import-from #:dbi
                #:connect
                #:disconnect))

(defpackage #:clails-test/model/db/pessimistic-lock
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/pessimistic-lock/lock-sqlite3)

(setup
  (clrhash clails/model/base-model::*table-information*)

  (defmodel <book> (<base-model>)
    (:table "pessimistic_lock_books"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
  (setf clails/environment:*project-environment* :test)
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment:*database-config*
        `(:test (:database-name ,(namestring (merge-pathnames "db/test.db" uiop:*temporary-directory*)))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0008" "/app/test/data/0008-pessimistic-lock-test"))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool)
  (initialize-table-information))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))

(defun cleanup-books ()
  "Delete all records from pessimistic_lock_books table.

   @return [null] Always returns NIL
   "
  (let ((connection (clails/model/connection:get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM pessimistic_lock_books") '())))


(deftest test-with-lock-immediate
  (testing "with-lock :immediate executes BEGIN IMMEDIATE"
    (cleanup-books)

    (let ((book (make-record '<book> :title "Test Book" :price 1000)))
      (save book)

      (with-locked-transaction (locked-book
                                (query <book> :as :b :where (:= (:b :id) :book-id))
                                `(:book-id ,(ref book :id))
                                :mode :immediate)
        (ok (not (null locked-book)))
        (ok (string= "Test Book" (ref (first locked-book) :title)))))))

(deftest test-with-lock-exclusive
    (testing "with-lock :exclusive executes BEGIN EXCLUSIVE"
             (cleanup-books)

             (let ((book (make-record '<book> :title "Test Book" :price 1000)))
               (save book)

               (with-locked-transaction (locked-book
                                         (query <book> :as :b :where (:= (:b :id) :book-id))
                                         `(:book-id ,(ref book :id))
                                         :mode :exclusive)
                 (ok (not (null locked-book)))
                 (ok (string= "Test Book" (ref (first locked-book) :title)))))))

(deftest test-unsupported-lock-modes
  (testing "Row-level lock modes raise error in SQLite3"
    (cleanup-books)

    (let ((book (make-record '<book> :title "Error Test" :price 2000)))
      (save book)

      (ok (signals
           (with-locked-transaction (locked-book
                                     (query <book> :as :b :where (:= (:b :id) :book-id))
                                     `(:book-id ,(ref book :id))
                                     :mode :for-update)
             locked-book)
           'error)
          "FOR UPDATE should raise error in SQLite3")

      (ok (signals
           (with-locked-transaction (locked-book
                                     (query <book> :as :b :where (:= (:b :id) :book-id))
                                     `(:book-id ,(ref book :id))
                                     :mode :for-share)
             locked-book)
           'error)
          "FOR SHARE should raise error in SQLite3"))))

(deftest test-lock-contention-waits
  (testing "Lock contention: main thread waits and sees updated value"
    (cleanup-books)

    (let* ((book (make-record '<book> :title "Contention Test" :price 1000))
           (lock-acquired nil)
           (update-completed nil)
           (db-path (getf (getf clails/environment:*database-config* :test) :database-name))
           (*sqlite3-busy-timeout* 500))
      (save book)
      (let* ((book-id (ref book :id))
             (bg-thread (bt:make-thread
                         (lambda ()
                           (handler-case
                               (let ((bg-conn (dbi:connect :sqlite3 :database-name db-path)))
                                 (unwind-protect
                                      (progn
                                        (dbi:do-sql bg-conn "BEGIN IMMEDIATE")
                                        (dbi:do-sql bg-conn "SELECT * FROM pessimistic_lock_books WHERE id = ?" (list book-id))
                                        (setf lock-acquired t)
                                        (sleep 3)
                                        (dbi:do-sql bg-conn "UPDATE pessimistic_lock_books SET price = ?, updated_at = current_timestamp WHERE id = ?"
                                                   (list 2000  book-id))
                                        (dbi:do-sql bg-conn "COMMIT")
                                        (setf update-completed t))
                                   (dbi:disconnect bg-conn)))
                             (error (e)
                               (format *standard-output* "Background thread error: ~A~%" e))))
                         :name "lock-bg-thread")))

        (sleep 1)
        (ok lock-acquired "Background thread should acquire lock")
        (ok (not update-completed) "Background thread should not complete yet")

        (let ((start-time (get-universal-time)))
          (with-locked-transaction (locked-book
                                    (query <book> :as :b :where (:= (:b :id) :book-id))
                                    `(:book-id ,book-id)
                                    :mode :immediate)
            (let ((elapsed (- (get-universal-time) start-time)))
              (ok (>= elapsed 2) "Main thread should wait at least 2 seconds")
              (ok update-completed "Background thread should complete before main thread acquires lock")
              (ok (= 2000 (ref (first locked-book) :price))
                  "Main thread should see updated value from background thread"))))

        (bt:join-thread bg-thread)))))

