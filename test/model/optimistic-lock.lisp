(in-package #:cl-user)
(defpackage #:clails-test/model/optimistic-lock
  (:use #:cl
        #:rove
        #:clails/model/query
        #:clails/model/query/crud
        #:clails/condition)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:initialize-table-information)
  (:import-from #:clails-test/support/temp-project
                #:make-test-project-root
                #:delete-test-project-root))

(defpackage #:clails-test/model/db/lock
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/model/optimistic-lock)

(defvar *test-project-root* nil
  "This suite's own directory under the system temporary directory, used as
   CLAILS/ENVIRONMENT:*PROJECT-DIR*.  Scoped to the suite so that TEARDOWN
   deletes only what the suite created.")

(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <lock-test> (<base-model>)
    (:table "lock_tests"
     :version :lock-version))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0004" "/app/test/data/0004-lock-test"))
  (setf *test-project-root* (make-test-project-root))
  (ensure-directories-exist (merge-pathnames "db/" *test-project-root*))
  (setf clails/environment::*project-dir* *test-project-root*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/connection:startup-connection-pool)
  (initialize-table-information))

(teardown
  (delete-test-project-root *test-project-root*)
  (clails/model/connection:shutdown-connection-pool))


(deftest optimistic-locking
  (testing "successful update should increment version"
    (let ((rec (make-record '<lock-test> :name "initial name")))
      (save rec)
      (ok (= (ref rec :lock-version) 1) "lock-version should be 1 after first save")

      (setf (ref rec :name) "updated name")
      (save rec)
      (ok (= (ref rec :lock-version) 2) "lock-version should be 2 after second save")

      (let* ((rec-from-db (first (execute-query (query <lock-test> :as :lt :where (:= (:lt :id) :id)) `(:id ,(ref rec :id))))))
        (ok (string= (ref rec-from-db :name) "updated name"))
        (ok (= (ref rec-from-db :lock-version) 2) "lock-version in db should be 2"))))

  (testing "conflicting update should raise optimistic-lock-error"
    (let ((rec (make-record '<lock-test> :name "conflict test")))
      (save rec) ; Initial save, version is 1

      (let* ((rec1 (first (execute-query (query <lock-test> :as :lt :where (:= (:lt :id) :id)) `(:id ,(ref rec :id)))))
             (rec2 (first (execute-query (query <lock-test> :as :lt :where (:= (:lt :id) :id)) `(:id ,(ref rec :id))))))

        (ok (= (ref rec1 :lock-version) 1))
        (ok (= (ref rec2 :lock-version) 1))

        ;; Update rec1, this should succeed
        (setf (ref rec1 :name) "first update")
        (save rec1)
        (ok (= (ref rec1 :lock-version) 2) "rec1 lock-version should be 2")

        ;; Try to update rec2, this should fail
        (setf (ref rec2 :name) "second update")
        (ok (signals (save rec2) 'optimistic-lock-error) "saving rec2 should signal an optimistic-lock-error")))))
