(in-package #:cl-user)
(defpackage #:clails-test/model/connection
  (:use #:cl
        #:rove
        #:clails/model/connection)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails-test/support/temp-project
                #:make-test-project-root
                #:delete-test-project-root))

(defpackage #:clails-test/model/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/model/connection)

(defvar *test-project-root* nil
  "This suite's own directory under the system temporary directory, used as
   CLAILS/ENVIRONMENT:*PROJECT-DIR*.  Scoped to the suite so that TEARDOWN
   deletes only what the suite created.")


(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0001" "/app/test/data/0001-migration-test"))
  (setf *test-project-root* (make-test-project-root))
  (ensure-directories-exist (merge-pathnames "db/" *test-project-root*))
  (setf clails/environment::*project-dir* *test-project-root*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool))

(teardown
  (delete-test-project-root *test-project-root*)
  (shutdown-connection-pool))


(deftest with-db-connection-test
  (with-db-connection (connection)
    (let ((result (dbi-cp:fetch-all
                    (dbi-cp:execute
                      (dbi-cp:prepare connection "select MIGRATION_NAME from migration order by MIGRATION_NAME")
                      '()))))
      (ok (equal '((:MIGRATION_NAME "20240101-000000-create-initial-tables")
                   (:MIGRATION_NAME "20240102-000000-add-done-at-to-todo")
                   (:MIGRATION_NAME "20240103-000000-create-debug-table"))
                  result)))))

