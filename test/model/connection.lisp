(in-package #:cl-user)
(defpackage #:clails-test/model/connection
  (:use #:cl
        #:rove
        #:clails/model/connection)
  (:import-from #:clails/util
                #:env-or-default))

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

(defvar *migration-dir* nil)

(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :user ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf *migration-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test"))
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate *migration-dir*)
  (startup-connection-pool))


(teardown
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

