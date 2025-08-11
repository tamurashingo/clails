(in-package #:cl-user)
(defpackage #:clails-test/model/impl/mysql
  (:use #:cl
        #:rove
        #:clails/model/impl/mysql)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))
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
(in-package #:clails-test/model/impl/mysql)

(defvar todo-my nil)

(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :user ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


(deftest create-database-mysql
  (clails/model/migration::db-create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check database exists
    (let* ((query (dbi:prepare connection "show databases like ?"))
           (result (dbi:execute query (list "clails_test"))))
      (ok (string= "clails_test" (getf (dbi:fetch result) :|Database (clails_test)|))))
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "show tables like ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|Tables_in_clails_test (migration)|))))))

(deftest migration-mysql
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check schema
    (let* ((result (dbi:fetch-all (dbi:execute (dbi:prepare connection
      "select column_name, column_type, column_key, extra from information_schema.columns where table_name = ? order by ordinal_position")
                                             (list "todo"))))
           (id (first result))
           (created-at (second result))
           (updated-at (third result))
           (title (fourth result))
           (done (fifth result))
           (done-at (sixth result)))
      ;; id
      (ok (string= "id" (getf id :COLUMN_NAME)))
      (ok (string= "int" (babel:octets-to-string (getf id :COLUMN_TYPE))))
      (ok (string= "PRI" (getf id :COLUMN_KEY)))
      (ok (string= "auto_increment" (getf id :EXTRA)))
      ;; created-at
      (ok (string= "created_at" (getf created-at :COLUMN_NAME)))
      (ok (string= "datetime" (babel:octets-to-string (getf created-at :COLUMN_TYPE))))
      (ok (null (getf created-at :COLUMN_KEY)))
      (ok (null (getf created-at :EXTRA)))
      ;; updated-at
      (ok (string= "updated_at" (getf updated-at :COLUMN_NAME)))
      (ok (string= "datetime" (babel:octets-to-string (getf updated-at :COLUMN_TYPE))))
      (ok (null (getf updated-at :COLUMN_KEY)))
      (ok (null (getf updated-at :EXTRA)))
      ;; title
      (ok (string= "title" (getf title :COLUMN_NAME)))
      (ok (string= "varchar(255)" (babel:octets-to-string (getf title :COLUMN_TYPE))))
      (ok (string= "MUL" (getf title :COLUMN_KEY)))
      (ok (null (getf title :EXTRA)))
      ;; done
      (ok (string= "done" (getf done :COLUMN_NAME)))
      (ok (string= "tinyint(1)" (babel:octets-to-string (getf done :COLUMN_TYPE))))
      (ok (null (getf done :COLUMN_KEY)))
      (ok (null (getf done :EXTRA)))
      ;; done-at
      (ok (string= "done_at" (getf done-at :COLUMN_NAME)))
      (ok (string= "datetime" (babel:octets-to-string (getf done-at :COLUMN_TYPE))))
      (ok (null (getf done-at :COLUMN_KEY)))
      (ok (null (getf done-at :EXTRA))))))


(deftest defmodel-mysql
  (defmodel <todo-mysql> (<base-model>)
    (:table "todo"))

  (setf todo-my (make-instance '<todo-mysql>))

  ;; check member field
  (ok (null (ref todo-my :id)))
  (ok (null (ref todo-my :created-at)))
  (ok (null (ref todo-my :updated-at)))
  (ok (null (ref todo-my :title)))
  (ok (null (ref todo-my :done)))
  (ok (null (ref todo-my :done-at)))

  ;; error when no member field
  (ok (signals (ref todo-my :done-by)))

  ;; update member
  (setf (ref todo-my :id) 1)
  (setf (ref todo-my :created-at) "2024-01-01 00:00:00")
  (setf (ref todo-my :updated-at) "2024-02-02 12:34:56")
  (setf (ref todo-my :title) "refactor mysql impl")
  (setf (ref todo-my :done) T)
  (setf (ref todo-my :done-at) "2024-03-03 12:00:00")

  ;; check updated
  (ok (= 1 (ref todo-my :id)))
  (ok (string= "2024-01-01 00:00:00" (ref todo-my :created-at)))
  (ok (string= "2024-02-02 12:34:56" (ref todo-my :updated-at)))
  (ok (string= "refactor mysql impl" (ref todo-my :title)))
  (ok (eq T (ref todo-my :done)))
  (ok (string= "2024-03-03 12:00:00" (ref todo-my :done-at))))

