(in-package #:cl-user)
(defpackage #:clails-test/model/impl/postgresql
  (:use #:cl
        #:rove
        #:clails/model/impl/postgresql)
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
(in-package #:clails-test/model/impl/postgresql)

(defvar todo-pg nil)

(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                                                      :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                                                      :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0001" "/app/test/data/0001-migration-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


(deftest create-database-postgresql
  (clails/model/migration::db-create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check database exists
    (let* ((query (dbi:prepare connection "select datname from pg_database where datname = ?"))
           (result (dbi:execute query (list "clails_test"))))
      (ok (string= "clails_test" (getf (dbi:fetch result) :|datname|))))
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "select tablename from pg_tables where tablename = ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|tablename|))))))

(deftest migration-postgresql
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check schema
    (let* ((result (dbi:fetch-all (dbi:execute (dbi:prepare connection
      "select column_name, data_type, is_nullable, column_default from information_schema.columns where table_name = ? order by ordinal_position")
                                               (list "todo"))))
           (id (first result))
           (created-at (second result))
           (updated-at (third result))
           (title (fourth result))
           (done (fifth result))
           (done-at (sixth result)))
      ;; id
      (ok (string= "id" (getf id :|column_name|)))
      (ok (string= "integer" (getf id :|data_type|)))
      (ok (string= "NO" (getf id :|is_nullable|)))
      (ok (string= "nextval('todo_id_seq'::regclass)" (getf id :|column_default|)))
      ;; created_at
      (ok (string= "created_at" (getf created-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf created-at :|data_type|)))
      (ok (string= "NO" (getf created-at :|is_nullable|)))
      (ok (eq :NULL (getf created-at :|column_default|)))
      ;; updated_at
      (ok (string= "updated_at" (getf updated-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf updated-at :|data_type|)))
      (ok (string= "NO" (getf updated-at :|is_nullable|)))
      (ok (eq :NULL (getf updated-at :|column_default|)))
      ;; title
      (ok (string= "title" (getf title :|column_name|)))
      (ok (string= "character varying" (getf title :|data_type|)))
      (ok (string= "NO" (getf title :|is_nullable|)))
      (ok (eq :NULL (getf title :|column_default|)))
      ;; done
      (ok (string= "done" (getf done :|column_name|)))
      (ok (string= "boolean" (getf done :|data_type|)))
      (ok (string= "YES" (getf done :|is_nullable|)))
      (ok (eq :NULL (getf done :|column_default|)))
      ;; done_at
      (ok (string= "done_at" (getf done-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf done-at :|data_type|)))
      (ok (string= "YES" (getf done-at :|is_nullable|)))
      (ok (eq :NULL (getf done-at :|column_default|))))))


(deftest defmodel-postgresql
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  (defmodel <todo-postgresql> (<base-model>)
    (:table "todo"))

  (clails/model/base-model:initialize-table-information)

  (setf todo-pg (make-instance '<todo-postgresql>))

  ;; check member field
  (ok (null (ref todo-pg :id)))
  (ok (null (ref todo-pg :created-at)))
  (ok (null (ref todo-pg :updated-at)))
  (ok (null (ref todo-pg :title)))
  (ok (null (ref todo-pg :done)))
  (ok (null (ref todo-pg :done-at)))

  ;; error when no member field
  (ok (signals (ref todo :done-by)))

  ;; update member
  (setf (ref todo-pg :id) 1)
  (setf (ref todo-pg :created-at) "2024-01-01 00:00:00")
  (setf (ref todo-pg :updated-at) "2024-02-02 12:34:56")
  (setf (ref todo-pg :title) "refactor postgresql impl")
  (setf (ref todo-pg :done) T)
  (setf (ref todo-pg :done-at) "2024-03-03 12:00:00")

  ;; check updated
  (ok (= 1 (ref todo-pg :id)))
  (ok (string= "2024-01-01 00:00:00" (ref todo-pg :created-at)))
  (ok (string= "2024-02-02 12:34:56" (ref todo-pg :updated-at)))
  (ok (string= "refactor postgresql impl" (ref todo-pg :title)))
  (ok (eq T (ref todo-pg :done)))
  (ok (string= "2024-03-03 12:00:00" (ref todo-pg :done-at))))

