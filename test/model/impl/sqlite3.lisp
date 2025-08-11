(in-package #:cl-user)
(defpackage #:clails-test/model/impl/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/impl/sqlite3)
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
(in-package #:clails-test/model/impl/sqlite3)

(defvar todo nil)

(setup
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
   (setf clails/environment:*project-environment* :test)
   (setf clails/environment:*database-config* `(:test (:database-name ,(format NIL "~A/volumes/clails_test.sqlite3" (env-or-default "CLAILS_SQLITE3_DATABASE" "/app")))))
   (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test"))
   (uiop:setup-temporary-directory)
   (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
   (setf clails/environment::*project-dir* uiop:*temporary-directory*))

(teardown
   (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))

(deftest create-database-sqlite3
  (clails/model/migration::db-create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "select tbl_name from sqlite_master where tbl_name = ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|tbl_name|))))))

(deftest migration-sqlite3
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check schema
    (let* ((result (dbi:fetch-all (dbi:execute (dbi:prepare connection "pragma table_info('todo')")
                                               '())))
           (id (first result))
           (created-at (second result))
           (updated-at (third result))
           (title (fourth result))
           (done (fifth result))
           (done-at (sixth result)))
      ;; id
      (ok (string= "id" (getf id :|name|)))
      (ok (string= "INTEGER" (getf id :|type|)))
      (ok (equal 1 (getf id :|notnull|)))
      (ok (equal 1 (getf id :|pk|)))
      ;; created_at
      (ok (string= "created_at" (getf created-at :|name|)))
      (ok (string= "datetime" (getf created-at :|type|)))
      (ok (equal 1 (getf created-at :|notnull|)))
      ;; updated_at
      (ok (string= "updated_at" (getf updated-at :|name|)))
      (ok (string= "datetime" (getf updated-at :|type|)))
      (ok (equal 1 (getf updated-at :|notnull|)))
      ;; title
      (ok (string= "title" (getf title :|name|)))
      (ok (string= "varchar(255)" (getf title :|type|)))
      (ok (equal 1 (getf title :|notnull|)))
      ;; done
      (ok (string= "done" (getf done :|name|)))
      (ok (string= "boolean" (getf done :|type|)))
      (ok (equal 0 (getf done :|notnull|)))
      ;; done_at
      (ok (string= "done_at" (getf done-at :|name|)))
      (ok (string= "datetime" (getf done-at :|type|)))
      (ok (equal 0 (getf done-at :|notnull|))))))


(deftest defmodel-sqlite3
    (defmodel <todo> (<base-model>))

  (setf todo (make-instance '<todo>))

(clails/model/base-model::show-model-columns todo)

  ;; check member field
  (ok (null (ref todo :id)))
  (ok (null (ref todo :created-at)))
  (ok (null (ref todo :updated-at)))
  (ok (null (ref todo :title)))
  (ok (null (ref todo :done)))
  (ok (null (ref todo :done-at)))

  ;; error when no member field
  (ok (signals (ref todo :done-by)))

  ;; update member
  (setf (ref todo :id) 1)
  (setf (ref todo :created-at) "2024-01-01 00:00:00")
  (setf (ref todo :updated-at) "2024-02-02 12:34:56")
  (setf (ref todo :title) "refactor sqlite3 impl")
  (setf (ref todo :done) T)
  (setf (ref todo :done-at) "2024-03-03 12:00:00")

  ;; check updated
  (ok (= 1 (ref todo :id)))
  (ok (string= "2024-01-01 00:00:00" (ref todo :created-at)))
  (ok (string= "2024-02-02 12:34:56" (ref todo :updated-at)))
  (ok (string= "refactor sqlite3 impl" (ref todo :title)))
  (ok (eq T (ref todo :done)))
  (ok (string= "2024-03-03 12:00:00" (ref todo :done-at))))

