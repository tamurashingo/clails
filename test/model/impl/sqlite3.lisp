(in-package #:cl-user)
(defpackage #:clails-test/model/impl/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/impl/sqlite3))
(in-package #:clails-test/model/impl/sqlite3)

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

(setup
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
   (setf clails/environment:*database-config* '(:database "/app/volumes/clails_test.sqlite3"))
)


(deftest create-database
  (clails/model/migration::db-create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "select tbl_name from sqlite_master where tbl_name = ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|tbl_name|))))))

(deftest migration
  (clails/model/migration::db-migrate "/app/test/")
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

