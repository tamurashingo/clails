(in-package #:cl-user)
(defpackage #:clails-test/model/impl/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/impl/sqlite3))
(in-package #:clails-test/model/impl/sqlite3)

(setup
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
   (setf clails/environment:*database-config* '(:database "/app/volumes/clails_test.sqlite3"))
)

(defhook initialize-tbl :before
  (setf clails/model/migration::*tbl* '()))

(deftest parse-migration
  (testing "generated sql: create table"
    (let* ((s '(create-table "todo"
                (("title" :type :string)
                 ("done" :type :boolean))))
           (result-raw (clails/model/migration::parse-migration s))
           (result (ppcre:regex-replace-all "\\s+" result-raw " "))
           (expect (ppcre:regex-replace-all "\\s+" (format nil "create table todo ( ~
                      id integer NOT NULL PRIMARY KEY AUTOINCREMENT , ~
                      created_at datetime NOT NULL , ~
                      updated_at datetime NOT NULL , ~
                      title varchar(255) NULL , ~
                      done boolean NULL ~
                 )") " ")))
      (ok (string= expect result))))
  (testing "generated model data"
    (ok (equal '(("todo" . (("title" :TYPE :STRING)
                            ("done" :TYPE :BOOLEAN))))
               clails/model/migration::*tbl*)))

  (testing "generated sql: add column"
    (let* ((s '(add-column "todo"
                (("done-at" :type :datetime))))
           (result-raw (clails/model/migration::parse-migration s))
           (result (ppcre:regex-replace-all "\\s+" result-raw " "))
           (expect (ppcre:regex-replace-all "\\s+" (format nil "alter table todo add column done_at datetime NULL ") " ")))
      (ok (string= expect result))))

  (testing "updated model data"
    (ok (equal '(("todo" . (("title" :TYPE :STRING)
                            ("done" :TYPE :BOOLEAN)
                            ("done-at" :TYPE :DATETIME))))
               clails/model/migration::*tbl*))))

(deftest create-database
  (clails/model/migration::%db/create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "select tbl_name from sqlite_master where tbl_name = ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|tbl_name|))))))

(deftest migration
  (clails/model/migration::%db/migrate "/app/test/")
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
      (ok (equal 0 (getf title :|notnull|)))
      ;; done
      (ok (string= "done" (getf done :|name|)))
      (ok (string= "boolean" (getf done :|type|)))
      (ok (equal 0 (getf done :|notnull|)))
      ;; done_at
      (ok (string= "done_at" (getf done-at :|name|)))
      (ok (string= "datetime" (getf done-at :|type|)))
      (ok (equal 0 (getf done-at :|notnull|))))))

