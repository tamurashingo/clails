(in-package #:cl-user)
(defpackage #:clails-test/model/impl/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/impl/sqlite3))
(in-package #:clails-test/model/impl/sqlite3)

(setup
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
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
                      id integer NOT NULL PRIMARY KEY AUTO_INCREMENT , ~
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

