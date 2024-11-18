(in-package #:cl-user)
(defpackage #:clails-test/model/impl/mysql
  (:use #:cl
        #:rove
        #:clails/model/impl/mysql))
(in-package #:clails-test/model/impl/mysql)

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
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
   (setf clails/environment:*database-config* '(:database "clails_test"
                                                :user "root"
                                                :password "password"
                                                :host "host.docker.internal"
                                                :port "3306")))


(deftest create-database
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

(deftest migration
  (clails/model/migration::db-migrate "/app/test/")
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
      (ok (string= "int" (flexi-streams:octets-to-string (getf id :COLUMN_TYPE))))
      (ok (string= "PRI" (getf id :COLUMN_KEY)))
      (ok (string= "auto_increment" (getf id :EXTRA)))
      ;; created-at
      (ok (string= "created_at" (getf created-at :COLUMN_NAME)))
      (ok (string= "datetime" (flexi-streams:octets-to-string (getf created-at :COLUMN_TYPE))))
      (ok (null (getf created-at :COLUMN_KEY)))
      (ok (null (getf created-at :EXTRA)))
      ;; updated-at
      (ok (string= "updated_at" (getf updated-at :COLUMN_NAME)))
      (ok (string= "datetime" (flexi-streams:octets-to-string (getf updated-at :COLUMN_TYPE))))
      (ok (null (getf updated-at :COLUMN_KEY)))
      (ok (null (getf updated-at :EXTRA)))
      ;; title
      (ok (string= "title" (getf title :COLUMN_NAME)))
      (ok (string= "varchar(255)" (flexi-streams:octets-to-string (getf title :COLUMN_TYPE))))
      (ok (string= "MUL" (getf title :COLUMN_KEY)))
      (ok (null (getf title :EXTRA)))
      ;; done
      (ok (string= "done" (getf done :COLUMN_NAME)))
      (ok (string= "tinyint(1)" (flexi-streams:octets-to-string (getf done :COLUMN_TYPE))))
      (ok (null (getf done :COLUMN_KEY)))
      (ok (null (getf done :EXTRA)))
      ;; done-at
      (ok (string= "done_at" (getf done-at :COLUMN_NAME)))
      (ok (string= "datetime" (flexi-streams:octets-to-string (getf done-at :COLUMN_TYPE))))
      (ok (null (getf done-at :COLUMN_KEY)))
      (ok (null (getf done-at :EXTRA))))))

