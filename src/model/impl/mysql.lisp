(in-package #:cl-user)
(defpackage #:clails/model/impl/mysql
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-mysql>
                #:*database-config*)
  (:import-from #:clails/model/migration
                #:create-column-impl
                #:apply-create-database-impl
                #:apply-create-migration-table-impl
                #:parse-add-column-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl))
(in-package #:clails/model/impl/mysql)

(defparameter *mysql-type-convert*
  '((:string . "varchar(255)")
    (:text . "text")
    (:integer . "integer")
    (:float . "float")
    (:decimal . "decimal")
    (:datetime . "datetime")
    (:date . "date")
    (:time . "time")
    (:boolean . "boolean")))
  
(defun type-convert (type)
  (cdr (assoc type *mysql-type-convert*)))

(defmethod create-column-impl ((database-type <database-type-mysql>) &key column-name type not-null-p primary-key-p auto-increment-p)
  (format NIL " ~A ~A ~A ~A ~A "
          column-name
          (type-convert type)
          (if not-null-p "NOT NULL" "NULL")
          (if primary-key-p "PRIMARY KEY" "")
          (if auto-increment-p "AUTO_INCREMENT" "")))

(defmethod get-connection-direct-impl ((database-type <database-type-mysql>) &key no-database)
  (let ((database-name (getf *database-config* :database))
        (host (getf *database-config* :host))
        (port (parse-integer (getf *database-config* :port)))
        (username (getf *database-config* :user))
        (password (getf *database-config* :password)))
    (if no-database
        (dbi:connect :mysql
                     :host host
                     :port port
                     :username username
                     :password password)
        (dbi:connect :mysql
                     :database-name database-name
                     :host host
                     :port port
                     :username username
                     :password password))))


(defparameter CREATE-DATABASE
  "CREATE DATABASE ~A CHARACTER SET utf8mb4 COLLATE utf8mb4_bin")

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE migration (~
                 migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                 created_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))


(defmethod apply-create-database-impl ((database-type <database-type-mysql>) connection)
  (let ((database-name (getf *database-config* :database)))
    (dbi:do-sql connection (format NIL CREATE-DATABASE database-name))))

(defmethod apply-create-migration-table-impl ((database-type <database-type-mysql>) connection)
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

(defmethod parse-add-column-impl ((database-type <database-type-mysql>) columns)
  (format NIL "add column ~{ ~A ~^, ~}" columns))
