(in-package #:cl-user)
(defpackage #:clails/model/impl/postgresql
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-postgresql>
                #:*database-config*)
  (:import-from #:clails/model/migration
                #:create-column-impl
                #:apply-create-database-impl
                #:apply-create-migration-table-impl
                #:parse-add-column-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl))
(in-package #:clails/model/impl/postgresql)

(defparameter *postgresql-type-convert*
  '((:string . "varchar(255)")
    (:text . "text")
    (:integer . "integer")
    (:float . "float")
    (:decimal . "decimal")
    (:datetime . "timestamp")
    (:date . "date")
    (:time . "time")
    (:boolean . "boolean")))

(defun type-convert (type)
  (cdr (assoc type *postgresql-type-convert*)))

(defmethod create-column-impl ((database-type <database-type-postgresql>) &key column-name type not-null-p primary-key-p auto-increment-p)
  (format NIL " ~A ~A ~A ~A "
          column-name
          (if auto-increment-p
              "SERIAL"
              (type-convert type))
          (if not-null-p "NOT NULL" "NULL")
          (if primary-key-p "PRIMARY KEY" "")))

(defmethod get-connection-direct-impl ((database-type <database-type-postgresql>) &key no-database)
  (let ((database-name (getf *database-config* :database))
        (host (getf *database-config* :host))
        (port (parse-integer (getf *database-config* :port)))
        (username (getf *database-config* :user))
        (password (getf *database-config* :password)))
    (if no-database
        (dbi:connect :postgres
                     :database-name "postgres"
                     :host host
                     :port port
                     :username username
                     :password password)
        (dbi:connect :postgres
                     :database-name database-name
                     :host host
                     :port port
                     :username username
                     :password password))))

(defparameter CREATE-DATABASE
  "CREATE DATABASE ~A ENCODING = 'UTF8'")

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE migration (~
                 migration_name VARCHAR(255) NOT NULL PRIMARY KEY, ~
                 created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))

(defmethod apply-create-database-impl ((database-type <database-type-postgresql>) connection)
  (let ((database-name (getf *database-config* :database)))
    (dbi:do-sql connection (format NIL CREATE-DATABASE database-name))))

(defmethod apply-create-migration-table-impl ((database-type <database-type-postgresql>) connection)
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

(defmethod parse-add-column-impl ((database-type <database-type-postgresql>) columns)
  (format NIL "~{ add column ~A ~^, ~}" columns))

