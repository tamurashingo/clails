(in-package #:cl-user)
(defpackage #:clails/model/impl/sqlite3
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-sqlite3>
                #:*database-config*)
  (:import-from #:clails/model/migration
                #:create-column-impl
                #:apply-create-database-impl
                #:apply-create-migration-table-impl
                #:parse-add-column-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl))
(in-package #:clails/model/impl/sqlite3)

(defparameter *sqlite3-type-convert*
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
  (cdr (assoc type *sqlite3-type-convert*)))

(defmethod create-column-impl ((database-type <database-type-sqlite3>) &key column-name type not-null-p primary-key-p auto-increment-p)
  (format NIL " ~A ~A ~A ~A ~A "
          column-name
          (type-convert type)
          (if not-null-p "NOT NULL" "NULL")
          (if primary-key-p "PRIMARY KEY" "")
          (if auto-increment-p "AUTOINCREMENT" "")))

(defmethod get-connection-direct-impl ((database-type <database-type-sqlite3>) &key no-database)
  (let ((database-name (getf *database-config* :database)))
    (dbi:connect :sqlite3
                 :database-name database-name)))

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE migration (~
                  migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                  created_at dateteime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))

(defmethod apply-create-database-impl ((database-type <database-type-sqlite3>) connection)
  'NOP)

(defmethod apply-create-migration-table-impl ((database-type <database-type-sqlite3>) connection)
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

(defmethod parse-add-column-impl ((database-type <database-type-sqlite3>) columns)
  (format NIL "add column ~{ ~A ~^, ~}" columns))

