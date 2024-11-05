(in-package #:cl-user)
(defpackage #:clails/model/impl/sqlite3
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-sqlite3>)
  (:import-from #:clails/model/migration
                #:create-column-impl
                #:parse-add-column-impl))
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
          (if auto-increment-p "AUTO_INCREMENT" "")))

(defmethod parse-add-column-impl ((database-type <database-type-sqlite3>) columns)
  (format NIL "add column ~{ ~A ~^, ~}" columns))

