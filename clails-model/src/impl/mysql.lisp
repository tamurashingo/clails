(in-package #:cl-user)
(defpackage #:clails-model/impl/mysql
  (:use #:cl
        #:clails-model/migration))
(in-package #:clails-model/impl/mysql)

(defclass <db-mysql> (clails-model/migration::<database>) ())

(setf clails-model/migration::*db* (make-instance '<db-mysql>))

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


(defmethod type-convert (type)
  (cdr (assoc type *mysql-type-convert*)))


(defmethod create-column-impl ((db <db-mysql>) &key column-name type not-null-p primary-key-p auto-increment-p)
  (format NIL "~A ~A ~A ~A ~A"
              column-name
              (type-convert type)
              (if not-null-p "NOT NULL" "")
              (if primary-key-p "PRIMARY KEY" "")
              (if auto-increment-p "AUTO_INCREMENT" "")))
