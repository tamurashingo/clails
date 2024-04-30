(in-package #:cl-user)
(defpackage #:clails-entity.migration
  (:use #:cl))
(in-package #:clails-entity.migration)

(defparameter *tbl* '())

(defun find-table (table)
  (assoc table *tbl*))

(defmacro create-table (table body)
  (when (find-table table)
    (error "~A already defined" table))
  `(setq *tbl* (push (cons ',table ',body) *tbl*)))

(defmacro add-column (table columns)
  (when (null (find-table table))
    (error "~A not defined yet" table))
  `(let ((table-columns (cdr (find-table ',table))))
     (nconc table-columns ',columns)))
