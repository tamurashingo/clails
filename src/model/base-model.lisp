(in-package #:cl-user)
(defpackage #:clails/model/base-model
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct)
  (:import-from #:clails/util
                #:kebab->snake)
  (:export #:<base-model>
           #:defmodel
           #:fetch-columns-impl
           #:ref))
(in-package #:clails/model/base-model)


(defclass <base-model> ()
  ((data :initform (make-hash-table))
   (columns)
   (table-name)
   (save-p :initform nil)))

(defmethod initialize-instance :after ((m <base-model>) &rest initargs)
  (declare (ignore initargs))
  (loop for col in (slot-value m 'columns)
        do (setf (gethash col (slot-value m 'data)) nil)))


(defmethod ref ((inst <base-model>) key)
  (multiple-value-bind (value present-p)
      (gethash key (slot-value inst 'data))
    (when (not present-p)
      (error "not found slot name: ~A" key))
    value))

(defun (setf ref) (new-value inst key)
  (multiple-value-bind (value present-p)
      (gethash key (slot-value inst 'data))
    (declare (ignorable value))
    (when (not present-p)
      (error "not found slot name: ~A" key)))
  (setf (gethash key (slot-value inst 'data))
        new-value))

(defun show-model (model)
  (maphash #'(lambda (key value)
               (format t "~A:~A~%" key value))
           (slot-value model 'data)))


(defun model->tbl (sym)
  "convert model name to table name.
   <todo> -> \"TODO\"
   <todo-history> -> \"TODO_HISTORY\""
  (kebab->snake
   (ppcre:regex-replace-all
      "\>"
      (ppcre:regex-replace-all
         "\<"
         (format NIL "~S" sym)
         "")
      "")))

(defmacro defmodel (cls-name superclass &optional options)
  (let ((table-name (anaphora:aif (getf options :table)
                                  anaphora:it
                                  (model->tbl cls-name)))
        (fn-name (intern (format NIL "%MAKE-~S-INITFORM" cls-name))))
    `(progn
       (defun ,fn-name ()
         (with-db-connection-direct (conn)
           (fetch-columns-impl *database-type* conn ,table-name)))
       (defclass ,cls-name ,superclass
         ((table-name :initform ,table-name)
          (columns :initform (,fn-name)))))))

(defgeneric fetch-columns-impl (database-type connection tabole)
  (:documentation "Implemantation of fetch column"))


;;
;; (defmodel <todo> (<base-model>)
;;   (:table "todo"))
;;
;; (defvar todo (make-instance '<todo>))
;;
;; (setf (ref todo :title) "refactor all products")
;; (show-model todo)
;;
;; ID:NIL
;; CREATED-AT:NIL
;; UPDATED-AT:NIL
;; TITLE:refactor all products
;; DONE:NIL
;;
