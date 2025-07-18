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
           #:validate
           #:defmodel
           #:fetch-columns-and-types-impl
           #:ref))
(in-package #:clails/model/base-model)


(defclass <base-model> ()
  ((data :initform (make-hash-table :test #'eq)
         :documentation "A hash table that holds the columns (:key) and their values for the table")
   (columns :documentation "Holds information about columns in the form of a plist. This value varies depending on the DB implementation.
- :name - [string] the name of the column
- :access - [keyword] the key used to retrieve values from the database (note: in PostgreSQL, keywords are converted to lowercase, so this is defined separately from :name)
- :type - [keyword] the type specified during migration
- :db-cl-fn - [function] function to convert database values to Common Lisp values
- :cl-db-fn - [function] function to convert Common Lisp values to database values

'ex: ((:name :id
       :access \"ID\"
       :type :integer
       :convert-fn #'identity)
      (:name :created-at
       :access \"CREATED-AT\"
       :type :datetime
       :function #'identity)
      (:name :updated-at
       :access \"UPDATED-AT\"
       :type :datetime
       :function #'identity)
      (:name :title
       :access \"TITLE\"
       :type :text
       :function #'babel:octets-to-string))")
   (table-name :documentation "database table name")
   (save-p :initform nil)))

(defmethod initialize-instance :after ((m <base-model>) &rest initargs)
  (declare (ignore initargs))
  (loop for column in (slot-value m 'columns)
        do (let ((col (getf column :name)))
              (setf (gethash col (slot-value m 'data)) nil))))

(defgeneric validate (inst)
  (:documentation "validate mode before save")
  (:method ((inst <base-model>))
    t))


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


(defun show-model-data (model)
  (maphash #'(lambda (key value)
               (format t "~A:~A~%" key value))
           (slot-value model 'data)))
(defun show-model-columns (model)
  (format t "show-model-columns: model: ~A~%" model)
  (loop for col in (slot-value model 'columns)
        do (format t "show-model-columns: column: ~A~%" col)))


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
        (fn-name (intern (format NIL "%MAKE-~A-INITFORM" cls-name))))
    `(progn
       (defun ,fn-name ()
         (let ((result
                 (with-db-connection-direct (conn)
                   (fetch-columns-and-types-impl *database-type* conn ,table-name))))
           result))
       (defclass ,cls-name ,superclass
         ((table-name :initform ,table-name)
          (columns :initform (,fn-name)))))))

(defgeneric fetch-columns-and-types-impl (database-type connection tabole)
  (:documentation "Implemantation of fetch column and its type"))


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
