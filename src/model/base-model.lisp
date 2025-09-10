(in-package #:cl-user)
(defpackage #:clails/model/base-model
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct)
  (:import-from #:clails/util
                #:kebab->snake
                #:symbol-from-string)
  (:import-from #:clails/helper/date-helper
                #:view/datetime)
  (:import-from #:jonathan
                #:with-object
                #:write-key-value)
  (:export #:<base-model>
           #:validate
           #:defmodel
           #:fetch-columns-and-types-impl
           #:fetch-columns-and-types-plist-impl
           #:ref
           #:initialize-table-information))
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
   (columns2 :documentation "property list
ex: (:id (:name :id
          :access \"ID\"
          :type :integer
          :db-cl-fn #'identity
          :cl-db-fn #'identity)
     :created-at (:name :created-at
                  :access \"CREATED_AT\"
                  :type :datetime
                  :db-cl-fn #'identity
                  :cl-db-fn #'identity))")
   (table-name :documentation "database table name")
   (save-p :initform nil)))

(defmethod initialize-instance :after ((m <base-model>) &rest initargs)
  (declare (ignore initargs))
  (loop for column in (slot-value m 'columns)
        do (let ((col (getf column :name)))
              (setf (gethash col (slot-value m 'data)) nil))))

(defmethod print-object ((obj <base-model>) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "table: ~a, value: " (slot-value obj 'table-name))
    (format stream "~{~{~a -> ~a~}~^, ~}"
            (loop for column in (slot-value obj 'columns)
                  for colname = (getf column :name)
                  with hash = (slot-value obj 'data)
                  collect (list colname (gethash colname hash))))))


(defmethod jonathan:%to-json ((obj <base-model>))
  (with-object
    (loop for column in (slot-value obj 'columns)
          for colname = (getf column :name)
          for key = (getf column :access)
          for type = (getf column :type)
          with hash = (slot-value obj 'data)
          for val = (gethash colname hash)
          do (write-key-value key
                           (cond ((eq type :datetime)
                                  (if (null val)
                                      :null
                                      (view/datetime val)))
                                 ((eq type :boolean)
                                  (if (null val)
                                      :false
                                      t))
                                 (t
                                  (if (null val)
                                      :null
                                      val)))))))


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

(defmethod (setf ref) (new-value (inst <base-model>) key)
  (multiple-value-bind (value present-p)
      (gethash key (slot-value inst 'data))
    (declare (ignorable value))
    (unless present-p
      (error "not found slot name: ~A in model ~A" key (class-name (class-of inst)))))
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

(defparameter *table-information* (make-hash-table))

(defun initialize-table-information ()
  (with-db-connection-direct (conn)
    (loop for key being each hash-key of *table-information*
            using (hash-value value)
          do (progn
               (format t "initializing ~A ... " key)
               (setf (getf value :columns)
                     (funcall (getf value :columns-fn) conn))
               (setf (getf value :columns2)
                     (funcall (getf value :columns-fn2) conn))

               ;; has-many
               (loop for has-many in (getf value :has-many)
                     as model = (getf has-many :model)
                     when (stringp model)
                       do (setf (getf has-many :model)
                                (symbol-from-string model)))

               ;; belongs-to
               (loop for belongs-to in (getf value :belongs-to)
                     as model = (getf belongs-to :model)
                     when (stringp model)
                       do (setf (getf belongs-to :model)
                                (symbol-from-string model)))

               (format t "done~%")))))

(defun debug-table-information ()
  (loop for key being each hash-key of *table-information*
          using (hash-value value)
        do (format t "key:~A, value:~A" key value)))


(defun get-columns (model-name)
  (getf (gethash model-name clails/model/base-model::*table-information*)
        :columns))

(defun get-columns-plist (model-name)
  (getf (gethash model-name clails/model/base-model::*table-information*)
        :columns2))



#|

(defmodel <account> (<base-model>)
  (:table "account"
   :has-many ((:model "<blog>"
               :as :blogs
               :foreign-key :account-id)
              (:model "<comment>"
               :as :comments
               :foreign-key :account-id)
              (:model "<comment>"
               :as :approved-comments
               :foreign-key :approved-id))))



(defmodel <blog> (<base-model>)
  (:table "blog"
   :belongs-to ((:model "<account>"
                 :column :account
                 :key :account-id))
   :has-many ((:model "<comment>"
               :as comments
               :foreign-key :blog-id))))

(defmodel <comment> (<base-model>)
  (:table "comment"
   :belongs-to ((:model "<account>"
                 :column :account
                 :key :account-id)
                (:model "<blog>"
                 :column :blog
                 :key :blog-id)
                (:model "<account>"
                 :column :approved-account
                 :key :approved-id))))


(query <blog>
  :as :blog
  :join ((:inner-join :account)
         (:left-join :comments)
         (:left-join :account :through :comments :as :comment-user)
         (:left-join :approved-account :through :comments))
|#


(defun validate-has-many (val)
  "returns missing mandatory keys"
  (let (errors)
    (unless (getf val :model)
      (push :model errors))
    (unless (getf val :as)
      (push :as errors))
    (unless (getf val :foreign-key)
      (push :foreign-key errors))
    errors))

(defun validate-belongs-to (val)
  "return missing madatory keys"
  (let (errors)
    (unless (getf val :model)
      (push :model errors))
    (unless (getf val :column)
      (push :column errors))
    (unless (getf val :key)
      (push :key errors))
    errors))



(defmacro defmodel (class-name superclass options)
  (let* ((cls-name (intern (string `,class-name) *package*))
         (table-name (anaphora:aif (getf options :table)
                                   anaphora:it
                                   (model->tbl `,class-name)))
         (fn-name (intern (format NIL "%MAKE-~A-INITFORM" `,class-name)))
         (has-many-errors (loop for has-many in (getf options :has-many)
                               as check = (validate-has-many has-many)
                               when check
                                 append check))
         (belongs-to-errors (loop for belongs-to in (getf options :belongs-to)
                                 as check = (vlaidate-belongs-to belongs-to)
                                 when check
                                   append check)))

    (when (or has-many-errors belongs-to-errors)
      (error "defmodel ~A:~@[ :has-many not found following parameter: ~{~A~^, ~}~]~@[ :belongs-to not found following parameter: ~{~A~^, ~}~]" class-name has-many-errors belongs-to-errors))

    `(progn
       (defun ,fn-name ()
         (let ((result
                 (with-db-connection-direct (conn)
                   (fetch-columns-and-types-impl *database-type* conn ,table-name))))
           result))

       (defclass ,cls-name ,superclass
         ((table-name :initform ,table-name)
          (columns :initform (clails/model/base-model::get-columns ',cls-name))
          (columns2 :initform (clails/model/base-model::get-columns ',cls-name))))

       (setf (gethash  ',cls-name clails/model/base-model::*table-information*)
             (list :table-name ,table-name
                   :columns-fn #'(lambda (conn)
                                   (clails/model/base-model::fetch-columns-and-types-impl clails/environment:*database-type*  conn ,table-name))
                   :columns-fn2 #'(lambda (conn)
                                    (clails/model/base-model::fetch-columns-and-types-plist-impl clails/environment:*database-type* conn ,table-name))
                   :columns nil
                   :columns2 nil
                   :belongs-to ',(getf options :belongs-to)
                   :has-one ',(getf options :has-one)
                   :has-many ',(getf options :has-many))))))


(defgeneric fetch-columns-and-types-impl (database-type connection table)
  (:documentation "Implemantation of fetch column and its type"))

(defgeneric fetch-columns-and-types-plist-impl (database-type connection table)
  (:documentation "Implemantation of fetch column and its type plist
ex: (:id (:name :id
          :access \"ID\"
          :type :integer
          :db-cl-fn #'identity
          :cl-db-fn #'identity)
     :created-at (:name :created-at
                  :access \"CREATED_AT\"
                  :type :datetime
                  :db-cl-fn #'db-datetime->cl-datetime
                  :cl-db-fn #'cl-datetime->db-datetime)
     .....)
"))


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
