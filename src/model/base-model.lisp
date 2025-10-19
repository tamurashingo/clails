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
           #:ref-error
           #:ref-in
           #:has-dirty-p
           #:has-error-p
           #:frozen-p
           #:clear-error
           #:clear-dirty-flag
           #:initialize-table-information))
(in-package #:clails/model/base-model)


(defclass <base-model> ()
  ((data :initform (make-hash-table :test #'eq)
         :documentation "A hash table that holds the columns (:key) and their values for the table")
   (dirty-flag :initform (make-hash-table :test #'eq)
               :documentation "")
   (has-dirty-p :initform nil
                :reader has-dirty-p)
   (columns :documentation "list of column
ex: ((:name :id
      :access \"ID\"
      :type :integer
      :db-cl-fn #'identity
      :cl-db-fn #'identity)
    (:name :created-at
     :access \"CREATED_AT\"
     :type :datetime
     :db-cl-fn #'identity
     :cl-db-fn #'identity))")
   (table-name :documentation "database table name")
   (errors :initform (make-hash-table))
   (has-error-p :initform nil
                :reader has-error-p)
   (frozen-p :initform nil
             :reader frozen-p)))

(defmethod print-object ((obj <base-model>) stream)
  (print-unreadable-object (obj stream :type t)
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
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name *table-information*))
         (columns-plist (getf table-info :columns-plist))
         (relations-ht (getf table-info :relations)))

    ;; Check if the key is a DB column or a defined relation alias
    (if (or (find key '(:id :created-at :updated-at))
            (getf columns-plist key)
            (and relations-ht (gethash key relations-ht)))
        ;; If it's a valid key, get the value from the instance's data hash-table
        ;; (gethash returns nil if the key doesn't exist)
        (gethash key (slot-value inst 'data))
        ;; Otherwise, throw an error as intended
        (error "not found slot name: `~A` is not a column or a defined relation in model `~A`"
               key
               class-name))))

(defmethod ref-error ((inst <base-model>) key)
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name *table-information*))
         (columns-plist (getf table-info :columns-plist)))
    (if (getf columns-plist key)
        (gethash key (slot-value inst 'errors))
        (error "not found slot-name: `~A` is not a column in model `~A`'"
               key
               class-name))))

(defmethod (setf ref) (new-value (inst <base-model>) key)
  (multiple-value-bind (old-value key-exists-p) (gethash key (slot-value inst 'data))
    (unless (and key-exists-p (value= old-value new-value))
      (setf (gethash key (slot-value inst 'data))
            new-value)
      (let* ((class-name (class-name (class-of inst)))
             (table-info (gethash class-name *table-information*))
             (columns-plist (getf table-info :columns-plist))
             (is-db-column (or (find key '(:id :created-at :updated-at))
                               (getf columns-plist key))))
        ;; Only set dirty-flag for database columns, not for relations
        (when is-db-column
          (setf (gethash key (slot-value inst 'dirty-flag))
                t)
          (setf (slot-value inst 'has-dirty-p) t))))))

(defmethod (setf ref-error) (error-value (inst <base-model>) key)
  (setf (gethash key (slot-value inst 'errors))
        error-value)
  (setf (slot-value inst 'has-error-p) t))

(defmethod clear-error ((inst <base-model>))
  (clrhash (slot-value inst 'errors))
  (setf (slot-value inst 'has-error-p) nil))

(defmethod clear-dirty-flag ((inst <base-model>))
  (clrhash (slot-value inst 'dirty-flag))
  (setf (slot-value inst 'has-dirty-p) nil))


(defun value= (old new)
  (or (and (eq t old)
           (eq t new))
      (and (null old)
           (null new))
      (and (numberp old)
           (numberp new)
           (= old new))
      (and (stringp old)
           (stringp new)
           (string= old new))
      (and (symbolp old)
           (symbolp new)
           (eq old new))))


(defmacro ref-in (instance &rest path)
  "Deeply accesses nested model relations.
Path segments can be:
- a keyword for a relation (e.g., :comments)
- a number for an index (e.g., 0)
- a symbol bound to an index
- a list for a function call (e.g., (nth 0))

Example: (ref-in blog :comments 0 :approved-account)"
  (let ((expansion instance))
    (dolist (segment path)
      (setf expansion
            (cond
              ((keywordp segment)
               `(ref ,expansion ,segment))
              ((listp segment)
               `(,(car segment) ,expansion ,@(cdr segment)))
              (t ;; Assume number or symbol for list index
               `(nth ,segment ,expansion)))))
    expansion))


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


(defparameter *table-information* (make-hash-table)
              "key: model class name
               value: plist
                :table-name <string>
                :columns <list of columns>
                :columns-fn <function to fetch columns>
                :columns-plist <plist of columns>
                :columns-plist-fn <function to fetch columns plist>
                :relations <hash-table of relations>
                :version-column <keyword or nil>")


(defun initialize-table-information ()
  (with-db-connection-direct (conn)
    (loop for key being each hash-key of *table-information*
            using (hash-value value)
          do (progn
               (format t "initializing ~A ... " key)
               (setf (getf value :columns)
                     (funcall (getf value :columns-fn) conn))
               (setf (getf value :columns-plist)
                     (funcall (getf value :columns-plist-fn) conn))

               ;; check version column type
               (let ((version-column-name (getf value :version-column))
                     (columns-plist (getf value :columns-plist)))
                 (when version-column-name
                   (let* ((column-info (getf columns-plist version-column-name))
                          (column-type (getf column-info :type)))
                     (unless (eq column-type :integer)
                       (error "Optimistic locking version column '~A' for model '~A' must be of type :integer, but is '~A'."
                              version-column-name
                              key
                              column-type)))))

               ;; initialize relations
               (let ((relations (getf value :relations)))
                 (when relations
                   (maphash (lambda (alias rel-info)
                              (declare (ignore alias))
                              (let ((model (getf rel-info :model)))
                                (when (stringp model)
                                  (setf (getf rel-info :model)
                                        (symbol-from-string model)))))
                            relations)))
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
        :columns-plist))



#|

(defmodel <account> (<base-model>)
  (:table "account"
   :relations ((:has-many "<blog>"
                :as :blogs
                :foreign-key :account-id)
               (:has-many "<comment>"
                :as :comments
                :foreign-key :approved-id))))

(defmodel <blog> (<base-model>)
  (:table "blog"
   :relations ((:belongs-to "<account>"
                :column :account
                :key :account-id)
               (:has-many "<comment>"
                :as :comments
                :foreign-key :blog-id))))

(defmodel <comment> (<base-model>)
  (:table "comment"
   :relations ((:belongs-to "<account>"
               :column :approved-account
               :key :approved-id)
              (:belongs-to "<blog>"
               :column :blog
               :key :blog-id))))


(query <blog>
  :as :blog
  :joins ((:inner-join :account)
          (:left-join :comments)
          (:left-join :approved-account :through :comments))
  :where (> (:blog :star) 0))

|#


(defun validate-relation (val)
  "returns missing mandatory keys"
  (let (errors)
    (unless (listp val) (return-from validate-relation '(:invalid-format)))
    (let ((type (first val))
          (model (second val))
          (options (cddr val)))
      (unless (stringp model) (push :model errors))
      (case type
        (:has-many
         (unless (getf options :as) (push :as errors))
         (unless (getf options :foreign-key) (push :foreign-key errors)))
        (:belongs-to
         (unless (getf options :column) (push :column errors))
         (unless (getf options :key) (push :key errors)))
        (otherwise (push :type errors))))
    errors))


(defmacro defmodel (class-name superclass options)
  (let* ((cls-name (intern (string `,class-name) *package*))
         (table-name (anaphora:aif (getf options :table)
                                   anaphora:it
                                   (model->tbl `,class-name)))
         (fn-name (intern (format NIL "%MAKE-~A-INITFORM" `,class-name)))
         (relations (getf options :relations))
         (version-column (getf options :version))
         (errors nil)
         (relations-ht (make-hash-table :test #'eq)))

    (loop for rel in relations
          do (let* ((type (first rel))
                    (validation-errors (validate-relation rel)))
               (if validation-errors
                   (push (format nil "Invalid relation ~S. Missing keys: ~A" rel validation-errors) errors)
                   (let* ((options (cddr rel))
                          (alias (case type
                                   (:has-many (getf options :as))
                                   (:belongs-to (getf options :column)))))
                     (setf (gethash alias relations-ht)
                           (list* :type type
                                  :model (second rel)
                                  options))))))
    (when errors
      (error "defmodel ~A:~{~%  ~A~}" class-name (reverse errors)))

    `(progn
       (defun ,fn-name ()
         (let ((result
                 (with-db-connection-direct (conn)
                   (fetch-columns-and-types-impl *database-type* conn ,table-name))))
           result))

       (defclass ,cls-name ,superclass
         ((table-name :initform ,table-name)
          (columns :initform (clails/model/base-model::get-columns ',cls-name))))

       (setf (gethash  ',cls-name clails/model/base-model::*table-information*)
             (list :table-name ,table-name
                   :columns-fn #'(lambda (conn)
                                   (clails/model/base-model::fetch-columns-and-types-impl clails/environment:*database-type*  conn ,table-name))
                   :columns-plist-fn #'(lambda (conn)
                                    (clails/model/base-model::fetch-columns-and-types-plist-impl clails/environment:*database-type* conn ,table-name))
                   :columns nil
                   :columns-plist nil
                   :relations ,relations-ht
                   :version-column ,version-column)))))


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
