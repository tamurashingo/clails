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
         :documentation "Hash table holding column keys and their values for the database table")
   (dirty-flag :initform (make-hash-table :test #'eq)
               :documentation "Hash table tracking which columns have been modified")
   (has-dirty-p :initform nil
                :reader has-dirty-p
                :documentation "Flag indicating whether any columns have been modified")
   (columns :documentation "List of column specifications.
Example: ((:name :id
           :access \"ID\"
           :type :integer
           :db-cl-fn #'identity
           :cl-db-fn #'identity)
          (:name :created-at
           :access \"CREATED_AT\"
           :type :datetime
           :db-cl-fn #'identity
           :cl-db-fn #'identity))")
   (table-name :documentation "Database table name")
   (errors :initform (make-hash-table)
           :documentation "Hash table holding validation errors for each column")
   (has-error-p :initform nil
                :reader has-error-p
                :documentation "Flag indicating whether any validation errors exist")
   (frozen-p :initform nil
             :reader frozen-p
             :documentation "Flag indicating whether the model instance is frozen (read-only)"))
  (:documentation "Base class for all model classes representing database tables."))

(defmethod print-object ((obj <base-model>) stream)
  "Print model instance showing column names and values.
   
   @param obj [<base-model>] Model instance
   @param stream [stream] Output stream
   "
  (print-unreadable-object (obj stream :type t)
    (format stream "~{~{~a -> ~a~}~^, ~}"
            (loop for column in (slot-value obj 'columns)
                  for colname = (getf column :name)
                  with hash = (slot-value obj 'data)
                  collect (list colname (gethash colname hash))))))


(defmethod jonathan:%to-json ((obj <base-model>))
  "Convert model instance to JSON format.
   
   Serializes all columns according to their types:
   - datetime: formatted using view/datetime
   - boolean: converted to true/false
   - null values: represented as :null
   
   @param obj [<base-model>] Model instance
   @return [string] JSON representation
   "
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
  (:documentation "Validate model instance before saving.
   
   Override this method to implement custom validation logic.
   Return T if valid, NIL or signal error if invalid.
   
   @param inst [<base-model>] Model instance to validate
   @return [boolean] T if valid
   ")
  (:method ((inst <base-model>))
    t))

(defmethod ref ((inst <base-model>) key)
  "Get the value of a model column or relation.
   
   Retrieves values for database columns (:id, :created-at, :updated-at, or columns
   defined in the model) or relation aliases. Signals an error if key is invalid.
   
   @param inst [<base-model>] Model instance
   @param key [keyword] Column or relation key
   @return [t] Column value, or NIL if not set
   @condition error Signaled when key is not a valid column or relation
   "
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
  "Get the validation error for a specific column.
   
   @param inst [<base-model>] Model instance
   @param key [keyword] Column key
   @return [t] Error value, or NIL if no error
   @condition error Signaled when key is not a valid column
   "
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name *table-information*))
         (columns-plist (getf table-info :columns-plist)))
    (if (getf columns-plist key)
        (gethash key (slot-value inst 'errors))
        (error "not found slot-name: `~A` is not a column in model `~A`'"
               key
               class-name))))

(defmethod (setf ref) (new-value (inst <base-model>) key)
  "Set the value of a model column or relation.

   Tracks changes by setting dirty-flag for database columns when:
   - The key doesn't exist in the data hash table (first-time set)
   - The value is different from the current value

   This enables insert1 to only insert explicitly set columns, allowing
   database default values to work correctly.

   @param new-value [t] New value to set
   @param inst [<base-model>] Model instance
   @param key [keyword] Column or relation key
   @return [t] The new value
   "
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
  "Set validation error for a specific column.
   
   @param error-value [t] Error value or message to set
   @param inst [<base-model>] Model instance
   @param key [keyword] Column key
   @return [t] The error value
   "
  (setf (gethash key (slot-value inst 'errors))
        error-value)
  (setf (slot-value inst 'has-error-p) t))

(defmethod clear-error ((inst <base-model>))
  "Clear all validation errors from the model instance.
   
   @param inst [<base-model>] Model instance
   "
  (clrhash (slot-value inst 'errors))
  (setf (slot-value inst 'has-error-p) nil))

(defmethod clear-dirty-flag ((inst <base-model>))
  "Clear all dirty flags from the model instance.
   
   Resets the tracking of modified columns.
   
   @param inst [<base-model>] Model instance
   "
  (clrhash (slot-value inst 'dirty-flag))
  (setf (slot-value inst 'has-dirty-p) nil))


(defun value= (old new)
  "Compare two values for equality.
   
   Handles comparison for various types: boolean, null, number, string, and symbol.
   
   @param old [t] Old value
   @param new [t] New value
   @return [boolean] T if values are equal, NIL otherwise
   "
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
  "Deeply access nested model relations.
   
   Path segments can be:
   - A keyword for a relation (e.g., :comments)
   - A number for an index (e.g., 0)
   - A symbol bound to an index
   - A list for a function call (e.g., (nth 0))
   
   @param instance [<base-model>] Model instance to start from
   @param path [list] Path segments to traverse
   @return [t] Value at the end of the path
   
   Example:
   (ref-in blog :comments 0 :approved-account)
   Expands to: (ref (nth 0 (ref blog :comments)) :approved-account)
   "
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
  "Display all column data from the model instance.
   
   Prints each column key and value to standard output.
   
   @param model [<base-model>] Model instance
   "
  (maphash #'(lambda (key value)
               (format t "~A:~A~%" key value))
           (slot-value model 'data)))

(defun show-model-columns (model)
  "Display all column definitions from the model instance.
   
   Prints each column specification to standard output.
   
   @param model [<base-model>] Model instance
   "
  (format t "show-model-columns: model: ~A~%" model)
  (loop for col in (slot-value model 'columns)
        do (format t "show-model-columns: column: ~A~%" col)))


(defun model->tbl (sym)
  "Convert model name to table name.
   
   Converts model class name symbols to database table names by removing
   angle brackets and converting kebab-case to snake_case.
   
   @param sym [symbol] Model class name (e.g., <todo> or <todo-history>)
   @return [string] Table name (e.g., \"TODO\" or \"TODO_HISTORY\")
   
   Examples:
   <todo> -> \"TODO\"
   <todo-history> -> \"TODO_HISTORY\"
   "
  (kebab->snake
   (ppcre:regex-replace-all
      "\>"
      (ppcre:regex-replace-all
         "\<"
         (format NIL "~S" sym)
         "")
      "")))


(defparameter *table-information* (make-hash-table)
  "Hash table storing metadata for all model classes.
   
   Key: model class name (symbol)
   Value: plist containing:
     :table-name [string] - Database table name
     :columns [list] - List of column specifications
     :columns-fn [function] - Function to fetch columns from database
     :columns-plist [plist] - Property list of columns by name
     :columns-plist-fn [function] - Function to fetch columns plist from database
     :relations [hash-table] - Hash table of relations (key: alias, value: relation info)
     :version-column [keyword] - Column name for optimistic locking, or NIL")


(defun initialize-table-information ()
  "Initialize table metadata for all registered models.
   
   Fetches column information from the database for each registered model
   and validates configuration (e.g., version column type). Also converts
   string model references in relations to symbols.
   
   Must be called after all models are defined and database connection is available.
   
   @condition error Signaled when version column is not of type :integer
   "
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
  "Display all table metadata for debugging purposes.
   
   Prints key-value pairs from *table-information* to standard output.
   "
  (loop for key being each hash-key of *table-information*
          using (hash-value value)
        do (format t "key:~A, value:~A" key value)))

(defun get-columns (model-name)
  "Get column specifications for a model.
   
   @param model-name [symbol] Model class name
   @return [list] List of column specifications
   "
  (getf (gethash model-name clails/model/base-model::*table-information*)
        :columns))

(defun get-columns-plist (model-name)
  "Get column specifications as plist for a model.
   
   @param model-name [symbol] Model class name
   @return [plist] Property list of column specifications by name
   "
  (getf (gethash model-name clails/model/base-model::*table-information*)
        :columns-plist))


(defun validate-relation (val)
  "Validate relation specification and return missing mandatory keys.
   
   Checks that the relation specification is properly formatted and contains
   all required keys for the relation type.
   
   @param val [list] Relation specification (type model-name &rest options)
   @return [list] List of missing or invalid keys, or empty list if valid
   
   Required keys for :has-many: :as, :foreign-key
   Required keys for :belongs-to: :column, :key
   "
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
  "Define a model class representing a database table.
   
   Creates a model class with automatic column introspection from the database
   and support for relations (has-many, belongs-to) and optimistic locking.
   
   @param class-name [symbol] Name for the model class (e.g., <todo>)
   @param superclass [list] List of superclasses (typically (<base-model>))
   @param options [plist] Model options
   
   Options:
   - :table [string] - Database table name (default: derived from class-name)
   - :relations [list] - List of relation specifications
   - :version [keyword] - Column name for optimistic locking version
   
   Relation specifications:
   - (:has-many \"<model-name>\" :as :alias :foreign-key :key-name)
   - (:belongs-to \"<model-name>\" :column :alias :key :key-name)
   
   @condition error Signaled when relation specifications are invalid
   
   Example:
   (defmodel <blog> (<base-model>)
     (:table \"blog\"
      :relations ((:belongs-to \"<account>\" :column :account :key :account-id))
      :version :version))
   "
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
  (:documentation "Fetch column definitions and types from database.
   
   Implementation must be provided for each database type.
   Returns list of column specifications.
   
   @param database-type [<database-type>] Database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @return [list] List of column specifications
   "))

(defgeneric fetch-columns-and-types-plist-impl (database-type connection table)
  (:documentation "Fetch column definitions and types as plist from database.
   
   Implementation must be provided for each database type.
   Returns property list of column specifications indexed by column name.
   
   @param database-type [<database-type>] Database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @return [plist] Property list of column specifications
   
   Example format:
   (:id (:name :id
         :access \"ID\"
         :type :integer
         :db-cl-fn #'identity
         :cl-db-fn #'identity)
    :created-at (:name :created-at
                 :access \"CREATED_AT\"
                 :type :datetime
                 :db-cl-fn #'db-datetime->cl-datetime
                 :cl-db-fn #'cl-datetime->db-datetime)
    ...)
   "))
