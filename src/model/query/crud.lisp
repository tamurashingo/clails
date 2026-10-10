(in-package #:cl-user)
(defpackage #:clails/model/query/crud
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:clails/condition
                #:optimistic-lock-error)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:validate
                #:ref
                #:has-error-p
                #:has-dirty-p
                #:frozen-p
                #:clear-error
                #:clear-dirty-flag)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/model/query
                #:<query>
                #:<query-placeholder>
                #:ensure-initialized
                #:generate-query
                #:relation
                #:through)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab
                #:plist-exists)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:import-from #:cl-batis
                #:<batis-sql>
                #:gen-sql-and-params)
  (:import-from #:dbi-cp
                #:fetch-all
                #:execute
                #:prepare)
  (:export #:execute-query
           #:save
           #:get-last-id-impl
           #:make-record
           #:destroy
           #:insert1))
(in-package #:clails/model/query/crud)

;;;; ----------------------------------------
;;;; This file executes queries built by clails/model/query (SELECT via
;;;; execute-query) and performs CRUD writes (save / make-record / destroy
;;;; and their INSERT / UPDATE / DELETE machinery) against the database
;;;; connection.

;;;; ========================================
;;;; export method

(defmethod execute-query ((query <query>) named-values &key connection (convert-types t))
  "Execute the query and return model instances.

   Generates SQL from the query specification, executes it against the database,
   and builds model instances from the results, including nested relations.

   @param query [<query>] Query specification
   @param named-values [plist] Named parameter values for the query
   @param connection [dbi:<dbi-connection>] Optional database connection to use
   @param convert-types [boolean] Whether to perform automatic type conversion (default: t)
   @return [list] List of model instances with populated relations
   "
  (multiple-value-bind (sql params)
      (generate-query query named-values :convert-types convert-types)
    (when (log-level-enabled-p :debug :sql)
      (log.sql (format nil "sql: ~S" query))
      (log.sql (format nil "params: ~S" params)))
    (let* ((connection (get-connection))
           (result (dbi-cp:fetch-all
                    (dbi-cp:execute
                     (dbi-cp:prepare connection sql)
                     params))))
      (build-model-instances query result))))

(defmethod execute-query ((placeholder <query-placeholder>) named-values &key connection (convert-types t))
  "Execute the query via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param named-values [plist] Named parameter values for the query
   @param connection [dbi:<dbi-connection>] Optional database connection to use
   @param convert-types [boolean] Whether to perform automatic type conversion (default: t)
   @return [list] List of model instances with populated relations
   @condition error Signaled when placeholder has not been initialized
   "
  (execute-query (ensure-initialized placeholder) named-values
                 :connection connection
                 :convert-types convert-types))


(defmethod save ((inst <base-model>) &key connection)
  "Save model instance to the database.

   Validates the instance, then either updates (if ID exists and dirty)
   or inserts (if no ID). Clears dirty flags on success.

   @param inst [<base-model>] Model instance to save
   @param connection [dbi:<dbi-connection>] Optional database connection to use
   @return [<base-model>] The saved model instance
   @return [nil] NIL if validation failed or instance is frozen
   @condition optimistic-lock-error Signaled when update affects 0 rows (version conflict)
   "
  (unless (frozen-p inst)
    (clear-error inst)
    (validate inst)
    (unless (has-error-p inst)
      (prog1
          (if (ref inst :id)
              (if (has-dirty-p inst)
                  (let ((rows-updated (update1 inst :connection connection)))
                    (when (= rows-updated 0)
                      (error 'optimistic-lock-error))
                    inst)
                  inst)
              (insert1 inst :connection connection))
        (clear-dirty-flag inst)))))


(defgeneric get-last-id-impl (database-type connection)
  (:documentation "Get the last inserted ID for the specified database type.

   Implementation must be provided for each database type.

   @param database-type [<database-type>] Database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   @return [integer] Last inserted ID
   "))


(defun make-record (model-name &rest values)
  "Create a new model instance with the given attribute values.

   @param model-name [symbol] Model class name (e.g., <todo>)
   @param values [plist] Property list of attribute key-value pairs
   @return [<base-model>] New model instance

   Example:
   (let ((inst (make-record '<todo> :title \"create new project\" :done nil)))
     (save inst))
   "
  (let ((inst (make-instance model-name)))
    (loop for (key value) on values by #'cddr
          do (setf (ref inst key) value))
    inst))


(defgeneric destroy (instance &key cascade)
  (:documentation "Delete record from the database.

   @param instance [<base-model>] Model instance to delete
   @param cascade [boolean] Whether to cascade delete to related records
   @return [integer] Number of rows deleted
   "))

(defmethod destroy ((inst <base-model>) &key cascade)
  "Delete a single model instance from the database.

   If cascade is true, also deletes related :has-many records.
   Sets the instance to frozen after deletion.

   @param inst [<base-model>] Model instance to delete
   @param cascade [boolean] If T, cascade delete to :has-many relations
   @return [integer] Number of rows deleted (0 if frozen, 1 otherwise)
   "
  (if (frozen-p inst)
      0
      (progn
        (when cascade
          (let ((relations (getf (gethash (class-name (class-of inst)) clails/model/base-model::*table-information*)
                                 :relations)))
            (maphash #'(lambda (k v)
                         ;; only :has-many relation is supported for cascade delete
                         (when (eq (getf v :type) :has-many)
                           (destroy (ref inst k) :cascade T)))
                     relations)))
        (let* ((table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
               (sql (format NIL "DELETE FROM ~A WHERE id = ?" table-name))
               (params (list (ref inst :id))))
          (when (log-level-enabled-p :debug :sql)
            (log.sql (format nil "sql: ~S" sql))
            (log.sql (format nil "params: ~S" params)))
          (let ((connection (get-connection)))
            (prog1
              (progn
                (dbi-cp:execute
                 (dbi-cp:prepare connection sql)
                 params)
                (dbi-cp:row-count connection))
              (clear-dirty-flag inst)
              (setf (slot-value inst 'clails/model/base-model::frozen-p) T)))))))

(defmethod destroy ((insts list) &key cascade)
  "Delete multiple model instances from the database.

   If cascade is true, also deletes related :has-many records for each instance.
   Sets all instances to frozen after deletion.

   @param insts [list] List of <base-model> instances to delete
   @param cascade [boolean] If T, cascade delete to :has-many relations
   @return [integer] Number of rows deleted
   "
  (if (null insts)
      0
      (progn
        ;; accept only <base-model> instances
        (dolist (i insts)
          (check-type i <base-model>))

        (when cascade
          (dolist (i insts)
            (let ((relations (getf (gethash (class-name (class-of i)) clails/model/base-model::*table-information*)
                                   :relations)))
              (maphash #'(lambda (k v)
                           ;; only :has-many relation is supported for cascade delete
                           (when (eq (getf v :type) :has-many)
                             (destroy (ref i k) :cascade T)))
                       relations))))


        (let* ((table-name (kebab->snake (slot-value (first insts) 'clails/model/base-model::table-name)))
               (ids (loop for i in insts
                      when (not (frozen-p i))
                        collect (ref i :id)))
               (sql (format NIL "DELETE FROM ~A WHERE id IN (~{?~*~^, ~})" table-name ids)))
          (when (log-level-enabled-p :debug :sql)
            (log.sql (format nil "sql: ~S" sql))
            (log.sql (format nil "ids: ~S" ids)))
          (prog1
              (let ((connection (get-connection)))
                (dbi-cp:execute
                  (dbi-cp:prepare connection sql)
                  ids)
                (dbi-cp:row-count connection))
            (dolist (i insts)
              (unless (frozen-p i)
                (clear-dirty-flag i)
                (setf (slot-value i 'clails/model/base-model::frozen-p) T))))))))


;;; ----------------------------------------
;;; save

(defun fetch-columns (inst &key insert update)
  "Fetch column names to be included in SQL statement.

   For INSERT operations, only returns columns that have been explicitly set
   (marked with dirty-flag). For UPDATE operations, returns columns that have
   been modified plus :updated-at.

   @param inst [<base-model>] Model instance
   @param insert [boolean] If true, fetches columns for INSERT statement
   @param update [boolean] If true, fetches columns for UPDATE statement
   @return [list] List of column name strings
   "
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        as dirty-flag-hash = (slot-value inst 'clails/model/base-model::dirty-flag)
        when (or (and insert
                      (not (eq (getf column :name) :id))
                      (gethash (getf column :name) dirty-flag-hash))
                 ;; update column if dirty
                 (and update
                      (not (eq (getf column :name) :id))
                      (not (eq (getf column :name) :created-at))
                      (or (eq (getf column :name) :updated-at)
                          (gethash (getf column :name) dirty-flag-hash)))
                 (and (not insert)
                      (not update)))
        collect (string (getf column :name))))


(defun insert1 (inst &key connection)
  "Insert a model instance into the database.

   Only inserts columns that have been explicitly set (marked with dirty-flag),
   allowing database default values to be applied to unset columns.
   Automatically sets created-at and updated-at timestamps.

   @param inst [<base-model>] Model instance to insert
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [<base-model>] The inserted instance with id, created-at, updated-at, and version set
   "
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name clails/model/base-model::*table-information*))
         (version-column (getf table-info :version-column))
         (current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (fetch-columns inst :insert T))
         (params nil))

    ;; Build params from columns that have dirty flag set
    (loop for colstr in columns
          as colkey = (intern colstr :KEYWORD)
          do (push (ref inst colkey) params))
    (setf params (nreverse params))

    ;; Add created-at and updated-at to columns and params
    (setf columns (append columns (list "CREATED-AT" "UPDATED-AT")))
    (setf params (append params (list current-datetime current-datetime)))

    ;; Add version column if specified
    (when version-column
      (setf columns (append columns (list (string version-column))))
      (setf params (append params (list 1))))

    ;; Build SQL with all columns including created-at and updated-at
    (let ((sql (format NIL "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{?~*~^, ~})"
                       table-name
                       (mapcar #'kebab->snake columns)
                       columns)))

      ;; convert parameter values using cl-db-fn
      (setf params (loop for colstr in columns
                         as colkey = (intern colstr :KEYWORD)
                         as column-info = (loop for col in (slot-value inst 'clails/model/base-model::columns)
                                                when (eq (getf col :name) colkey)
                                                return col)
                         for i from 0
                         as value = (nth i params)
                         collect (if column-info
                                     (funcall (getf column-info :cl-db-fn) value)
                                     value)))

      (when (log-level-enabled-p :debug :sql)
        (log.sql (format nil "sql: ~S" sql))
        (log.sql (format nil "params: ~S" params)))

      (let ((body #'(lambda (connection)
                      (dbi-cp:execute
                       (dbi-cp:prepare connection sql)
                       params)
                      (let ((last-id (get-last-id connection)))
                        (setf (ref inst :id) last-id)
                        (setf (ref inst :created-at) current-datetime)
                        (setf (ref inst :updated-at) current-datetime)
                        (when version-column
                          (setf (ref inst version-column) 1)))
                      inst)))

        (if connection
            (funcall body connection)
            (let ((connection (get-connection)))
              (funcall body connection)))))))


(defun get-last-id (connection)
  "Get the last inserted ID from the database.

   @param connection [dbi:<dbi-connection>] Database connection
   @return [integer] Last inserted ID
   "
  (get-last-id-impl *database-type* connection))


(defun update1 (inst &key connection)
  "Update a model instance in the database.

   Only updates columns marked as dirty. Automatically updates :updated-at
   and version column (if configured). Uses optimistic locking when version
   column is specified.

   @param inst [<base-model>] Model instance to update
   @param connection [connection] Optional database connection
   @return [integer] Number of rows updated (0 if version mismatch, 1 otherwise)
   "
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name clails/model/base-model::*table-information*))
         (version-column (getf table-info :version-column))
         (current-version (when version-column (ref inst version-column)))
         (current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns nil)
         (where-clause "id = ?")
         (where-params (list (ref inst :id)))
         (params nil)
         (sql nil))

    (when version-column
      ;; the version-column will be automatically added to the `update` columns when flag sets to dirty
      (setf (gethash version-column (slot-value inst 'clails/model/base-model::dirty-flag))
            T)
      (setf where-clause (format nil "~A AND ~A = ?" where-clause (kebab->snake version-column)))
      (appendf where-params (list current-version)))
    (setf columns (fetch-columns inst :update T))

    (setf params (alexandria:alist-plist (loop for colstr in columns
                                               as colkey = (intern colstr :KEYWORD)
                                               collect (cons colkey (ref inst colkey)))))

    ;; set updated-at and version
    (setf (getf params :updated-at) current-datetime)
    (when version-column
      (setf (getf params version-column) (1+ current-version)))

    (setf sql (format NIL "UPDATE ~A SET ~{~A = ?~^, ~} WHERE ~A"
                      table-name
                      (mapcar #'kebab->snake columns)
                      where-clause))

    ;; convert parameter plist -> values
    (setf params (convert-cl-db-values params inst))

    ;; append where-params
    (setf params (append params where-params))

    (when (log-level-enabled-p :debug :sql)
      (log.sql (format nil "sql: ~S" sql))
      (log.sql (format nil "params: ~S" params)))

    (let ((body #'(lambda (connection)
                    (dbi-cp:execute (dbi-cp:prepare connection sql) params)
                    (let ((rows (dbi-cp:row-count connection)))
                      (when (> rows 0)
                        (setf (ref inst :updated-at) current-datetime)
                        (when version-column
                          (setf (ref inst version-column) (1+ current-version))))
                      rows))))
      (if connection
          (funcall body connection)
          (let ((connection (get-connection)))
            (funcall body connection))))))


(defun convert-cl-db-values (params inst)
  "Convert Common Lisp values to database values.

   Applies cl-db-fn conversion function for each column.

   @param params [plist] Parameter values by column name
   @param inst [<base-model>] Model instance
   @return [list] List of converted values
   "
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        when (plist-exists params (getf column :name))
        collect (let ((name (getf column :name))
                      (fn (getf column :cl-db-fn)))
                  (funcall fn (getf params name)))))


;;; ----------------------------------------
;;; query -> model

(defun make-record-from (model-name &rest db-values)
  "Create model instance from database row values.

   Applies db-cl-fn conversion for each column and clears dirty flags.

   @param model-name [symbol] Model class name
   @param db-values [plist] Database values by column keyword
   @return [<base-model>] Model instance with values set and dirty flags cleared
   "
  (let ((inst (make-instance model-name))
        (columns-plist (clails/model/base-model::get-columns-plist model-name)))
    (loop for (key db-value) on db-values by #'cddr
          do (let ((fn (getf (getf columns-plist key) :DB-CL-FN)))
               (setf (ref inst key)
                     (funcall fn db-value))))
    (clear-dirty-flag inst)
    inst))


(defun split-db-column-name (keyword-name)
  "Split database column keyword into alias and column name.

   Splits a keyword like :|TABLE.COLUMN| into two keywords, :TABLE and :COLUMN.
   The keyword comes from the database driver.

   @param keyword-name [keyword] Database column keyword in format :|ALIAS.COLUMN|
   @return [keyword] Table alias keyword
   @return [keyword] Column name keyword
   @condition error Signaled when keyword is not in expected format
   "
  (let* ((str (string keyword-name))
         (dot-pos (position #\. str)))
    (if dot-pos
        (values (intern (snake->kebab (string-upcase (subseq str 0 dot-pos))) :keyword)
                (intern (snake->kebab (string-upcase (subseq str (1+ dot-pos)))) :keyword))
        (error "Invalid column name from DB: ~A. Expected 'ALIAS.COLUMN' format." keyword-name))))

(defun group-row-data-by-alias (row-plist)
  "Group flat database result row by table alias.

   Groups a flat plist of results from the DB into a hash table where keys are
   table aliases and values are plists of column data for that alias.

   @param row-plist [plist] Flat result row from database
   @return [hash-table] Hash table with aliases as keys and column data plists as values
   "
  (let ((grouped (make-hash-table)))
    ;; Group data into alists first to handle multiple columns for the same alias
    (loop for (key val) on row-plist by #'cddr
          do (multiple-value-bind (alias col) (split-db-column-name key)
               (push (cons col val) (gethash alias grouped))))
    ;; Convert the alists to plists for easier use with getf
    (maphash #'(lambda (alias alist)
                 (setf (gethash alias grouped) (alexandria:alist-plist (nreverse alist))))
             grouped)
    grouped))

(defun hydrate-instance (model-class data-plist record-cache)
  "Create or retrieve cached model instance from data.

   Creates a model instance from a plist of data, or retrieves it from cache if it
   has already been created for the same ID.

   @param model-class [symbol] Model class name
   @param data-plist [plist] Column data for the instance
   @param record-cache [hash-table] Cache of instances by model class and ID
   @return [<base-model>] Model instance
   @return [nil] NIL if data-plist has no :ID
   "
  (let ((id (getf data-plist :ID)))
    (when id
      (let* ((model-cache (or (gethash model-class record-cache)
                              (setf (gethash model-class record-cache) (make-hash-table :test #'eql))))
             (instance (gethash id model-cache)))
        (unless instance
          (setf instance (apply #'make-record-from model-class data-plist))
          (setf (gethash id model-cache) instance))
        instance))))

(defun hydrate-instances-for-row (grouped-data alias->model record-cache)
  "Create or retrieve model instances for all aliases in a result row.

   For a single result row (grouped by alias), creates or retrieves all
   corresponding model instances.

   @param grouped-data [hash-table] Row data grouped by alias
   @param alias->model [hash-table] Mapping from aliases to model classes
   @param record-cache [hash-table] Cache of instances
   @return [hash-table] Hash table mapping aliases to model instances
   "
  (let ((hydrated-row-instances (make-hash-table :test #'eq)))
    (maphash
     #'(lambda (alias data-plist)
         (let ((model-class (gethash alias alias->model)))
           (when model-class
             (let ((instance (hydrate-instance model-class data-plist record-cache)))
               (when instance
                 (setf (gethash alias hydrated-row-instances) instance))))))
     grouped-data)
    hydrated-row-instances))

(defmethod link-row-instances ((query <query>) hydrated-row-instances)
  "Links the model instances for a single row together based on the query's
   join definitions and the model's relation metadata.
   ex: #{ :blog =>    <blog {:id 1, :account-id: 1001, :account => (unbound)>
          :account => <account {:id 1001, :username \"user1\", :blogs => (unbound)>
       =>
       #{ :blog =>    <blog {:id 1, :account-id: 1001, :account => <instnact of account>
          :account => <account {:id 1001, :username \"user1\", :blogs => (<instance of blog>)> }
"
  (let ((alias->model (slot-value query 'clails/model/query::alias->model))
        (base-alias (slot-value query 'clails/model/query::alias)))
    (loop for join-obj in (slot-value query 'clails/model/query::joins)
          do (let* ((target-alias (relation join-obj))
                    (source-alias (or (through join-obj) base-alias))
                    (source-inst (gethash source-alias hydrated-row-instances))
                    (target-inst (gethash target-alias hydrated-row-instances))
                    (source-model (gethash source-alias alias->model)))
               (when (and source-inst target-inst source-model)
                 (let* ((relations (getf (gethash source-model clails/model/base-model::*table-information*) :relations))
                        (rel-info (gethash target-alias relations)))
                   (when rel-info
                     (case (getf rel-info :type)
                       (:belongs-to
                        (setf (ref source-inst (getf rel-info :column)) target-inst))
                       (:has-many
                        ;; pushnew avoids duplicates if the same child is joined multiple times
                        (pushnew target-inst (ref source-inst (getf rel-info :as)) :test #'eq))))))))))

(defun finalize-has-many-relations (instances)
  "Initialize unbound :has-many relation slots to NIL.

   Ensures that for a list of instances, any :has-many relation slots that were not
   populated during result processing are initialized to NIL instead of being unbound.

   @param instances [list] List of model instances
   @return [list] The same list of instances
   "
  (loop for inst in instances
        do (let* ((model (class-name (class-of inst)))
                  (relations (getf (gethash model clails/model/base-model::*table-information*) :relations)))
             (when relations
               (maphash #'(lambda (alias rel-info)
                            (when (eq (getf rel-info :type) :has-many)
                              ;; Check if the slot is bound. If not, set it to nil.
                              (multiple-value-bind (val foundp) (gethash alias (slot-value inst 'clails/model/base-model::data))
                                (declare (ignore val))
                                (unless foundp
                                  (setf (ref inst alias) nil)))))
                        relations))))
  instances)

(defun build-model-instances (query result)
  "Process database result set into graph of nested model instances.

   Processes a raw database result set for a given <query> object and constructs
   a graph of nested model instances with relations properly linked.

   @param query [<query>] Query specification
   @param result [list] Raw database result set (list of plists)
   @return [list] List of unique main model instances with relations populated
   "
  (let* ((record-cache (make-hash-table :test #'eq)) ; Cache for all instances across all rows {model-class -> {id -> instance}}
         (main-instances (make-hash-table :test #'eql))) ; Cache for top-level instances {id -> instance}

    ;; Process each row from the database result
    (loop for row-plist in result
          do (let* ((grouped-data (group-row-data-by-alias row-plist))
                    (hydrated-row-instances (hydrate-instances-for-row grouped-data (slot-value query 'clails/model/query::alias->model) record-cache)))

               ;; Link the instances created/retrieved for this specific row
               (link-row-instances query hydrated-row-instances)

               ;; Identify the main instance for this row and add it to our final set
               (let ((main-inst (gethash (slot-value query 'clails/model/query::alias) hydrated-row-instances)))
                 (when main-inst
                   (setf (gethash (ref main-inst :ID) main-instances) main-inst)))))

    ;; Collect the unique main instances into a list
    (let ((final-results (loop for inst being the hash-value of main-instances collect inst)))
      ;; Post-process to ensure has-many slots are initialized
      (finalize-has-many-relations final-results))))


;;;; ----------------------------------------
;;;; Native Query Support (using cl-batis)

(defun execute-select-query (connection sql-string param-values)
  "Execute SELECT query and return results.

   @param connection [<connection>] Database connection
   @param sql-string [string] SQL query string
   @param param-values [list] Parameter values
   @return [list of plist] Query results (list of plists)
   "
  (when (log-level-enabled-p :debug :sql)
    (log.sql (format nil "sql: ~A" sql-string))
    (log.sql (format nil "params: ~S" param-values)))

  (dbi-cp:fetch-all
   (dbi-cp:execute
    (dbi-cp:prepare connection sql-string)
    param-values)))

(defun execute-update-query (connection sql-string param-values)
  "Execute UPDATE/INSERT/DELETE query and return affected row count.

   @param connection [<connection>] Database connection
   @param sql-string [string] SQL query string
   @param param-values [list] Parameter values
   @return [integer] Number of affected rows
   "
  (when (log-level-enabled-p :debug :sql)
    (log.sql (format nil "sql: ~A" sql-string))
    (log.sql (format nil "params: ~S" param-values)))

  (dbi-cp:execute
   (dbi-cp:prepare connection sql-string)
   param-values)
  (dbi-cp:row-count connection))

(defmethod execute-query ((sql <batis-sql>) named-values &key connection)
  "Execute SQL defined by cl-batis and return results.

   Uses gen-sql-and-params to convert the SQL definition to
   prepared statement SQL and parameter list, then executes it.
   SELECT queries return result rows, UPDATE queries return affected row count.

   @param sql [<batis-sql>] SQL definition created by select/defsql or update/defsql
   @param named-values [plist] Parameter values as property list
   @param connection [<connection>] Database connection (optional)
   @return [list of plist] Query results for SELECT queries
   @return [integer] Number of affected rows for UPDATE queries
   @condition database-error SQL execution error
   "
  (let ((conn (or connection (get-connection))))
    (multiple-value-bind (sql-string param-values)
        (gen-sql-and-params sql named-values)

      (let ((sql-type (slot-value sql 'batis.macro::sql-type)))
        (ecase sql-type
          (:select (execute-select-query conn sql-string param-values))
          (:update (execute-update-query conn sql-string param-values)))))))
