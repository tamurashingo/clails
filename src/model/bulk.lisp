(in-package #:cl-user)
(defpackage #:clails/model/bulk
  (:use #:cl)
  (:import-from #:clails/model/query
                #:insert1
                #:generate-query
                #:generate-values
                #:make-record
                #:<query>)
  (:import-from #:clails/model/query
                #:update1
                #:destroy)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/model/transaction
                #:with-transaction-using-connection)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:ref)
  (:import-from #:clails/environment
                #:*database-type*
                #:<database-type-postgresql>
                #:<database-type-mysql>
                #:<database-type-sqlite3>)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab)
  (:import-from #:cl-batis
                #:<batis-sql>
                #:gen-sql-and-params)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:alexandria
                #:flatten)
  (:export #:with-query-cursor
           #:show-query-sql
           #:insert-all
           #:insert-bulk
           #:update-all
           #:update-bulk
           #:delete-all
           #:delete-bulk
           #:type-mismatch-error))
(in-package #:clails/model/bulk)

;;;; ----------------------------------------
;;;; Error Definitions
;;;; ----------------------------------------

(define-condition type-mismatch-error (error)
  ((expected-class :initarg :expected-class
                   :reader type-mismatch-error-expected-class)
   (actual-instances :initarg :actual-instances
                     :reader type-mismatch-error-actual-instances))
  (:report (lambda (condition stream)
             (format stream "Type mismatch: expected ~A, but found instances of different types: ~A"
                     (type-mismatch-error-expected-class condition)
                     (mapcar (lambda (inst) (class-name (class-of inst)))
                             (type-mismatch-error-actual-instances condition))))))

;;;; ----------------------------------------
;;;; Main API
;;;; ----------------------------------------

(defmacro with-query-cursor ((variable-name query-or-sql parameter-plist
                              &key (batch-size 1000) connection)
                             &body body)
  "Execute SELECT query in batches and process results efficiently.

   This macro processes large SELECT results in batch units.
   Database-specific optimizations are automatically applied:
   - PostgreSQL: Server-side cursor
   - MySQL: Streaming result set
   - SQLite3: LIMIT/OFFSET pagination

   @param variable-name [symbol] Variable name to bind batch records to
   @param query-or-sql [<query>|<batis-sql>] Query specification
   @param parameter-plist [plist] Query parameters
   @param batch-size [integer] Number of rows per batch (default: 1000)
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Optional database connection proxy
   @return [value] Returns the value of the last form in body

   Example:
   (with-query-cursor (user-rows
                       (query <user> :as :u :order-by '((:u :id)))
                       nil
                       :batch-size 500)
     (dolist (row user-rows)
       (format t \"User: ~A~%\" (getf row :name))))
   "
  (let ((conn-var (gensym "CONN"))
        (sql-var (gensym "SQL"))
        (params-var (gensym "PARAMS"))
        (query-type-var (gensym "QUERY-TYPE"))
        (query-obj-var (gensym "QUERY-OBJ")))
    `(let* ((,conn-var (or ,connection (get-connection)))
            (,query-obj-var ,query-or-sql)
            (,query-type-var (query-type-of ,query-obj-var))
            ,sql-var
            ,params-var)

       ;; Generate SQL and params based on query type
       (ecase ,query-type-var
         (:query
          ;; <query> object: use generate-query from clails/model/query
          (multiple-value-bind (sql params)
              (generate-query ,query-obj-var ,parameter-plist)
            (setf ,sql-var sql
                  ,params-var params)))

         (:batis-sql
          ;; <batis-sql> object: use gen-sql-and-params from cl-batis
          (multiple-value-bind (sql params)
              (gen-sql-and-params ,query-obj-var ,parameter-plist)
            (setf ,sql-var sql
                  ,params-var params))))

       ;; Execute database-specific batch processing
       ;; All query types return hash-table list
       (execute-batch-query *database-type*
                           ,conn-var
                           ,sql-var
                           ,params-var
                           ,batch-size
                           ;; Callback function: variable-name is bound to hash-table list
                           (lambda (,variable-name)
                             ,@body)))))

;;;; ----------------------------------------
;;;; Helper Functions
;;;; ----------------------------------------

(defun query-type-of (query-or-sql)
  "Determine the type of query object.

   Uses class-of to identify the object type.

   @param query-or-sql [<query>|<batis-sql>] Query specification
   @return [keyword] :query or :batis-sql
   @condition error Signaled for unsupported types
   "
  (cond
    ((typep query-or-sql 'clails/model/query:<query>) :query)
    ((typep query-or-sql 'cl-batis:<batis-sql>) :batis-sql)
    (t (error "Unsupported query type: ~A" (class-name (class-of query-or-sql))))))


(defun show-query-sql (query-or-sql parameter-plist &key (output *standard-output*))
  "Display SQL with parameters for debugging purposes.

   Generates SQL and embeds parameters for easy debugging.

   @param query-or-sql [<query>|<batis-sql>] Query specification
   @param parameter-plist [plist] Parameters
   @param output [stream] Output destination (default: *standard-output*)
   @return [string] Generated SQL string with parameters embedded

   Example:
   (show-query-sql (query <user> :as :u :where (:= (:u :age) :age))
                   '(:age 30))
   ;; => \"SELECT ... WHERE U.AGE = 30\"
   "
  (let ((query-type (query-type-of query-or-sql))
        sql params)

    ;; Generate SQL
    (ecase query-type
      (:query
       (multiple-value-bind (s p)
           (generate-query query-or-sql parameter-plist)
         (setf sql s
               params p)))
      (:batis-sql
       (multiple-value-bind (s p)
           (gen-sql-and-params query-or-sql parameter-plist)
         (setf sql s
               params p))))

    ;; Embed parameters into SQL
    (let ((formatted-sql (format-sql-with-params sql params)))
      (format output "~A~%" formatted-sql)
      formatted-sql)))


(defun format-sql-with-params (sql params)
  "Embed parameters into SQL placeholders.

   @param sql [string] SQL string (placeholders: ?)
   @param params [list] Parameter list
   @return [string] SQL string with parameters embedded
   "
  (let ((result sql)
        (param-index 0))
    (loop while (search "?" result)
          do (let* ((pos (search "?" result))
                    (param (nth param-index params))
                    (param-str (format-sql-value param)))
               (setf result (concatenate 'string
                                        (subseq result 0 pos)
                                        param-str
                                        (subseq result (1+ pos))))
               (incf param-index)))
    result))


(defun format-sql-value (value)
  "Convert value to SQL string representation.

   Strings are quoted with single quotes, numbers as-is,
   NIL as NULL, T as TRUE.

   @param value [t] Any value
   @return [string] SQL representation string
   "
  (cond
    ((null value) "NULL")
    ((eq value t) "TRUE")
    ((stringp value) (format nil "'~A'" (escape-sql-string value)))
    ((numberp value) (format nil "~A" value))
    ((keywordp value) (format nil "'~A'" (string-downcase (symbol-name value))))
    (t (format nil "'~A'" value))))


(defun escape-sql-string (str)
  "Escape single quotes in SQL strings.

   @param str [string] String to escape
   @return [string] Escaped string
   "
  (cl-ppcre:regex-replace-all "'" str "''"))

;;;; ----------------------------------------
;;;; Database-specific implementations
;;;; ----------------------------------------

(defgeneric execute-batch-query (database-type connection sql params batch-size callback)
  (:documentation "Execute batch query using database-specific optimization.

   Implementation must be provided for each database type.

   @param database-type [<database-type>] Database type instance
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Database connection proxy
   @param sql [string] SQL to execute
   @param params [list] SQL parameters
   @param batch-size [integer] Batch size
   @param callback [function] Function to process each batch
   "))


(defmethod execute-batch-query ((db-type <database-type-postgresql>)
                                 connection
                                 sql
                                 params
                                 batch-size
                                 callback)
  "Execute batch query using PostgreSQL server-side cursor.

   Cursors work only within transactions.
   If no transaction is active, automatically starts one.

   Uses cl-dbi's make-cursor, execute, fetch to operate cursors.

   @param db-type [<database-type-postgresql>] Database type
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Database connection proxy
   @param sql [string] SQL to execute
   @param params [list] SQL parameters
   @param batch-size [integer] Batch size
   @param callback [function] Function to process each batch
   "
  (when (log-level-enabled-p :debug :sql)
    (log.sql (format nil "PostgreSQL CURSOR: ~A" sql))
    (log.sql (format nil "PARAMS: ~A" params)))

  (let ((dbi-conn (dbi-cp.proxy:dbi-connection connection)))
    (labels ((fetch-batches (cursor)
               "Fetch data in batch units from cursor and process.

                @param cursor [dbi-cursor] Cursor object
                "
               (loop
                 (let ((batch (loop repeat batch-size
                                   for row = (dbi:fetch cursor :format :plist)
                                   while row
                                   collect row)))
                   (when (null batch)
                     (return))
                   (funcall callback batch))))

             (process-with-cursor ()
               "Create cursor and execute processing.
               "
               (let ((cursor nil))
                 (unwind-protect
                      (progn
                        ;; Create and execute cursor
                        ;; cl-dbi's make-cursor returns dbi-cursor object
                        (setf cursor (dbi:make-cursor dbi-conn sql))

                        ;; execute-using-connection executes DECLARE CURSOR for PostgreSQL
                        (dbi:execute cursor params)

                        ;; Fetch and process in batch units
                        (fetch-batches cursor))

                   ;; Cleanup
                   (when cursor
                     ;; Execute CLOSE CURSOR
                     (ignore-errors (dbi:close-cursor cursor)))))))

      ;; Automatically start transaction if not started
      (if (dbi:in-transaction dbi-conn)
          ;; Already in transaction: execute as-is
          (process-with-cursor)
          ;; Start transaction and execute
          (with-transaction-using-connection connection
            (process-with-cursor))))))


(defmethod execute-batch-query ((db-type <database-type-mysql>)
                                 connection
                                 sql
                                 params
                                 batch-size
                                 callback)
  "Execute batch query using MySQL streaming result set.

   By specifying :store nil to prepare, streaming mode is enabled,
   and rows are fetched one by one with fetch, calling callback per batch size.

   Resources are reliably freed using unwind-protect on error.

   @param db-type [<database-type-mysql>] Database type
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Database connection proxy
   @param sql [string] SQL to execute
   @param params [list] SQL parameters
   @param batch-size [integer] Batch size
   @param callback [function] Function to process each batch
   "
  (when (log-level-enabled-p :debug :sql)
    (log.sql (format nil "MySQL STREAMING: ~A" sql))
    (log.sql (format nil "PARAMS: ~A" params)))

  (let* ((dbi-conn (dbi-cp.proxy:dbi-connection connection))
         (stmt (dbi:prepare dbi-conn sql :store nil)))  ;; Streaming mode
    (unwind-protect
         (progn
           (dbi:execute stmt params)

           (loop
             (let ((batch (loop for i from 1 to batch-size
                               for row = (dbi:fetch stmt :format :plist)
                               while row
                               collect row)))

               (when (null batch)
                 (return))

               (funcall callback batch))))

      ;; Cleanup
      (ignore-errors (dbi:free-query-resources stmt)))))


(defmethod execute-batch-query ((db-type <database-type-sqlite3>)
                                 connection
                                 sql
                                 params
                                 batch-size
                                 callback)
  "Execute batch query using SQLite3 LIMIT/OFFSET pagination.

   Appends LIMIT/OFFSET to SQL and fetches batch by batch.
   ORDER BY in SQL is strongly recommended.

   @param db-type [<database-type-sqlite3>] Database type
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Database connection proxy
   @param sql [string] SQL to execute (ORDER BY recommended)
   @param params [list] SQL parameters
   @param batch-size [integer] Batch size
   @param callback [function] Function to process each batch
   "
  (when (log-level-enabled-p :debug :sql)
    (log.sql (format nil "SQLite3 LIMIT/OFFSET: ~A" sql))
    (log.sql (format nil "PARAMS: ~A" params)))

  (loop for offset from 0 by batch-size
        do
           (let* ((paginated-sql (format nil "~A LIMIT ? OFFSET ?" sql))
                  (stmt (dbi:prepare connection paginated-sql))
                  (result (dbi:execute stmt (append params (list batch-size offset))))
                  (batch (loop for row = (dbi:fetch result :format :plist)
                              while row
                              collect row)))

             (when (null batch)
               (return))

             (funcall callback batch))))

;;;; ========================================
;;;; INSERT Operations
;;;; ========================================

;;; ----------------------------------------
;;; insert-all
;;; ----------------------------------------

(defun insert-all (list-of-model &key connection)
  "Insert model instances one by one and write back IDs.

   Each instance is inserted using insert1, so
   id, created-at, and updated-at are set on each instance after INSERT.

   @param list-of-model [list] List of <base-model> instances
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [list] List of model instances with IDs set
   @condition database-error When INSERT fails
   "
  (when (null list-of-model)
    (return-from insert-all nil))

  (unless (typep (first list-of-model) '<base-model>)
    (error "insert-all requires a list of model instances"))

  (dolist (model list-of-model)
    (insert1 model :connection connection))

  list-of-model)


;;; ----------------------------------------
;;; insert-bulk
;;; ----------------------------------------

(defun insert-bulk (model-class columns list &key (batch-size 100) (use-transaction t) (on-type-mismatch :error) connection)
  "Execute bulk INSERT and return the number of inserted rows.

   Accepts a list of plists or model instances and performs bulk INSERT.
   Does not write back IDs (for performance).

   Column information is automatically adjusted:
   - :id is removed if included
   - :created-at is added if not included
   - :updated-at is added if not included

   Timestamps are automatically set:
   - :created-at is set to current time if not set
   - :updated-at is set to current time if not set

   @param model-class [symbol] Model symbol to INSERT (e.g., '<user>)
   @param columns [list] List of columns to INSERT (list of keywords) (e.g., '(:name :age :address))
   @param list [list] List of plists or list of model instances
   @param batch-size [integer] Batch size (default: 100)
   @param use-transaction [boolean] Use transaction (default: t)
   @param on-type-mismatch [keyword] Behavior on type mismatch (default: :error)
                                     :error - Raise error when type mismatch occurs (default)
                                     :skip - Skip instances that don't match the model class
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [integer] Number of inserted records
   @condition database-error When INSERT fails
   @condition type-mismatch-error When on-type-mismatch is :error and type mismatch occurs
   "
  (when (null list)
    (return-from insert-bulk 0))

  ;; Validate and adjust column information
  (let ((adjusted-columns (validate-and-adjust-columns model-class columns)))

    ;; Determine if plist or model
    (let ((first-item (first list)))
      (cond
        ;; plist case
        ((and (listp first-item) (keywordp (first first-item)))
         (batch-insert-plist model-class adjusted-columns list batch-size use-transaction connection))

        ;; model case
        ((typep first-item '<base-model>)
         ;; Type check and filtering
         (let ((filtered-list (filter-models-by-class model-class list on-type-mismatch)))
           (when (null filtered-list)
             (return-from insert-bulk 0))
           (batch-insert-instances model-class adjusted-columns filtered-list batch-size use-transaction connection)))

        (t
         (error "insert-bulk requires a list of plists or model instances"))))))


;;; ----------------------------------------
;;; Column validation and adjustment
;;; ----------------------------------------

(defun validate-and-adjust-columns (model-class columns)
  "Validate and adjust column list.

   Removes :id if included, adds :created-at and :updated-at if not included.

   @param model-class [symbol] Model class name
   @param columns [list] Column list (keywords)
   @return [list] Adjusted column list
   @condition error When model-class is not found
   "
  (let ((table-info (gethash model-class clails/model/base-model::*table-information*)))
    (unless table-info
      (error "Model class '~A' not found in table information. Did you forget to call initialize-table-information?" model-class))

    (let ((adjusted-columns (remove :id columns)))
      (unless (member :created-at adjusted-columns)
        (push :created-at adjusted-columns))
      (unless (member :updated-at adjusted-columns)
        (push :updated-at adjusted-columns))
      adjusted-columns)))


;;; ----------------------------------------
;;; Model type filtering
;;; ----------------------------------------

(defun filter-models-by-class (model-class list-of-model on-type-mismatch)
  "Filter model instances by class.

   Checks if all instances are of the specified model class.
   Behavior depends on on-type-mismatch:
   - :error - Signal error if different class exists (default)
   - :skip - Filter out different classes and continue

   @param model-class [symbol] Model class name
   @param list-of-model [list] List of model instances
   @param on-type-mismatch [keyword] Error handling mode (:error or :skip)
   @return [list] Filtered list of model instances
   @condition type-mismatch-error When on-type-mismatch is :error and type mismatch occurs
   "
  (let ((filtered nil)
        (mismatched nil))
    (dolist (inst list-of-model)
      (if (eq (class-name (class-of inst)) model-class)
          (push inst filtered)
          (push inst mismatched)))

    (when mismatched
      (case on-type-mismatch
        (:error
         (error 'type-mismatch-error
                :expected-class model-class
                :actual-instances mismatched))
        (:skip
         (warn "insert-bulk: Skipping ~D instances that are not of class '~A'."
               (length mismatched)
               model-class))))

    (nreverse filtered)))


;;; ----------------------------------------
;;; Batch insert for plists
;;; ----------------------------------------

(defun batch-insert-plist (model-class columns list batch-size use-transaction connection)
  "Execute batch INSERT for plist list.

   @param model-class [symbol] Model class name
   @param columns [list] Column list (keywords)
   @param list [list] List of plists
   @param batch-size [integer] Batch size
   @param use-transaction [boolean] Use transaction
   @param connection [connection] Optional database connection
   @return [integer] Number of inserted rows
   "
  (let ((table-name (get-table-name model-class))
        (total-inserted 0)
        (now (get-universal-time)))

    ;; Prepare timestamps for all records
    (let ((prepared-list (mapcar (lambda (plist)
                                  (prepare-timestamps-plist plist columns now))
                                list)))

      ;; Split into batches
      (let ((batches (split-into-batches prepared-list batch-size)))
        (labels ((do-insert ()
                   (dolist (batch batches)
                     (let ((count (execute-bulk-insert-plist model-class table-name columns batch connection)))
                       (incf total-inserted count)))))

          (if use-transaction
              (let ((conn (or connection (get-connection))))
                (with-transaction-using-connection conn
                  (do-insert)))
              (do-insert)))))

    total-inserted))


;;; ----------------------------------------
;;; Batch insert for model instances
;;; ----------------------------------------

(defun batch-insert-instances (model-class columns list batch-size use-transaction connection)
  "Execute batch INSERT for model instance list.

   @param model-class [symbol] Model class name
   @param columns [list] Column list (keywords)
   @param list [list] List of model instances
   @param batch-size [integer] Batch size
   @param use-transaction [boolean] Use transaction
   @param connection [connection] Optional database connection
   @return [integer] Number of inserted rows
   "
  (let ((table-name (get-table-name model-class))
        (total-inserted 0)
        (now (get-universal-time)))

    ;; Prepare timestamps for all instances
    (dolist (inst list)
      (prepare-timestamps-instance inst columns now))

    ;; Split into batches
    (let ((batches (split-into-batches list batch-size)))
      (labels ((do-insert ()
                 (dolist (batch batches)
                   (let ((count (execute-bulk-insert-instances model-class table-name columns batch connection)))
                     (incf total-inserted count)))))

        (if use-transaction
            (let ((conn (or connection (get-connection))))
              (with-transaction-using-connection conn
                (do-insert)))
            (do-insert))))

    total-inserted))


;;; ----------------------------------------
;;; Execute bulk INSERT for plists
;;; ----------------------------------------

(defun execute-bulk-insert-plist (model-class table-name columns batch connection)
  "Execute bulk INSERT for a batch of plists.

   @param model-class [symbol] Model class name
   @param table-name [string] Table name
   @param columns [list] Column list (keywords)
   @param batch [list] Batch of plists
   @param connection [connection] Optional database connection
   @return [integer] Number of inserted rows
   "
  (let* ((conn (or connection (get-connection)))
         (placeholder (make-placeholders (length columns)))
         (values-clause (make-multi-row-values placeholder (length batch)))
         (column-names (mapcar #'kebab->snake columns))
         (sql (format nil "INSERT INTO ~A (~{~A~^, ~}) VALUES ~A"
                     table-name
                     column-names
                     values-clause))
         (params (flatten (mapcar (lambda (plist)
                                   (extract-plist-values plist columns model-class))
                                 batch))))

    (when (log-level-enabled-p :debug :sql)
      (log.sql (format nil "sql: ~S" sql))
      (log.sql (format nil "params: ~S" params)))

    (dbi-cp:execute
     (dbi-cp:prepare conn sql)
     params)
    (dbi-cp:row-count conn)))


;;; ----------------------------------------
;;; Execute bulk INSERT for model instances
;;; ----------------------------------------

(defun execute-bulk-insert-instances (model-class table-name columns batch connection)
  "Execute bulk INSERT for a batch of model instances.

   @param model-class [symbol] Model class name
   @param table-name [string] Table name
   @param columns [list] Column list (keywords)
   @param batch [list] Batch of model instances
   @param connection [connection] Optional database connection
   @return [integer] Number of inserted rows
   "
  (let* ((conn (or connection (get-connection)))
         (placeholder (make-placeholders (length columns)))
         (values-clause (make-multi-row-values placeholder (length batch)))
         (column-names (mapcar #'kebab->snake columns))
         (sql (format nil "INSERT INTO ~A (~{~A~^, ~}) VALUES ~A"
                     table-name
                     column-names
                     values-clause))
         (params (flatten (mapcar (lambda (inst)
                                   (extract-instance-values inst columns model-class))
                                 batch))))

    (when (log-level-enabled-p :debug :sql)
      (log.sql (format nil "sql: ~S" sql))
      (log.sql (format nil "params: ~S" params)))

    (dbi-cp:execute
     (dbi-cp:prepare conn sql)
     params)
    (dbi-cp:row-count conn)))


;;; ----------------------------------------
;;; Helper functions
;;; ----------------------------------------

(defun prepare-timestamps-plist (plist columns now)
  "Set created-at and updated-at timestamps in plist.

   If :created-at or :updated-at is in the column list and not set in plist,
   sets the specified time.

   @param plist [plist] Property list
   @param columns [list] Column list (keywords)
   @param now [integer] Timestamp to set
   @return [plist] Plist with timestamps set
   "
  (let ((result (copy-list plist)))
    (when (and (member :created-at columns)
               (null (getf result :created-at)))
      (setf (getf result :created-at) now))
    (when (and (member :updated-at columns)
               (null (getf result :updated-at)))
      (setf (getf result :updated-at) now))
    result))


(defun prepare-timestamps-instance (instance columns now)
  "Set created-at and updated-at timestamps in model instance.

   If :created-at or :updated-at is in the column list and not set in instance,
   sets the specified time.

   @param instance [<base-model>] Model instance
   @param columns [list] Column list (keywords)
   @param now [integer] Timestamp to set
   @return [<base-model>] Instance with timestamps set
   "
  (when (and (member :created-at columns)
             (null (ref instance :created-at)))
    (setf (ref instance :created-at) now))
  (when (and (member :updated-at columns)
             (null (ref instance :updated-at)))
    (setf (ref instance :updated-at) now))
  instance)


(defun split-into-batches (items batch-size)
  "Split list into batches of specified size.

   @param items [list] List to split
   @param batch-size [integer] Batch size
   @return [list of list] List of batches
   "
  (loop for i from 0 below (length items) by batch-size
        collect (subseq items i (min (+ i batch-size) (length items)))))


(defun make-placeholders (count)
  "Create SQL placeholder string.

   @param count [integer] Number of placeholders
   @return [string] Placeholder string (e.g., \"(?, ?, ?)\")
   "
  (format nil "(~{~A~^, ~})" (make-list count :initial-element "?")))


(defun make-multi-row-values (placeholder-template row-count)
  "Create multi-row VALUES clause.

   @param placeholder-template [string] Placeholder template (e.g., \"(?, ?, ?)\")
   @param row-count [integer] Number of rows
   @return [string] VALUES clause (e.g., \"(?, ?, ?), (?, ?, ?), (?, ?, ?)\")
   "
  (format nil "~{~A~^, ~}" (make-list row-count :initial-element placeholder-template)))


(defun extract-plist-values (plist columns model-class)
  "Extract values for specified columns from plist.

   Applies cl-db-fn conversion for each column.

   @param plist [plist] Property list
   @param columns [list] Column list (keywords)
   @param model-class [symbol] Model class name
   @return [list] List of values
   "
  (let ((columns-plist (getf (gethash model-class clails/model/base-model::*table-information*)
                            :columns-plist)))
    (mapcar (lambda (col)
              (let* ((value (getf plist col))
                     (column-info (getf columns-plist col))
                     (cl-db-fn (if column-info
                                  (getf column-info :cl-db-fn)
                                  #'identity)))
                (funcall cl-db-fn value)))
            columns)))


(defun extract-instance-values (instance columns model-class)
  "Extract values for specified columns from model instance.

   Applies cl-db-fn conversion for each column.

   @param instance [<base-model>] Model instance
   @param columns [list] Column list (keywords)
   @param model-class [symbol] Model class name
   @return [list] List of values
   "
  (let ((columns-plist (getf (gethash model-class clails/model/base-model::*table-information*)
                            :columns-plist)))
    (mapcar (lambda (col)
              (let* ((value (ref instance col))
                     (column-info (getf columns-plist col))
                     (cl-db-fn (if column-info
                                  (getf column-info :cl-db-fn)
                                  #'identity)))
                (funcall cl-db-fn value)))
            columns)))


(defun get-table-name (model-class)
  "Get table name from model class name.

   @param model-class [symbol] Model class name
   @return [string] Table name
   "
  (let ((table-info (gethash model-class clails/model/base-model::*table-information*)))
    (if table-info
        (getf table-info :table-name)
        (kebab->snake (string-downcase (symbol-name model-class))))))

;;;; ========================================
;;;; UPDATE Operations
;;;; ========================================

;;; ----------------------------------------
;;; update-all
;;; ----------------------------------------

(defun update-all (list-of-model &key connection)
  "Update model instances one by one.

   Each instance is updated using update1, so
   updated_at is set on each instance after UPDATE.
   Only columns marked as dirty are updated.

   Note: The list can contain instances of different model classes.
   Each instance will be updated to its own table independently.
   This is an intentional design to allow flexible update operations.

   @param list-of-model [list] List of <base-model> instances (can be mixed model classes)
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [list] List of model instances with updated_at updated
   @condition database-error When UPDATE fails
   @condition optimistic-lock-error Optimistic lock error (version mismatch)
   "
  (when (null list-of-model)
    (return-from update-all nil))

  (unless (typep (first list-of-model) '<base-model>)
    (error "update-all requires a list of model instances"))

  (dolist (model list-of-model)
    (let ((rows-updated (clails/model/query::update1 model :connection connection)))
      (when (= rows-updated 0)
        (error 'clails/condition:optimistic-lock-error))))

  list-of-model)


;;; ----------------------------------------
;;; update-bulk
;;; ----------------------------------------

(defun update-bulk (model-class columns list-of-model &key (batch-size 100) (use-transaction t) (on-type-mismatch :error) connection)
  "Execute bulk UPDATE and return the number of updated rows.

   Accepts a list of model instances and performs bulk UPDATE.
   Explicitly specifies the model class and columns to update.

   Column information is automatically adjusted:
   - :id is removed if included (not updatable)
   - :created-at is removed if included (not updatable)
   - :updated-at is added if not included (automatically updated)

   @param model-class [symbol] Model symbol to UPDATE (e.g., '<user>)
   @param columns [list] List of columns to UPDATE (list of keywords) (e.g., '(:name :age))
   @param list-of-model [list] List of model instances
   @param batch-size [integer] Batch size (default: 100)
   @param use-transaction [boolean] Use transaction (default: t)
   @param on-type-mismatch [keyword] Behavior on type mismatch (default: :error)
                                     :error - Raise error when type mismatch occurs (default)
                                     :skip - Skip instances that don't match the model class
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [integer] Number of updated records
   @condition database-error When UPDATE fails
   @condition type-mismatch-error When on-type-mismatch is :error and type mismatch occurs
   "
  (when (null list-of-model)
    (return-from update-bulk 0))

  ;; Validate and adjust column information
  (let ((adjusted-columns (validate-and-adjust-update-columns columns)))

    ;; Type check and filtering
    (let ((filtered-list (filter-models-by-class model-class list-of-model on-type-mismatch)))

      (when (null filtered-list)
        (return-from update-bulk 0))

      ;; Execute bulk UPDATE
      (batch-update-instances model-class adjusted-columns filtered-list batch-size use-transaction connection))))


;;; ----------------------------------------
;;; Column validation and adjustment for UPDATE
;;; ----------------------------------------

(defun validate-and-adjust-update-columns (columns)
  "Validate and adjust column list for UPDATE.

   - Removes :id if included (not updatable)
   - Removes :created-at if included (not updatable)
   - Adds :updated-at if not included

   @param columns [list] Column list (keywords)
   @return [list] Adjusted column list
   "
  (let ((adjusted-columns (remove-if (lambda (col)
                                      (or (eq col :id)
                                          (eq col :created-at)))
                                    columns)))
    ;; Add updated-at (avoid duplicates)
    (unless (member :updated-at adjusted-columns)
      (push :updated-at adjusted-columns))
    adjusted-columns))


;;; ----------------------------------------
;;; Batch update for model instances
;;; ----------------------------------------

(defun batch-update-instances (model-class columns list-of-model batch-size use-transaction connection)
  "Execute batch UPDATE for model instance list (internal function).

   Updates only the specified columns.
   updated-at is automatically set to the current time.

   @param model-class [symbol] Model class name
   @param columns [list] List of columns to update (keywords)
   @param list-of-model [list] List of model instances
   @param batch-size [integer] Batch size
   @param use-transaction [boolean] Use transaction
   @param connection [connection] Optional database connection
   @return [integer] Number of updated records
   "
  (let ((table-name (get-table-name model-class))
        (total 0)
        (now (get-universal-time)))  ;; Pre-generate timestamp

    (labels ((execute-batch (batch)
               ;; Generate UPDATE SQL (once per batch)
               (let* ((set-clause (generate-set-clause columns))
                      (sql (format nil "UPDATE ~A SET ~A WHERE id = ?"
                                  table-name set-clause))
                      (conn (or connection (get-connection))))

                 (when (log-level-enabled-p :debug :sql)
                   (log.sql (format nil "sql: ~S" sql)))

                 ;; Execute for each instance
                 (dolist (instance batch)
                   ;; Set updated-at timestamp
                   (when (member :updated-at columns)
                     (setf (ref instance :updated-at) now))

                   ;; Extract values with cl-db-fn conversion
                   (let ((params (append
                                  (extract-instance-values instance columns model-class)
                                  (list (ref instance :id)))))
                     (when (log-level-enabled-p :debug :sql)
                       (log.sql (format nil "params: ~S" params)))
                     (dbi-cp:execute (dbi-cp:prepare conn sql) params)
                     (incf total)))
                 total)))

      (if (and use-transaction (null connection))
          (let ((conn (get-connection)))
            (with-transaction-using-connection conn
              (loop for batch in (split-into-batches list-of-model batch-size)
                    do (execute-batch batch))))
          (loop for batch in (split-into-batches list-of-model batch-size)
                do (execute-batch batch)))

      total)))


;;; ----------------------------------------
;;; Helper function for UPDATE
;;; ----------------------------------------

(defun generate-set-clause (columns)
  "Generate SET clause for UPDATE statement.

   @param columns [list] List of columns to update (keywords)
   @return [string] SET clause (e.g., \"name = ?, age = ?, updated_at = ?\")
   "
  (let ((column-strs (mapcar (lambda (col)
                              (kebab->snake (symbol-name col)))
                            columns)))
    (format nil "~{~A = ?~^, ~}" column-strs)))

;;;; ========================================
;;;; DELETE Operations
;;;; ========================================

;;; ----------------------------------------
;;; delete-all
;;; ----------------------------------------

(defun delete-all (list-of-model &key (cascade nil) connection)
  "Delete model instances one by one.

   Each instance is deleted using destroy.
   Cascade deletion option can be specified.

   @param list-of-model [list] List of <base-model> instances
   @param cascade [boolean] Whether to perform cascade delete (default: nil)
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [list] List of deleted model instances
   @condition database-error When DELETE fails
   "
  (when (null list-of-model)
    (return-from delete-all nil))

  (unless (typep (first list-of-model) '<base-model>)
    (error "delete-all requires a list of model instances"))

  (dolist (model list-of-model)
    (destroy model :cascade cascade))

  list-of-model)


;;; ----------------------------------------
;;; delete-bulk
;;; ----------------------------------------

(defun delete-bulk (model-class list-of-model &key (batch-size 100) (use-transaction t) (on-type-mismatch :error) connection)
  "Execute bulk DELETE and return the number of deleted rows.

   Deletes only instances of the specified model class.
   Does not support cascade deletion.

   @param model-class [symbol] Model symbol to DELETE (e.g., '<user>)
   @param list-of-model [list] List of model instances
   @param batch-size [integer] Batch size (default: 100)
   @param use-transaction [boolean] Use transaction (default: t)
   @param on-type-mismatch [keyword] Behavior on type mismatch (default: :error)
                                     :error - Raise error when type mismatch occurs (default)
                                     :skip - Skip instances that don't match the model class
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [integer] Number of deleted records
   @condition database-error When DELETE fails
   @condition type-mismatch-error When on-type-mismatch is :error and type mismatch occurs
   "
  (when (null list-of-model)
    (return-from delete-bulk 0))

  ;; Type check and filtering
  (let ((filtered-list (filter-models-by-class model-class list-of-model on-type-mismatch)))

    (when (null filtered-list)
      (return-from delete-bulk 0))

    ;; Execute bulk DELETE
    (batch-delete-instances model-class filtered-list batch-size use-transaction connection)))


;;; ----------------------------------------
;;; Batch delete for model instances
;;; ----------------------------------------

(defun batch-delete-instances (model-class list-of-model batch-size use-transaction connection)
  "Execute batch DELETE for model instance list (internal function).

   Uses IN clause for optimized deletion.

   @param model-class [symbol] Model class name
   @param list-of-model [list] List of model instances
   @param batch-size [integer] Batch size
   @param use-transaction [boolean] Use transaction
   @param connection [connection] Optional database connection
   @return [integer] Number of deleted records
   "
  (let ((table-name (get-table-name model-class))
        (total 0))

    (labels ((execute-batch (batch)
               ;; Extract IDs from batch
               (let* ((ids (mapcar (lambda (inst) (ref inst :id)) batch))
                      (placeholders (format nil "~{~A~^, ~}" (make-list (length ids) :initial-element "?")))
                      (sql (format nil "DELETE FROM ~A WHERE id IN (~A)"
                                  table-name
                                  placeholders))
                      (conn (or connection (get-connection))))

                 (when (log-level-enabled-p :debug :sql)
                   (log.sql (format nil "sql: ~S" sql))
                   (log.sql (format nil "params: ~S" ids)))

                 (dbi-cp:execute (dbi-cp:prepare conn sql) ids)
                 (let ((count (dbi-cp:row-count conn)))
                   (incf total count)))))

      (if (and use-transaction (null connection))
          (let ((conn (get-connection)))
            (with-transaction-using-connection conn
              (loop for batch in (split-into-batches list-of-model batch-size)
                    do (execute-batch batch))))
          (loop for batch in (split-into-batches list-of-model batch-size)
                do (execute-batch batch)))

      total)))
