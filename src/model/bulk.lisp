(in-package #:cl-user)
(defpackage #:clails/model/bulk
  (:use #:cl)
  (:import-from #:clails/model/query
                #:generate-query
                #:<query>)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/model/transaction
                #:with-transaction-using-connection)
  (:import-from #:clails/environment
                #:*database-type*
                #:<database-type-postgresql>
                #:<database-type-mysql>
                #:<database-type-sqlite3>)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:import-from #:cl-batis
                #:<batis-sql>
                #:gen-sql-and-params)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export #:with-query-cursor
           #:show-query-sql))
(in-package #:clails/model/bulk)

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
  (when (log-level-enabled-p :sql :debug)
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
  (when (log-level-enabled-p :sql :debug)
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
  (when (log-level-enabled-p :sql :debug)
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
