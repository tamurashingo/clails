(in-package #:cl-user)
(defpackage #:clails/model/lock
  (:use #:cl)
  (:import-from #:clails/environment
                #:*default-lock-mode*
                #:*database-type*
                #:*sqlite3-busy-timeout*
                #:*sqlite3-lock-retry-count*
                #:*sqlite3-transaction-mode*
                #:*sqlite3-lock-module-loaded*)
  (:import-from #:clails/model/query
                #:execute-query)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:export #:with-locked-transaction
           #:load-sqlite3-lock-module))
(in-package #:clails/model/lock)


;;;; ========================================
;;;; Export Macro

(defmacro with-locked-transaction ((variable-name query-spec parameter-plist
                                    &key (mode '*default-lock-mode*) nowait skip-locked)
                                   &body body)
  "Execute body within a transaction with locked records.

   Creates a transaction, retrieves and locks records based on the query specification,
   then executes the body. The transaction is automatically committed on success
   or rolled back on error.

   For SQLite3, sets *sqlite3-transaction-mode* to control BEGIN statement type,
   and implements retry logic on lock errors with exponential backoff.

   @param variable-name [symbol] Variable to bind the retrieved record(s)
   @param query-spec [query form] Query form like (query <model> ...)
   @param parameter-plist [plist] Parameters for the query
   @param mode [keyword] Lock mode (default: *default-lock-mode*)
                         - PostgreSQL: :for-update, :for-share, :for-no-key-update, :for-key-share
                         - MySQL: :for-update, :for-share
                         - SQLite3: :immediate, :exclusive
   @param nowait [boolean] If T, don't wait for lock
   @param skip-locked [boolean] If T, skip locked rows
   @return [value] Returns the value of the last form in body

   SQLite3 :exclusive mode note:
   While :exclusive mode can be specified for SQLite3, use it with extreme caution.
   When :exclusive is used, ALL other connections will fail with errors, even those
   not using locks. This can cause unintended errors in other parts of the application.
   For most use cases, :immediate mode is recommended.

   Example:
   (with-locked-transaction (book
                             (query <book> :as :b :where (:= (:b :id) :book-id))
                             '(:book-id 1))
     (setf (ref book :title) \"New Title\")
     (save book))
   "
  (let ((connection-var (gensym "CONNECTION"))
        (query-var (gensym "QUERY"))
        (mode-var (gensym "MODE")))
    `(let* ((,connection-var (get-connection))
            (,mode-var ,mode))

       ;; check lock mode
       (check-lock-mode *database-type* ,mode-var)

       ;; Set busy timeout for SQLite3
       (set-busy-timeout *database-type* ,connection-var)

       (when (log-level-enabled-p :sql :debug)
         (log.sql (format nil "BEGIN LOCKED TRANSACTION (mode: ~A)" ,mode-var)))

       ;; Retry logic for SQLite3 lock errors
       (retry-on-lock-error *database-type*
         (lambda ()
           ;; Set transaction mode for SQLite3
           (let ((clails/environment:*sqlite3-transaction-mode* ,mode-var))
             (dbi-cp:with-transaction ,connection-var
               ;; Evaluate query and add lock clause
               (let* ((,query-var ,query-spec)
                      (,query-var (build-lock-query ,query-var ,mode-var
                                                    :nowait ,nowait
                                                    :skip-locked ,skip-locked))
                      (,variable-name (execute-query ,query-var ,parameter-plist
                                                     :connection ,connection-var)))
                 ,@body))))))))


;;;; ========================================
;;;; Export Function

(defun load-sqlite3-lock-module ()
  "Load sqlite3-lock module for SQLite3 database.

   Loads src/model/impl/sqlite3-lock.lisp if the module hasn't been loaded yet.
   This function should be called from startup-connection-pool when database type is SQLite3.

   @return [boolean] T if module was loaded, NIL if already loaded
   "
  (unless clails/environment:*sqlite3-lock-module-loaded*
    (when (log-level-enabled-p :sql :info)
      (log.sql "Loading SQLite3 lock module"))
    (load (merge-pathnames "src/model/impl/sqlite3-lock.lisp"
                           (asdf:system-source-directory :clails)))
    (setf clails/environment:*sqlite3-lock-module-loaded* t)
    t))


;;;; ========================================
;;;; Internal Functions

(defun build-lock-query (query-spec mode &key nowait skip-locked)
  "Build a query with lock clause.

   Adds lock clause to the existing query specification based on
   the database type and lock mode.

   @param query-spec [<query>] Original query specification
   @param mode [keyword] Lock mode
   @param nowait [boolean] NOWAIT option
   @param skip-locked [boolean] SKIP LOCKED option
   @return [<query>] Query with lock clause
   "
  (let ((lock-clause (generate-lock-clause *database-type* mode
                                           :nowait nowait
                                           :skip-locked skip-locked)))
    ;; Copy query and add lock clause
    (setf (slot-value query-spec 'clails/model/query::lock-clause) lock-clause)
    query-spec))


(defun retry-on-lock-error (database-type thunk)
  "Execute thunk with retry logic for lock errors.

   For SQLite3, retries on SQLITE_BUSY errors with exponential backoff.
   For PostgreSQL/MySQL, executes thunk without retry.

   @param database-type [<database-type>] Database type instance
   @param thunk [function] Function to execute
   @return [value] Returns the value of thunk
   "
  (if (typep database-type 'clails/environment:<database-type-sqlite3>)
      (retry-on-sqlite3-lock-error thunk)
      (funcall thunk)))


(defun retry-on-sqlite3-lock-error (thunk)
  "Retry thunk on SQLite3 lock errors with exponential backoff.

   @param thunk [function] Function to execute
   @return [value] Returns the value of thunk
   "
  (let ((max-retries clails/environment:*sqlite3-lock-retry-count*)
        (retry-count 0))
    (loop
      (handler-case
          (return (funcall thunk))
        (error (e)
          (if (and (< retry-count max-retries)
                   (search "database is locked" (format nil "~A" e)))
              (progn
                (incf retry-count)
                (let ((wait-ms (* 100 (expt 2 (1- retry-count)))))
                  (when (log-level-enabled-p :sql :warn)
                    (log.sql (format nil "SQLite3 database locked, retry ~A/~A after ~Ams"
                                     retry-count max-retries wait-ms)))
                  (sleep (/ wait-ms 1000.0))))
              (error e)))))))



;;;; ========================================
;;;; Generic Functions

(defgeneric check-lock-mode (database-type mode)
  (:documentation "Validate that the specified lock mode is supported by the database type.

   Raises an error if the lock mode is not supported.

   @param database-type [<database-type>] Database type instance
   @param mode [keyword] Lock mode to validate
   "))

(defgeneric set-busy-timeout (database-type connection)
  (:documentation "Set busy timeout for database connection.

   For SQLite3, sets the busy timeout to wait for locked database.
   For PostgreSQL/MySQL, this is a no-op.

   @param database-type [<database-type>] Database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   "))

(defmethod set-busy-timeout ((db clails/environment:<database-type-postgresql>) connection)
  "PostgreSQL does not need busy timeout setting.

   @param db [<database-type-postgresql>] PostgreSQL database type instance
   @param connection [dbi:<dbi-connection>] Database connection (ignored)
   @return [null] Always returns NIL
   "
  (declare (ignore connection))
  nil)

(defmethod set-busy-timeout ((db clails/environment:<database-type-mysql>) connection)
  "MySQL does not need busy timeout setting.

   @param db [<database-type-mysql>] MySQL database type instance
   @param connection [dbi:<dbi-connection>] Database connection (ignored)
   @return [null] Always returns NIL
   "
  (declare (ignore connection))
  nil)

(defmethod set-busy-timeout ((db clails/environment:<database-type-sqlite3>) connection)
  "Set SQLite3 busy timeout in milliseconds.

   @param db [<database-type-sqlite3>] SQLite3 database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   @return [null] Returns NIL
   "
  (let ((timeout-ms clails/environment:*sqlite3-busy-timeout*))
    (when (log-level-enabled-p :sql :debug)
      (log.sql (format nil "PRAGMA busy_timeout = ~A" timeout-ms)))
    (dbi-cp:execute
     (dbi-cp:prepare connection (format nil "PRAGMA busy_timeout = ~A" timeout-ms))
     '())))

(defmethod set-busy-timeout ((db clails/environment:<database-type-dummy>) connection)
  "Dummy database does not need busy timeout setting.

   @param db [<database-type-dummy>] Dummy database type instance
   @param connection [dbi:<dbi-connection>] Database connection (ignored)
   @return [null] Always returns NIL
   "
  (declare (ignore connection))
  nil)

(defmethod check-lock-mode ((db clails/environment:<database-type-postgresql>) mode)
  "Validate PostgreSQL lock mode.

   Supported modes: :for-update, :for-share, :for-no-key-update, :for-key-share

   @param db [<database-type-postgresql>] PostgreSQL database type instance
   @param mode [keyword] Lock mode to validate
   @condition error Raised when unsupported lock mode is specified
   "
  (unless (member mode '(:for-update :for-share :for-no-key-update :for-key-share))
    (error "Unsupported lock mode ~A for PostgreSQL. Supported modes: :for-update, :for-share, :for-no-key-update, :for-key-share" mode)))

(defmethod check-lock-mode ((db clails/environment:<database-type-mysql>) mode)
  "Validate MySQL lock mode.

   Supported modes: :for-update, :for-share

   @param db [<database-type-mysql>] MySQL database type instance
   @param mode [keyword] Lock mode to validate
   @condition error Raised when unsupported lock mode is specified
   "
  (unless (member mode '(:for-update :for-share))
    (error "Unsupported lock mode ~A for MySQL. Supported modes: :for-update, :for-share" mode)))

(defmethod check-lock-mode ((db clails/environment:<database-type-sqlite3>) mode)
  "Validate SQLite3 lock mode.

   Supported modes: :immediate, :exclusive, :deferred

   @param db [<database-type-sqlite3>] SQLite3 database type instance
   @param mode [keyword] Lock mode to validate
   @condition error Raised when unsupported lock mode is specified
   "
  (unless (member mode '(:immediate :exclusive :deferred))
    (error "Unsupported lock mode ~A for SQLite3. Supported modes: :immediate, :exclusive, :deferred" mode)))

(defmethod check-lock-mode ((db clails/environment:<database-type-dummy>) mode)
  "Dummy database accepts any lock mode.

   @param db [<database-type-dummy>] Dummy database type instance
   @param mode [keyword] Lock mode (ignored)
   @return [null] Always returns NIL
   "
  (declare (ignore mode))
  nil)


(defgeneric generate-lock-clause (database-type mode &key nowait skip-locked)
  (:documentation "Generate database-specific lock clause.

   @param database-type [<database-type>] Database type instance
   @param mode [keyword] Lock mode
   @param nowait [boolean] NOWAIT option
   @param skip-locked [boolean] SKIP LOCKED option
   @return [string] SQL lock clause
   "))

(defmethod generate-lock-clause ((db clails/environment:<database-type-postgresql>) mode
                                 &key nowait skip-locked)
  "Generate PostgreSQL lock clause."
  (let ((lock-type (case mode
                     (:for-update "FOR UPDATE")
                     (:for-share "FOR SHARE")
                     (:for-no-key-update "FOR NO KEY UPDATE")
                     (:for-key-share "FOR KEY SHARE")
                     (otherwise nil)))
        (options (append (when nowait (list "NOWAIT"))
                        (when skip-locked (list "SKIP LOCKED")))))
    (if options
        (format nil "~A ~{~A~^ ~}" lock-type options)
        lock-type)))

(defmethod generate-lock-clause ((db clails/environment:<database-type-mysql>) mode
                                &key nowait skip-locked)
  "Generate MySQL lock clause."
  (let ((lock-type (case mode
                     (:for-update "FOR UPDATE")
                     (:for-share "FOR SHARE")
                     (otherwise nil)))
        (options (append (when nowait (list "NOWAIT"))
                        (when skip-locked (list "SKIP LOCKED")))))
    (if options
        (format nil "~A ~{~A~^ ~}" lock-type options)
        lock-type)))

(defmethod generate-lock-clause ((db clails/environment:<database-type-sqlite3>) mode
                                &key nowait skip-locked)
  "Generate SQLite3 lock clause.

   SQLite3 does not support row-level locks.
   Returns nil as locking is handled at transaction level."
  nil)  ; Locking is handled at transaction level, not in SQL

(defmethod generate-lock-clause ((db clails/environment:<database-type-dummy>) mode
                                &key nowait skip-locked)
  "Generate dummy lock clause (returns nil)."
  (declare (ignore mode nowait skip-locked))
  nil)
