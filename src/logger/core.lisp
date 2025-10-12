(defpackage #:clails/logger/core
  (:use #:cl)
  (:import-from #:local-time
                #:now)
  (:export #:<log-record>
           #:make-log-record
           #:log-record-timestamp
           #:log-record-level
           #:log-record-message
           #:log-record-context
           #:<logger>
           #:log-message-to
           #:register-logger
           #:get-logger
           #:remove-logger
           #:with-log-context
           #:log-to
           #:log.sql
           #:log.web-access
           #:log.audit
           #:set-logger-level
           #:add-appender
           #:clear-loggers
           #:log-package.trace
           #:log-package.debug
           #:log-package.info
           #:log-package.warn
           #:log-package.error
           #:log-package.fatal))
(in-package #:clails/logger/core)

;;; ------------------------------------------------------------------
;;; Log Record
;;; ------------------------------------------------------------------
(defclass <log-record> ()
  ((timestamp :initarg :timestamp :reader log-record-timestamp)
   (level :initarg :level :reader log-record-level)
   (message :initarg :message :reader log-record-message)
   (context :initarg :context :reader log-record-context))
  (:documentation "Represents a single log entry, containing timestamp, level, message, and contextual data."))

(defun make-log-record (&key level message context)
  "Creates a new log-record instance."
  (make-instance '<log-record>
                 :timestamp (now)
                 :level level
                 :message message
                 :context context))

;;; ------------------------------------------------------------------
;;; Appender Generic Function
;;; ------------------------------------------------------------------
(defgeneric log-append (appender record)
  (:documentation "Appends a log record to the specified appender."))

;;; ------------------------------------------------------------------
;;; Logger
;;; ------------------------------------------------------------------
(defclass <logger> ()
  ((name
    :initarg :name
    :reader logger-name
    :initform :root
    :documentation "Logger name.")
   (appenders
    :initarg :appenders
    :accessor logger-appenders
    :initform '()
    :documentation "List of appenders attached to this logger.")
   (level
    :initarg :level
    :accessor logger-level
    :initform :info
    :documentation "Log level for this logger.")
   (parent
    :initarg :parent
    :accessor logger-parent
    :initform nil
    :documentation "Parent logger for hierarchical structure."))
  (:documentation "The logger class, holding appenders and log level configuration."))

(defvar *logger-registry* (make-hash-table :test 'equal)
  "Hash table managing named loggers.")

(defvar *log-context* nil
  "A dynamic context for log messages, managed with with-log-context.")

;;; ------------------------------------------------------------------
;;; Package-Based Logger Hierarchy
;;; ------------------------------------------------------------------
(defun package-to-logger-name (package-designator)
  "Convert a package designator to a normalized logger name keyword.

   @param package-designator [package] Package object
   @param package-designator [symbol] Package name symbol
   @param package-designator [string] Package name string
   @param package-designator [keyword] Logger name keyword
   @return [keyword] Normalized logger name
   "
  (etypecase package-designator
    (keyword
     ;; Already a keyword, just normalize to uppercase
     (intern (string-upcase (symbol-name package-designator)) :keyword))
    (symbol
     ;; Symbol: convert to keyword
     (intern (string-upcase (symbol-name package-designator)) :keyword))
    (string
     ;; String: convert to keyword
     (intern (string-upcase package-designator) :keyword))
    (package
     ;; Package object: get name and convert
     (intern (string-upcase (package-name package-designator)) :keyword))))

(defun get-parent-logger-name (logger-name)
  "Get the parent logger name from a hierarchical logger name.

   Examples:
     :CLAILS/MODEL/QUERY => :CLAILS/MODEL
     :CLAILS/MODEL => :CLAILS
     :CLAILS => :ROOT
     :ROOT => NIL

   @param logger-name [keyword] Logger name
   @return [keyword] Parent logger name or nil if at root
   "
  (let ((name-string (symbol-name logger-name)))
    (cond
      ;; Already at root
      ((string= name-string "ROOT")
       nil)
      ;; No hierarchy separator found, parent is :ROOT
      ((not (position #\/ name-string))
       :root)
      ;; Has hierarchy, strip last component
      (t
       (let ((last-slash (position #\/ name-string :from-end t)))
         (intern (subseq name-string 0 last-slash) :keyword))))))

(defun find-logger-in-hierarchy (logger-name &optional (max-depth 20))
  "Find a logger by searching up the hierarchy.

   Searches for the logger by name, and if not found, searches
   parent loggers until a logger is found or root is reached.

   @param logger-name [keyword] Logger name
   @param max-depth [integer] Maximum hierarchy depth to prevent infinite loops
   @return [<logger>] Logger instance or nil if not found
   "
  (when (and logger-name (plusp max-depth))
    (or
     ;; Try exact match first
     (gethash logger-name *logger-registry*)
     ;; Try parent logger
     (let ((parent-name (get-parent-logger-name logger-name)))
       (when parent-name
         (find-logger-in-hierarchy parent-name (1- max-depth)))))))

;;; ------------------------------------------------------------------
;;; Logger Registry Management
;;; ------------------------------------------------------------------
(defun register-logger (name &key appenders appender level parent)
  "Register a named logger to the registry.

   If parent is not specified but the logger name has a hierarchical
   structure (e.g., :CLAILS/MODEL), the parent is automatically
   determined from the name.

   @param name [keyword] Logger name
   @param name [string] Package name string
   @param name [symbol] Package name symbol
   @param appenders [list] List of appenders
   @param appender [<appender>] Single appender
   @param level [keyword] Log level (default: :info)
   @param parent [<logger>] Parent logger (auto-determined if nil)
   @return [<logger>] Registered logger instance
   "
  (let* ((normalized-name (package-to-logger-name name))
         (appender-list (cond
                          (appenders appenders)
                          (appender (list appender))
                          (t '())))
         ;; Auto-determine parent if not specified
         (actual-parent (or parent
                            (let ((parent-name (get-parent-logger-name normalized-name)))
                              (when parent-name
                                (find-logger-in-hierarchy parent-name)))))
         (logger (make-instance '<logger>
                                :name normalized-name
                                :appenders appender-list
                                :level (or level :info)
                                :parent actual-parent)))
    (setf (gethash normalized-name *logger-registry*) logger)
    logger))

(defun get-logger (name)
  "Get a logger by name, searching up the hierarchy if not found.

   Search order:
   1. Exact match in registry
   2. Parent logger (hierarchical search)
   3. :ROOT logger
   4. nil if :ROOT not configured

   @param name [keyword] Logger name
   @param name [string] Package name string
   @param name [symbol] Package name symbol
   @param name [package] Package object
   @return [<logger>] Logger instance or nil
   "
  (let ((normalized-name (package-to-logger-name name)))
    (find-logger-in-hierarchy normalized-name)))

(defun remove-logger (name)
  "Remove a logger from the registry.

   @param name [keyword] Logger name
   @return [boolean] t if the logger was removed, nil otherwise
   "
  (remhash name *logger-registry*))

;;; ------------------------------------------------------------------
;;; Logging API
;;; ------------------------------------------------------------------
(defvar *log-levels* '(:trace :debug :info :warn :error :fatal :none)
  "Ordered list of available log levels.")

(defun log-level>= (level1 level2)
  "Returns true if level1 is greater than or equal to level2."
  (let ((pos1 (position level1 *log-levels*))
        (pos2 (position level2 *log-levels*)))
    (when (and pos1 pos2)
      (>= pos1 pos2))))

(defmethod log-append-all ((logger <logger>) (record <log-record>))
  "Append a log record to all appenders attached to the logger.

   @param logger [<logger>] Logger instance
   @param record [<log-record>] Log record to append
   @return [null] nil
   "
  (dolist (appender (logger-appenders logger))
    (log-append appender record)))

(defun log-message-to (logger-name level message &rest context)
  "Log a message to a specific named logger.

   @param logger-name [keyword] Logger name
   @param level [keyword] Log level
   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   @return [null] nil
   "
  (let ((logger (get-logger logger-name)))
    (when logger
      (when (log-level>= level (logger-level logger))
        (let ((record (make-log-record :level level
                                       :message message
                                       :context (append context clails/logger/core::*log-context*))))
          (log-append-all logger record))))))

(defmacro with-log-context ((&rest context) &body body)
  "Executes body with additional context added to all log messages."
  `(let ((clails/logger/core::*log-context* (append (list ,@context) clails/logger/core::*log-context*)))
     ,@body))

(defmacro log-to (logger-name level message &rest context)
  "Log a message to a specific named logger.

   @param logger-name [keyword] Logger name
   @param level [keyword] Log level
   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-message-to ,logger-name ,level ,message ,@context))

;;; ------------------------------------------------------------------
;;; Package-Based Logging API
;;; ------------------------------------------------------------------
(defun log-for-package (package level message &rest context)
  "Log a message using a package-based logger.

   The logger is determined by the package name, with hierarchical
   search if the exact logger is not found.

   @param package [package] Package object
   @param package [symbol] Package name symbol
   @param package [string] Package name string
   @param package [keyword] Logger name
   @param level [keyword] Log level
   @param message [string] Log message
   @param context [plist] Additional context
   @return [null] nil
   "
  (let* ((logger-name (package-to-logger-name package))
         (logger (get-logger logger-name)))
    (when logger
      (when (log-level>= level (logger-level logger))
        (let ((record (make-log-record :level level
                                       :message message
                                       :context (append context clails/logger/core::*log-context*))))
          (log-append-all logger record))))))

(defmacro log-package.trace (message &rest context)
  "Log a message using the current package's logger at TRACE level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :trace ,message ,@context))

(defmacro log-package.debug (message &rest context)
  "Log a message using the current package's logger at DEBUG level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :debug ,message ,@context))

(defmacro log-package.info (message &rest context)
  "Log a message using the current package's logger at INFO level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :info ,message ,@context))

(defmacro log-package.warn (message &rest context)
  "Log a message using the current package's logger at WARN level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :warn ,message ,@context))

(defmacro log-package.error (message &rest context)
  "Log a message using the current package's logger at ERROR level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :error ,message ,@context))

(defmacro log-package.fatal (message &rest context)
  "Log a message using the current package's logger at FATAL level.

   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(clails/logger/core::log-for-package *package* :fatal ,message ,@context))

;;; ------------------------------------------------------------------
;;; Purpose-Specific Logging API
;;; ------------------------------------------------------------------
(defmacro log.sql (message &rest context)
  "Log an SQL query to the :sql logger with :debug level.

   Used for logging database queries and operations.

   @param message [string] SQL query or database operation message
   @param context [plist] Additional context (e.g., :query-time, :rows-affected)
   "
  `(clails/logger/core::log-message-to :sql :debug ,message ,@context))

(defmacro log.web-access (message &rest context)
  "Log a web access to the :web-access logger with :info level.

   Used for logging HTTP requests and responses.

   @param message [string] Request summary message
   @param context [plist] Additional context (e.g., :method, :path, :status, :duration)
   "
  `(clails/logger/core::log-message-to :web-access :info ,message ,@context))

(defmacro log.audit (message &rest context)
  "Log an audit event to the :audit logger with :info level.

   Used for logging security-related events and user actions.

   @param message [string] Audit event message
   @param context [plist] Additional context (e.g., :user-id, :ip-address, :action)
   "
  `(clails/logger/core::log-message-to :audit :info ,message ,@context))

;;; ------------------------------------------------------------------
;;; Dynamic Configuration API
;;; ------------------------------------------------------------------
(defun set-logger-level (logger-name level)
  "Set the log level of a logger.

   Changes the minimum log level for the specified logger.

   @param logger-name [keyword] Logger name
   @param level [keyword] New log level (:trace, :debug, :info, :warn, :error, :fatal)
   @return [<logger>] Updated logger instance or nil if logger not found
   "
  (let ((logger (get-logger logger-name)))
    (when logger
      (setf (logger-level logger) level)
      logger)))

(defun add-appender (logger-name appender)
  "Add an appender to a logger.

   Adds a new appender to the logger's appenders list without removing existing ones.

   @param logger-name [keyword] Logger name
   @param appender [<appender>] Appender instance to add
   @return [<logger>] Updated logger instance or nil if logger not found
   "
  (let ((logger (get-logger logger-name)))
    (when logger
      (pushnew appender (logger-appenders logger))
      logger)))

(defun clear-loggers ()
  "Clear all registered loggers from the registry.

   This function removes all loggers from the registry.
   Useful for reinitializing the logging system.

   @return [null] nil
   "
  (clrhash *logger-registry*)
  nil)
