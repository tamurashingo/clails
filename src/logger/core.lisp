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
           #:*logger*
           #:*logger-registry*
           #:*default-logger-name*
           #:logger-name
           #:logger-appenders
           #:logger-appender
           #:logger-level
           #:logger-parent
           #:log-append
           #:log-append-all
           #:log-message
           #:log-message-to
           #:register-logger
           #:get-logger
           #:remove-logger
           #:*log-context*
           #:with-log-context
           #:log.trace
           #:log.debug
           #:log.info
           #:log.warn
           #:log.error
           #:log.fatal
           #:log-to
           #:log.sql
           #:log.web-access
           #:log.audit
           #:set-logger-level
           #:add-appender
           #:clear-loggers))
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
    :initform :default
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

(defvar *default-logger-name* :default
  "Default logger name.")

(defvar *logger* nil
  "The current default logger instance.")

(defvar *log-context* nil
  "A dynamic context for log messages, managed with with-log-context.")

;;; ------------------------------------------------------------------
;;; Logger Registry Management
;;; ------------------------------------------------------------------
(defmethod logger-appender ((logger <logger>))
  "Get the first appender from the logger's appenders list.

   This method provides backward compatibility with the old API.

   @param logger [<logger>] Logger instance
   @return [<appender>] First appender or nil if no appenders exist
   "
  (first (logger-appenders logger)))

(defun register-logger (name &key appenders appender level parent)
  "Register a named logger to the registry.

   @param name [keyword] Logger name
   @param appenders [list] List of appenders
   @param appender [<appender>] Single appender (backward compatibility)
   @param level [keyword] Log level (default: :info)
   @param parent [<logger>] Parent logger for hierarchical structure
   @return [<logger>] Registered logger instance
   "
  (let* ((appender-list (cond
                          (appenders appenders)
                          (appender (list appender))
                          (t '())))
         (logger (make-instance '<logger>
                                :name name
                                :appenders appender-list
                                :level (or level :info)
                                :parent parent)))
    (setf (gethash name *logger-registry*) logger)
    logger))

(defun get-logger (name)
  "Get a logger by name from the registry.

   Returns the default logger if the specified logger does not exist.

   @param name [keyword] Logger name
   @return [<logger>] Logger instance or nil if not found
   "
  (or (gethash name *logger-registry*)
      (gethash *default-logger-name* *logger-registry*)))

(defun remove-logger (name)
  "Remove a logger from the registry.

   @param name [keyword] Logger name
   @return [boolean] t if the logger was removed, nil otherwise
   "
  (remhash name *logger-registry*))

;;; ------------------------------------------------------------------
;;; Logging API
;;; ------------------------------------------------------------------
(defvar *log-levels* '(:trace :debug :info :warn :error :fatal)
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
                                       :context (append context *log-context*))))
          (log-append-all logger record))))))

(defun log-message (level message &rest context)
  "Core logging function. Creates a log-record and passes it to the current logger's appenders if the level is sufficient.

   @param level [keyword] Log level
   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   @return [null] nil
   "
  (when *logger*
    (when (log-level>= level (logger-level *logger*))
      (let ((record (make-log-record :level level
                                     :message message
                                     :context (append context *log-context*))))
        (log-append-all *logger* record)))))

(defmacro with-log-context ((&rest context) &body body)
  "Executes body with additional context added to all log messages."
  `(let ((*log-context* (append (list ,@context) *log-context*)))
     ,@body))

(defmacro log.trace (message &rest context)
  "Logs a message with the :trace level."
  `(log-message :trace ,message ,@context))

(defmacro log.debug (message &rest context)
  "Logs a message with the :debug level."
  `(log-message :debug ,message ,@context))

(defmacro log.info (message &rest context)
  "Logs a message with the :info level."
  `(log-message :info ,message ,@context))

(defmacro log.warn (message &rest context)
  "Logs a message with the :warn level."
  `(log-message :warn ,message ,@context))

(defmacro log.error (message &rest context)
  "Logs a message with the :error level."
  `(log-message :error ,message ,@context))

(defmacro log.fatal (message &rest context)
  "Logs a message with the :fatal level."
  `(log-message :fatal ,message ,@context))

(defmacro log-to (logger-name level message &rest context)
  "Log a message to a specific named logger.

   @param logger-name [keyword] Logger name
   @param level [keyword] Log level
   @param message [string] Log message
   @param context [plist] Additional context key-value pairs
   "
  `(log-message-to ,logger-name ,level ,message ,@context))

;;; ------------------------------------------------------------------
;;; Purpose-Specific Logging API
;;; ------------------------------------------------------------------
(defmacro log.sql (message &rest context)
  "Log an SQL query to the :sql logger with :debug level.

   Used for logging database queries and operations.

   @param message [string] SQL query or database operation message
   @param context [plist] Additional context (e.g., :query-time, :rows-affected)
   "
  `(log-message-to :sql :debug ,message ,@context))

(defmacro log.web-access (message &rest context)
  "Log a web access to the :web-access logger with :info level.

   Used for logging HTTP requests and responses.

   @param message [string] Request summary message
   @param context [plist] Additional context (e.g., :method, :path, :status, :duration)
   "
  `(log-message-to :web-access :info ,message ,@context))

(defmacro log.audit (message &rest context)
  "Log an audit event to the :audit logger with :info level.

   Used for logging security-related events and user actions.

   @param message [string] Audit event message
   @param context [plist] Additional context (e.g., :user-id, :ip-address, :action)
   "
  `(log-message-to :audit :info ,message ,@context))

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
