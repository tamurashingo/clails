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
           #:logger-appender
           #:logger-level
           #:log-append
           #:log-message
           #:*log-context*
           #:with-log-context
           #:log.trace
           #:log.debug
           #:log.info
           #:log.warn
           #:log.error
           #:log.fatal))
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
  ((appender :initarg :appender :reader logger-appender)
   (level :initarg :level :reader logger-level))
  (:documentation "The logger class, holding appender and log level configuration."))

(defvar *logger* nil
  "The current logger instance.")

(defvar *log-context* nil
  "A dynamic context for log messages, managed with with-log-context.")

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

(defun log-message (level message &rest context)
  "Core logging function. Creates a log-record and passes it to the current logger's appender if the level is sufficient."
  (when *logger*
    (when (log-level>= level (logger-level *logger*))
      (let ((record (make-log-record :level level
                                     :message message
                                     :context (append context *log-context*))))
        (log-append (logger-appender *logger*) record)))))

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
