(defpackage #:clails/logger/appender
  (:use #:cl)
  (:import-from #:clails/logger/core
                #:<log-record>
                #:log-append)
  (:import-from #:clails/logger/formatter
                #:formatter
                #:format-record)
  (:export #:<appender>
           #:<stream-appender>
           #:make-console-appender
           #:<file-appender>
           #:make-file-appender
           #:close-appender))
(in-package #:clails/logger/appender)

(defclass <appender> () ()
  (:documentation "Base class for log appenders, which are responsible for writing log records to a destination."))

(defmethod log-append ((appender <appender>) (record <log-record>))
   (declare (ignore appender record))
   (error "log-append is not implemented for this appender type."))

(defclass <stream-appender> (<appender>)
  ((stream :initarg :stream :reader stream-appender-stream)
   (formatter :initarg :formatter :reader stream-appender-formatter)
   (use-dynamic-stream :initarg :use-dynamic-stream
                       :initform nil
                       :reader stream-appender-use-dynamic-stream
                       :documentation "If true, use *standard-output* at log time instead of the stored stream."))
  (:documentation "Appender that writes log records to a stream, using a specified formatter."))

(defmethod log-append ((appender <stream-appender>) (record <log-record>))
  (let ((target-stream (if (stream-appender-use-dynamic-stream appender)
                           *standard-output*
                           (stream-appender-stream appender))))
    (format-record (stream-appender-formatter appender)
                   target-stream
                   record)
    (finish-output target-stream)))

(defun make-console-appender (&key formatter)
  "Creates an appender that writes to *standard-output*.
   
   This appender uses the dynamic value of *standard-output* at log time,
   making it thread-safe and compatible with bordeaux-threads.
   
   @param formatter [<formatter>] Formatter to use for log records
   @return [<stream-appender>] Console appender instance
   "
  (make-instance '<stream-appender>
                 :stream *standard-output*
                 :formatter formatter
                 :use-dynamic-stream t))

;;; ------------------------------------------------------------------
;;; File Appender
;;; ------------------------------------------------------------------
(defclass <file-appender> (<appender>)
  ((filepath
    :initarg :filepath
    :reader file-appender-filepath
    :documentation "Path to the log file.")
   (formatter
    :initarg :formatter
    :reader file-appender-formatter
    :documentation "Formatter used to format log records.")
   (stream
    :accessor file-appender-stream
    :initform nil
    :documentation "File stream for writing logs."))
  (:documentation "Appender that writes log records to a file."))

(defmethod log-append ((appender <file-appender>) (record <log-record>))
  "Append a log record to the file.

   Opens the file stream if not already open, formats the record, and writes it to the file.

   @param appender [<file-appender>] File appender instance
   @param record [<log-record>] Log record to append
   @return [null] nil
   "
  ;; Open stream if not already open
  (unless (file-appender-stream appender)
    (setf (file-appender-stream appender)
          (open (file-appender-filepath appender)
                :direction :output
                :if-exists :append
                :if-does-not-exist :create)))

  ;; Format and write the record
  (format-record (file-appender-formatter appender)
                 (file-appender-stream appender)
                 record)
  (finish-output (file-appender-stream appender)))

;;; ------------------------------------------------------------------
;;; Appender Cleanup
;;; ------------------------------------------------------------------
(defgeneric close-appender (appender)
  (:documentation "Close the appender and release any resources.

   @param appender [<appender>] Appender instance to close
   @return [null] nil
   "))

(defmethod close-appender ((appender <appender>))
  "Default implementation does nothing.

   @param appender [<appender>] Appender instance
   @return [null] nil
   "
  (declare (ignore appender))
  nil)

(defmethod close-appender ((appender <stream-appender>))
  "Close stream appender (does not close standard streams).

   @param appender [<stream-appender>] Stream appender instance
   @return [null] nil
   "
  ;; Don't close standard streams
  (declare (ignore appender))
  nil)

(defmethod close-appender ((appender <file-appender>))
  "Close file appender and its underlying file stream.

   @param appender [<file-appender>] File appender instance
   @return [null] nil
   "
  (when (file-appender-stream appender)
    (close (file-appender-stream appender))
    (setf (file-appender-stream appender) nil)))

;;; ------------------------------------------------------------------
;;; Helper Functions
;;; ------------------------------------------------------------------
(defun make-file-appender (&key filepath formatter)
  "Create a file appender that writes to the specified file.

   The file is opened in append mode and created if it does not exist.

   @param filepath [pathname] Path to the log file
   @param filepath [string] Path to the log file
   @param formatter [<formatter>] Formatter to use for log records
   @return [<file-appender>] File appender instance
   "
  (make-instance '<file-appender>
                 :filepath filepath
                 :formatter formatter))
