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
           #:make-console-appender))
(in-package #:clails/logger/appender)

(defclass <appender> () ()
  (:documentation "Base class for log appenders, which are responsible for writing log records to a destination."))

(defmethod log-append ((appender <appender>) (record <log-record>))
   (declare (ignore appender record))
   (error "log-append is not implemented for this appender type."))

(defclass <stream-appender> (<appender>)
  ((stream :initarg :stream :reader stream-appender-stream)
   (formatter :initarg :formatter :reader stream-appender-formatter))
  (:documentation "Appender that writes log records to a stream, using a specified formatter."))

(defmethod log-append ((appender <stream-appender>) (record <log-record>))
  (format-record (stream-appender-formatter appender)
                 (stream-appender-stream appender)
                 record)
  (finish-output (stream-appender-stream appender)))

(defun make-console-appender (&key formatter)
  "Creates an appender that writes to *standard-output*."
  (make-instance '<stream-appender>
                 :stream *standard-output*
                 :formatter formatter))
