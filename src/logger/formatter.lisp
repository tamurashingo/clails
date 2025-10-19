(in-package #:cl-user)
(defpackage #:clails/logger/formatter
  (:use #:cl)
  (:import-from #:clails/logger/core
                #:<log-record>
                #:log-record-timestamp
                #:log-record-level
                #:log-record-message
                #:log-record-context)
  (:import-from #:local-time
                #:format-timestring
                #:+rfc3339-format+)
  (:import-from #:jonathan
                #:to-json)
  (:export #:<formatter>
           #:format-record
           #:<text-formatter>
           #:<json-formatter>))
(in-package #:clails/logger/formatter)

(defclass <formatter> () ()
  (:documentation "Base class for log formatters, which are responsible for converting a log record into a string."))

(defgeneric format-record (formatter stream record)
  (:documentation "Formats the log record and writes it to the stream.")
  (:method ((formatter <formatter>) stream (record <log-record>))
   (declare (ignore formatter stream record))
   (error "format-record is not implemented for this formatter type.")))

(defclass <text-formatter> (<formatter>) ()
  (:documentation "Formats log records into a human-readable text format."))

(defmethod format-record ((formatter <text-formatter>) stream record)
  "Format log record as human-readable text.
   
   Outputs format: [timestamp] LEVEL: message key1=value1 key2=value2
   
   @param formatter [<text-formatter>] Text formatter instance
   @param stream [stream] Output stream
   @param record [<log-record>] Log record to format
   "
  (format stream "[~A] ~A: ~A~{ ~A=~S~}~%"
          (format-timestring nil (log-record-timestamp record) :format +rfc3339-format+)
          (string-upcase (log-record-level record))
          (log-record-message record)
          (loop for (key val) on (log-record-context record) by #'cddr
                collect (string-downcase (symbol-name key))
                collect val)))

(defclass <json-formatter> (<formatter>) ()
  (:documentation "Formats log records into a JSON format."))

(defmethod format-record ((formatter <json-formatter>) stream record)
  "Format log record as JSON.
   
   Outputs a single-line JSON object with timestamp, level, message, and context fields.
   
   @param formatter [<json-formatter>] JSON formatter instance
   @param stream [stream] Output stream
   @param record [<log-record>] Log record to format
   "
  (let ((payload (list :timestamp (format-timestring nil (log-record-timestamp record) :format +rfc3339-format+)
                       :level (log-record-level record)
                       :message (log-record-message record))))
    (loop for (key val) on (log-record-context record) by #'cddr
          do (setf (getf payload key) val))
    (write-line (to-json payload) stream)))
