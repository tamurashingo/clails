(defpackage clails/test/logger/logger
  (:use #:cl
        #:rove
        #:jonathan)
  (:import-from #:clails/logger
                #:log.debug
                #:log.info
                #:log.warn
                #:*logger*
                #:with-log-context
                #:<text-formatter>
                #:<json-formatter>
                #:make-console-appender
                #:<logger>))
(in-package :clails/test/logger/logger)

(deftest logger-tests
  (testing "Text Formatter"
    (let* ((formatter (make-instance '<text-formatter>))
           (output (with-output-to-string (stream)
                     (let ((record (clails/logger/core::make-log-record
                                    :level :info
                                    :message "Test message"
                                    :context '(:user-id 123))))
                       (clails/logger/formatter::format-record formatter stream record)))))
      (ok (str:starts-with-p "[" output) "Starts with timestamp bracket")
      (ok (str:contains? "] INFO: Test message" output) "Contains level and message")
      (ok (str:ends-with-p (format nil "user-id=123~%") output) "Ends with context")))

  (testing "JSON Formatter"
    (let* ((formatter (make-instance '<json-formatter>))
           (output (with-output-to-string (stream)
                     (let ((record (clails/logger/core::make-log-record
                                    :level :warn
                                    :message "Another test"
                                    :context '(:request-id "xyz"))))
                       (clails/logger/formatter::format-record formatter stream record))))
           (json (parse output)))
      (ok (getf json :timestamp) "JSON has timestamp")
      (ok (string= (getf json :level) "WARN") "JSON level is correct")
      (ok (string= (getf json :message) "Another test") "JSON message is correct")
      (ok (string= (getf json :request-id) "xyz") "JSON context is correct")))

  (testing "Logger with context and level filtering"
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((*logger* (make-instance '<logger>
                                                   :level :info
                                                   :appenders (list (make-console-appender
                                                                     :formatter (make-instance '<text-formatter>))))))
                      (log.debug "This should not be printed.")
                      (with-log-context (:session-id "abc")
                        (log.info "Info message with context.")
                        (log.warn "Warning message." :extra "data"))))))
      (ok (not (str:contains? "This should not be printed." output)) "DEBUG message is filtered")
      (ok (str:contains? "Info message with context." output) "INFO message is present")
      (ok (str:contains? "session-id=\"abc\"" output) "Context is present in INFO message")
      (ok (str:contains? "Warning message." output) "WARN message is present")
      (ok (str:contains? "extra=\"data\"" output) "Additional context is present in WARN message")
      (let ((warning-line (find "Warning message" (str:lines output) :test #'str:contains?)))
        (ok (str:contains? "session-id=\"abc\"" warning-line) "Context is also present in WARN message")))))
