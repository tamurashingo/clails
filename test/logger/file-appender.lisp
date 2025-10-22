(defpackage #:clails/test/logger/file-appender
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:register-logger
                #:clear-loggers
                #:log-to
                #:make-file-appender
                #:close-appender
                #:<text-formatter>
                #:<json-formatter>))
(in-package #:clails/test/logger/file-appender)

(defvar *test-log-dir* nil)

(setup
 (uiop:setup-temporary-directory)
 (setf *test-log-dir* (merge-pathnames "clails-test-logs/" uiop:*temporary-directory*))
 (ensure-directories-exist *test-log-dir*))


(teardown
  ;; Clean up test files
  (dolist (file (directory (merge-pathnames "*.log" *test-log-dir*)))
    (when (probe-file file)
      (delete-file file))))

(deftest file-appender-tests
  (testing "Basic file appender"
    (let* ((log-file (merge-pathnames "test-basic.log" *test-log-dir*))
           (appender (make-file-appender :filepath log-file
                                         :formatter (make-instance '<text-formatter>))))
      (clear-loggers)
      (register-logger :file-test :appender appender :level :info)
      
      (log-to :file-test :info "Test message")
      (close-appender appender)
      
      (ok (probe-file log-file) "Log file is created")
      (let ((content (uiop:read-file-string log-file)))
        (ok (str:contains? "Test message" content) "Message is written to file")
        (ok (str:contains? "INFO" content) "Log level is in file"))))
  
  (testing "File appender with append mode"
    (let* ((log-file (merge-pathnames "test-append.log" *test-log-dir*))
           (appender1 (make-file-appender :filepath log-file
                                          :formatter (make-instance '<text-formatter>))))
      (clear-loggers)
      (register-logger :append-test1 :appender appender1 :level :info)
      (log-to :append-test1 :info "First message")
      (close-appender appender1)
      
      ;; Second write
      (let ((appender2 (make-file-appender :filepath log-file
                                           :formatter (make-instance '<text-formatter>))))
        (clear-loggers)
        (register-logger :append-test2 :appender appender2 :level :info)
        (log-to :append-test2 :info "Second message")
        (close-appender appender2))
      
      (let ((content (uiop:read-file-string log-file)))
        (ok (str:contains? "First message" content) "First message is in file")
        (ok (str:contains? "Second message" content) "Second message is appended"))))
  
  (testing "File appender with JSON formatter"
    (let* ((log-file (merge-pathnames "test-json.log" *test-log-dir*))
           (appender (make-file-appender :filepath log-file
                                         :formatter (make-instance '<json-formatter>))))
      (clear-loggers)
      (register-logger :json-test :appender appender :level :debug)
      
      (log-to :json-test :info "JSON message" :key1 "value1" :key2 42)
      (close-appender appender)
      
      (ok (probe-file log-file) "JSON log file is created")
      (let ((content (uiop:read-file-string log-file)))
        (ok (str:contains? "JSON message" content) "JSON message is in file")
        (ok (str:contains? "\"KEY1\":\"value1\"" content) "JSON context is in file"))))
  
  (testing "Close appender idempotency"
    (let* ((log-file (merge-pathnames "test-close.log" *test-log-dir*))
           (appender (make-file-appender :filepath log-file
                                         :formatter (make-instance '<text-formatter>))))
      (close-appender appender)
      (ok t "First close succeeds")
      (close-appender appender)
      (ok t "Second close does not error"))))
