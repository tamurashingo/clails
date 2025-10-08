(defpackage #:clails/test/logger/environment
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:setup-logger/development
                #:setup-logger/test
                #:setup-logger/production
                #:initialize-logging
                #:get-logger
                #:logger-level
                #:logger-appenders))
(in-package #:clails/test/logger/environment)

(defvar *test-log-dir* nil)

(setup
  (uiop:setup-temporary-directory)
  (setf *test-log-dir* (merge-pathnames "clails-test-env-logs/" uiop:*temporary-directory*))
  (ensure-directories-exist *test-log-dir*))

(teardown
  ;; Clean up test files
  (dolist (file (directory (merge-pathnames "*.log" *test-log-dir*)))
    (when (probe-file file)
      (delete-file file))))

(deftest environment-setup-tests
  (testing "Development environment setup"
    (setup-logger/development)
    (let ((default-logger (get-logger :default))
          (sql-logger (get-logger :sql))
          (web-logger (get-logger :web-access))
          (audit-logger (get-logger :audit)))
      (ok default-logger "Default logger exists")
      (ok (eq (logger-level default-logger) :debug) "Default logger is DEBUG level")
      (ok sql-logger "SQL logger exists")
      (ok (eq (logger-level sql-logger) :debug) "SQL logger is DEBUG level in development")
      (ok web-logger "Web-access logger exists")
      (ok (eq (logger-level web-logger) :info) "Web-access logger is INFO level")
      (ok audit-logger "Audit logger exists")
      (ok (eq (logger-level audit-logger) :info) "Audit logger is INFO level")))
  
  (testing "Test environment setup"
    (setup-logger/test)
    (let ((default-logger (get-logger :default))
          (sql-logger (get-logger :sql)))
      (ok default-logger "Default logger exists in test")
      (ok (eq (logger-level default-logger) :warn) "Default logger is WARN level in test")
      (ok sql-logger "SQL logger exists in test")
      (ok (eq (logger-level sql-logger) :error) "SQL logger is ERROR level in test")))
  
  (testing "Production environment setup"
    (setup-logger/production)
    (let ((default-logger (get-logger :default))
          (sql-logger (get-logger :sql))
          (web-logger (get-logger :web-access))
          (audit-logger (get-logger :audit)))
      (ok default-logger "Default logger exists in production")
      (ok (eq (logger-level default-logger) :info) "Default logger is INFO level in production")
      (ok sql-logger "SQL logger exists in production")
      (ok (eq (logger-level sql-logger) :warn) "SQL logger is WARN level in production")
      (ok web-logger "Web-access logger exists in production")
      (ok audit-logger "Audit logger exists in production")
      
      ;; Check that appenders are file appenders
      (let ((appender (first (logger-appenders default-logger))))
        (ok (typep appender 'clails/logger/appender::<file-appender>)
            "Production uses file appender"))))
  
  (testing "Initialize logging with explicit environment"
    (initialize-logging :development)
    (ok (eq (logger-level (get-logger :default)) :debug) "Initialized to development")
    
    (initialize-logging :test)
    (ok (eq (logger-level (get-logger :default)) :warn) "Switched to test")
    
    (initialize-logging :production)
    (ok (eq (logger-level (get-logger :default)) :info) "Switched to production"))
  
  (testing "Initialize logging without argument"
    (initialize-logging)
    (ok (get-logger :default) "Default initialization succeeds")))
