(defpackage #:clails/test/logger/registry
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:register-logger
                #:get-logger
                #:remove-logger
                #:clear-loggers
                #:logger-name
                #:logger-level
                #:make-console-appender
                #:<text-formatter>))
(in-package #:clails/test/logger/registry)

(deftest logger-registry-tests
  (testing "Register and get logger"
    (clear-loggers)
    (let* ((appender (make-console-appender :formatter (make-instance '<text-formatter>)))
           (logger (register-logger :test-logger :appender appender :level :debug)))
      (ok logger "Logger is created")
      (ok (eq (logger-name logger) :test-logger) "Logger name is correct")
      (ok (eq (logger-level logger) :debug) "Logger level is correct")
      (ok (= (length (clails/logger/core::logger-appenders logger)) 1) "Logger has one appender")
      
      (let ((retrieved (get-logger :test-logger)))
        (ok retrieved "Logger can be retrieved")
        (ok (eq logger retrieved) "Retrieved logger is the same instance"))))
  
  (testing "Register logger with multiple appenders"
    (clear-loggers)
    (let* ((appender1 (make-console-appender :formatter (make-instance '<text-formatter>)))
           (appender2 (make-console-appender :formatter (make-instance '<text-formatter>)))
           (logger (register-logger :multi-logger 
                                    :appenders (list appender1 appender2)
                                    :level :info)))
      (ok (= (length (clails/logger/core::logger-appenders logger)) 2) "Logger has two appenders")))
  
  (testing "Get root logger fallback"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :root :appender appender :level :info)
      (let ((logger (get-logger :non-existent)))
        (ok logger "Returns root logger for non-existent logger")
        (ok (eq (logger-name logger) :root) "Returns the :root logger"))))
  
  (testing "Remove logger"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :temp-logger :appender appender :level :info)
      (ok (get-logger :temp-logger) "Logger exists before removal")
      (remove-logger :temp-logger)
      ;; After removal, should return nil if no default logger
      (ok (not (clails/logger/core::gethash :temp-logger clails/logger/core::*logger-registry*))
          "Logger is removed from registry")))
  
  (testing "Clear loggers"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :logger1 :appender appender :level :info)
      (register-logger :logger2 :appender appender :level :debug)
      (clear-loggers)
      (ok (zerop (hash-table-count clails/logger/core::*logger-registry*))
          "All loggers are cleared"))))
