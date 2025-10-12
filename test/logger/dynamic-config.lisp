(defpackage #:clails/test/logger/dynamic-config
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:register-logger
                #:get-logger
                #:clear-loggers
                #:set-logger-level
                #:add-appender
                #:logger-level
                #:make-console-appender
                #:<text-formatter>
                #:<json-formatter>))
(in-package #:clails/test/logger/dynamic-config)

(deftest dynamic-configuration-tests
  (testing "Set logger level"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test-logger :appender appender :level :debug)
      (ok (eq (logger-level (get-logger :test-logger)) :debug) "Initial level is DEBUG")
      
      (set-logger-level :test-logger :warn)
      (ok (eq (logger-level (get-logger :test-logger)) :warn) "Level changed to WARN")
      
      (set-logger-level :test-logger :error)
      (ok (eq (logger-level (get-logger :test-logger)) :error) "Level changed to ERROR")))
  
  (testing "Set logger level on non-existent logger"
    (clear-loggers)
    (let ((result (set-logger-level :non-existent :info)))
      (ok (not result) "Returns nil for non-existent logger")))
  
  (testing "Add appender to logger"
    (clear-loggers)
    (let ((appender1 (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test-logger :appender appender1 :level :info)
      (ok (= (length (clails/logger/core::logger-appenders (get-logger :test-logger))) 1)
          "Logger starts with one appender")
      
      (let ((appender2 (make-console-appender :formatter (make-instance '<json-formatter>))))
        (add-appender :test-logger appender2)
        (ok (= (length (clails/logger/core::logger-appenders (get-logger :test-logger))) 2)
            "Logger now has two appenders"))))
  
  (testing "Add appender to non-existent logger"
    (clear-loggers)
    (let* ((appender (make-console-appender :formatter (make-instance '<text-formatter>)))
           (result (add-appender :non-existent appender)))
      (ok (not result) "Returns nil for non-existent logger")))
  
  (testing "Dynamic configuration does not duplicate appenders"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test-logger :appender appender :level :info)
      (add-appender :test-logger appender)
      (ok (= (length (clails/logger/core::logger-appenders (get-logger :test-logger))) 1)
          "Duplicate appender is not added"))))
