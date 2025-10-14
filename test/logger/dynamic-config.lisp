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
                #:log-level-enabled-p
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

(deftest log-level-enabled-p-tests
  (testing "Check log level enabled for specific logger"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test-logger :appender appender :level :info)

      (ok (not (log-level-enabled-p :trace :test-logger)) "TRACE is not enabled when level is INFO")
      (ok (not (log-level-enabled-p :debug :test-logger)) "DEBUG is not enabled when level is INFO")
      (ok (log-level-enabled-p :info :test-logger) "INFO is enabled when level is INFO")
      (ok (log-level-enabled-p :warn :test-logger) "WARN is enabled when level is INFO")
      (ok (log-level-enabled-p :error :test-logger) "ERROR is enabled when level is INFO")
      (ok (log-level-enabled-p :fatal :test-logger) "FATAL is enabled when level is INFO")))

  (testing "Check log level after changing logger level"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test-logger :appender appender :level :info)

      (ok (not (log-level-enabled-p :debug :test-logger)) "DEBUG is not enabled initially")

      (set-logger-level :test-logger :debug)
      (ok (log-level-enabled-p :debug :test-logger) "DEBUG is enabled after level change")
      (ok (log-level-enabled-p :info :test-logger) "INFO is still enabled after level change")))

  (testing "Check log level for non-existent logger"
    (clear-loggers)
    (ok (not (log-level-enabled-p :info :non-existent-logger))
        "Returns nil for non-existent logger"))

  (testing "Check log level with package name"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :clails/test/logger/dynamic-config :appender appender :level :warn)

      (ok (not (log-level-enabled-p :info *package*)) "INFO is not enabled when level is WARN")
      (ok (log-level-enabled-p :warn *package*) "WARN is enabled when level is WARN")
      (ok (log-level-enabled-p :error *package*) "ERROR is enabled when level is WARN")))

  (testing "Check log level with hierarchical logger"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :root :appender appender :level :info)
      (register-logger :clails :level :debug)

      (ok (log-level-enabled-p :debug :clails) "DEBUG is enabled for :clails logger")
      (ok (log-level-enabled-p :debug :clails/test) "DEBUG is enabled for child logger :clails/test (inherits from :clails)")
      (ok (log-level-enabled-p :info :root) "INFO is enabled for :root logger"))))
