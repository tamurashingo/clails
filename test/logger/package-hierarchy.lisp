(defpackage #:clails/test/logger/package-hierarchy
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:register-logger
                #:get-logger
                #:clear-loggers
                #:log-package.debug
                #:log-package.info
                #:make-console-appender
                #:<text-formatter>))
(in-package #:clails/test/logger/package-hierarchy)

(deftest package-to-logger-name-tests
  (testing "Convert keyword to logger name"
    (ok (eq (clails/logger/core::package-to-logger-name :clails/model)
            :CLAILS/MODEL)
        "Keyword is normalized to uppercase"))
  
  (testing "Convert string to logger name"
    (ok (eq (clails/logger/core::package-to-logger-name "clails/model")
            :CLAILS/MODEL)
        "String is converted to uppercase keyword"))
  
  (testing "Convert symbol to logger name"
    (ok (eq (clails/logger/core::package-to-logger-name 'clails/controller)
            :CLAILS/CONTROLLER)
        "Symbol is converted to uppercase keyword"))
  
  (testing "Convert package to logger name"
    (ok (eq (clails/logger/core::package-to-logger-name (find-package :cl))
            :COMMON-LISP)
        "Package object is converted correctly")))

(deftest get-parent-logger-name-tests
  (testing "Get parent of hierarchical logger"
    (ok (eq (clails/logger/core::get-parent-logger-name :CLAILS/MODEL/QUERY)
            :CLAILS/MODEL)
        "Returns parent of three-level hierarchy"))
  
  (testing "Get parent of two-level logger"
    (ok (eq (clails/logger/core::get-parent-logger-name :CLAILS/MODEL)
            :CLAILS)
        "Returns parent of two-level hierarchy"))
  
  (testing "Get parent of top-level logger"
    (ok (eq (clails/logger/core::get-parent-logger-name :CLAILS)
            :ROOT)
        "Returns :ROOT for top-level logger"))
  
  (testing "Get parent of root logger"
    (ok (null (clails/logger/core::get-parent-logger-name :ROOT))
        "Returns nil for :ROOT logger")))

(deftest find-logger-in-hierarchy-tests
  (testing "Find exact match logger"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :clails/model :appender appender :level :info)
      (let ((logger (clails/logger/core::find-logger-in-hierarchy :CLAILS/MODEL)))
        (ok logger "Finds exact match")
        (ok (eq (clails/logger/core::logger-name logger) :CLAILS/MODEL)
            "Returns correct logger"))))
  
  (testing "Find parent logger when exact match not found"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :clails :appender appender :level :warn)
      (let ((logger (clails/logger/core::find-logger-in-hierarchy :CLAILS/MODEL/QUERY)))
        (ok logger "Finds parent logger")
        (ok (eq (clails/logger/core::logger-name logger) :CLAILS)
            "Returns parent logger"))))
  
  (testing "Hierarchical search through multiple levels"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :root :appender appender :level :error)
      (let ((logger (clails/logger/core::find-logger-in-hierarchy :CLAILS/MODEL/QUERY/SQLITE3)))
        (ok logger "Finds root logger after searching hierarchy")
        (ok (eq (clails/logger/core::logger-name logger) :ROOT)
            "Returns :ROOT logger"))))
  
  (testing "Returns nil when no logger found"
    (clear-loggers)
    (let ((logger (clails/logger/core::find-logger-in-hierarchy :NON-EXISTENT/LOGGER)))
      (ok (null logger) "Returns nil when no logger in hierarchy"))))

(deftest hierarchical-logger-registration-tests
  (testing "Auto-determine parent logger"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :clails :appender appender :level :info)
      (register-logger :clails/model :appender appender :level :debug)
      
      (let* ((child-logger (get-logger :clails/model))
             (parent-logger (clails/logger/core::logger-parent child-logger)))
        (ok parent-logger "Child logger has parent")
        (ok (eq (clails/logger/core::logger-name parent-logger) :CLAILS)
            "Parent is correctly determined"))))
  
  (testing "Explicit parent overrides auto-determination"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :custom-parent :appender appender :level :info)
      (register-logger :clails/model
                       :appender appender
                       :level :debug
                       :parent (get-logger :custom-parent))
      
      (let* ((child-logger (get-logger :clails/model))
             (parent-logger (clails/logger/core::logger-parent child-logger)))
        (ok parent-logger "Child logger has parent")
        (ok (eq (clails/logger/core::logger-name parent-logger) :CUSTOM-PARENT)
            "Explicit parent is used")))))

(deftest get-logger-hierarchy-tests
  (testing "Get logger with hierarchy search"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      ;; Only register :clails
      (register-logger :clails :appender appender :level :warn)
      
      ;; Get :clails/model should find :clails
      (let ((logger (get-logger :clails/model)))
        (ok logger "Finds parent logger")
        (ok (eq (clails/logger/core::logger-name logger) :CLAILS)
            "Returns :clails logger"))))
  
  (testing "Get logger with string package name"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :my-app :appender appender :level :info)
      (let ((logger (get-logger "my-app/controller")))
        (ok logger "Finds logger from string")
        (ok (eq (clails/logger/core::logger-name logger) :MY-APP)
            "Returns correct parent")))))

(deftest log-for-package-tests
  (testing "Log with package-based logger"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender
                                     :formatter (make-instance '<text-formatter>))))
                      (register-logger :clails/test :appender appender :level :debug)
                      (clails/logger/core::log-for-package :clails/test :info "Test message" :key "value")))))
      (ok (str:contains? "Test message" output) "Message is logged")
      (ok (str:contains? "key=\"value\"" output) "Context is logged")))
  
  (testing "Log with hierarchical package search"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender
                                     :formatter (make-instance '<text-formatter>))))
                      (register-logger :clails :appender appender :level :info)
                      ;; Log to :clails/test/logger - should find :clails
                      (clails/logger/core::log-for-package :clails/test/logger :info "Hierarchical message")))))
      (ok (str:contains? "Hierarchical message" output)
          "Message is logged via parent logger")))
  
  (testing "Log filtering by level"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender
                                     :formatter (make-instance '<text-formatter>))))
                      (register-logger :clails :appender appender :level :warn)
                      (clails/logger/core::log-for-package :clails/model :debug "Debug message")
                      (clails/logger/core::log-for-package :clails/model :warn "Warning message")))))
      (ok (not (str:contains? "Debug message" output))
          "DEBUG message is filtered")
      (ok (str:contains? "Warning message" output)
          "WARN message is logged"))))

(deftest log-package-macros-tests
  (testing "log-package.info macro"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender
                                     :formatter (make-instance '<text-formatter>))))
                      (register-logger :clails/test/logger/package-hierarchy
                                       :appender appender
                                       :level :debug)
                      (log-package.info "Package info message")))))
      (ok (str:contains? "Package info message" output)
          "log-package.info works")))
  
  (testing "log-package.debug macro with hierarchy"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender
                                     :formatter (make-instance '<text-formatter>))))
                      ;; Only register parent
                      (register-logger :clails/test :appender appender :level :debug)
                      (log-package.debug "Package debug message")))))
      (ok (str:contains? "Package debug message" output)
          "log-package.debug finds parent logger"))))
