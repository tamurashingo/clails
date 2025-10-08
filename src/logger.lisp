(in-package #:cl-user)
(defpackage #:clails/logger
  (:use #:cl)
  (:import-from #:clails/logger/core
                #:<logger>
                #:*logger*
                #:*logger-registry*
                #:*default-logger-name*
                #:logger-name
                #:logger-appenders
                #:logger-appender
                #:logger-level
                #:logger-parent
                #:log-append-all
                #:log-message-to
                #:register-logger
                #:get-logger
                #:remove-logger
                #:with-log-context
                #:log.trace
                #:log.debug
                #:log.info
                #:log.warn
                #:log.error
                #:log.fatal
                #:log-to
                #:log.sql
                #:log.web-access
                #:log.audit
                #:set-logger-level
                #:add-appender
                #:clear-loggers)
  (:import-from #:clails/logger/appender
                #:make-console-appender
                #:<file-appender>
                #:make-file-appender
                #:close-appender)
  (:import-from #:clails/logger/fluentd-appender
                #:<fluentd-appender>
                #:make-fluentd-appender)
  (:import-from #:clails/logger/formatter
                #:<text-formatter>
                #:<json-formatter>)
  (:export #:<logger>
           #:*logger*
           #:*logger-registry*
           #:*default-logger-name*
           #:logger-name
           #:logger-appenders
           #:logger-appender
           #:logger-level
           #:logger-parent
           #:log-append-all
           #:log-message-to
           #:register-logger
           #:get-logger
           #:remove-logger
           #:with-log-context
           #:log.trace
           #:log.debug
           #:log.info
           #:log.warn
           #:log.error
           #:log.fatal
           #:log-to
           #:log.sql
           #:log.web-access
           #:log.audit
           #:set-logger-level
           #:add-appender
           #:clear-loggers
           #:make-console-appender
           #:<file-appender>
           #:make-file-appender
           #:close-appender
           #:<fluentd-appender>
           #:make-fluentd-appender
           #:<text-formatter>
           #:<json-formatter>
           #:setup-logger/development
           #:setup-logger/test
           #:setup-logger/production
           #:initialize-logging))
(in-package #:clails/logger)

;;; ------------------------------------------------------------------
;;; Environment-Specific Setup Functions
;;; ------------------------------------------------------------------
(defun setup-logger/development ()
  "Setup loggers for development environment.

   Development environment configuration:
   - Default logger: console output with text formatter, debug level
   - SQL logger: console output with text formatter, debug level
   - Web-access logger: console output with text formatter, info level
   - Audit logger: console output with text formatter, info level

   @return [null] nil
   "
  (clear-loggers)

  ;; Default logger: console with text format
  (register-logger
   :default
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :debug)

  ;; Set as current logger
  (setf *logger* (get-logger :default))

  ;; SQL logger: console with text format (debug for development)
  (register-logger
   :sql
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :debug)

  ;; Web-access logger: console with text format
  (register-logger
   :web-access
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :info)

  ;; Audit logger: console with text format
  (register-logger
   :audit
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :info)

  nil)

(defun setup-logger/test ()
  "Setup loggers for test environment.

   Test environment configuration:
   - Default logger: console output with text formatter, warn level
   - SQL logger: console output with text formatter, error level (suppress SQL logs in tests)
   - Web-access logger: console output with text formatter, warn level
   - Audit logger: console output with text formatter, info level

   @return [null] nil
   "
  (clear-loggers)

  ;; Default logger: console with text format, warn level for tests
  (register-logger
   :default
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :warn)

  ;; Set as current logger
  (setf *logger* (get-logger :default))

  ;; SQL logger: console with text format, error level (suppress in tests)
  (register-logger
   :sql
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :error)

  ;; Web-access logger: console with text format, warn level
  (register-logger
   :web-access
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :warn)

  ;; Audit logger: console with text format
  (register-logger
   :audit
   :appenders (list (make-console-appender
                     :formatter (make-instance '<text-formatter>)))
   :level :info)

  nil)

(defun setup-logger/production ()
  "Setup loggers for production environment.

   Production environment configuration:
   - Default logger: file output with JSON formatter, info level
   - SQL logger: file output with JSON formatter, warn level
   - Web-access logger: file output with JSON formatter, info level
   - Audit logger: file output with JSON formatter, info level

   Note: Requires appropriate directory permissions for log files.

   @return [null] nil
   "
  (clear-loggers)

  ;; Ensure logs directory exists
  (ensure-directories-exist "logs/")

  ;; Default logger: file with JSON format
  (register-logger
   :default
   :appenders (list (make-file-appender
                     :filepath "logs/application.log"
                     :formatter (make-instance '<json-formatter>)))
   :level :info)

  ;; Set as current logger
  (setf *logger* (get-logger :default))

  ;; SQL logger: separate file with JSON format (security separation)
  (register-logger
   :sql
   :appenders (list (make-file-appender
                     :filepath "logs/sql.log"
                     :formatter (make-instance '<json-formatter>)))
   :level :warn) ; Only warn and above in production

  ;; Web-access logger: separate file with JSON format
  (register-logger
   :web-access
   :appenders (list (make-file-appender
                     :filepath "logs/access.log"
                     :formatter (make-instance '<json-formatter>)))
   :level :info)

  ;; Audit logger: separate file with JSON format
  (register-logger
   :audit
   :appenders (list (make-file-appender
                     :filepath "logs/audit.log"
                     :formatter (make-instance '<json-formatter>)))
   :level :info)

  nil)

;;; ------------------------------------------------------------------
;;; Initialization
;;; ------------------------------------------------------------------
(defun initialize-logging (&optional environment)
  "Initialize the logging system according to the environment.

   If environment is not specified, it will be obtained from
   clails/environment:*project-environment*.

   Supported environments:
   - :develop, :development - Development environment
   - :test - Test environment
   - :production - Production environment

   @param environment [keyword] Environment keyword (optional)
   @return [null] nil
   @condition error Unknown environment specified
   "
  (let ((env (or environment
                 (and (find-package :clails/environment)
                      (symbol-value (find-symbol "*PROJECT-ENVIRONMENT*" :clails/environment)))
                 :develop)))
    (case env
      ((:develop :development) (setup-logger/development))
      (:test (setup-logger/test))
      (:production (setup-logger/production))
      (t (error "Unknown environment: ~A" env)))))

