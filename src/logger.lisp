(in-package #:cl-user)
(defpackage #:clails/logger
  (:use #:cl)
  (:import-from #:clails/logger/core
                #:<logger>
                #:*logger-registry*
                #:logger-name
                #:logger-level
                #:log-message-to
                #:register-logger
                #:get-logger
                #:remove-logger
                #:with-log-context
                #:log-to
                #:log.sql
                #:log.web-access
                #:log.audit
                #:set-logger-level
                #:add-appender
                #:clear-loggers
                #:log-package.trace
                #:log-package.debug
                #:log-package.info
                #:log-package.warn
                #:log-package.error
                #:log-package.fatal)
  (:import-from #:clails/logger/appender
                #:make-console-appender
                #:<file-appender>
                #:make-file-appender
                #:close-appender)
  (:import-from #:clails/logger/formatter
                #:<text-formatter>
                #:<json-formatter>)
  (:export #:<logger>
           #:*logger-registry*
           #:logger-name
           #:logger-level
           #:log-message-to
           #:register-logger
           #:get-logger
           #:remove-logger
           #:with-log-context
           #:log-to
           #:log.sql
           #:log.web-access
           #:log.audit
           #:set-logger-level
           #:add-appender
           #:clear-loggers
           #:log-package.trace
           #:log-package.debug
           #:log-package.info
           #:log-package.warn
           #:log-package.error
           #:log-package.fatal
           #:make-console-appender
           #:<file-appender>
           #:make-file-appender
           #:close-appender
           #:<text-formatter>
           #:<json-formatter>))
(in-package #:clails/logger)

