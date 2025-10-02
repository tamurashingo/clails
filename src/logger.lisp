(in-package #:cl-user)
(defpackage #:clails/logger
  (:use #:cl)
  (:import-from #:clails/logger/core
                #:<logger>
                #:*logger*
                #:logger-appender
                #:logger-level
                #:with-log-context
                #:log.trace
                #:log.debug
                #:log.info
                #:log.warn
                #:log.error
                #:log.fatal)
  (:import-from #:clails/logger/appender
                #:make-console-appender)
  (:import-from #:clails/logger/formatter
                #:<text-formatter>
                #:<json-formatter>)
  (:export #:<logger>
           #:*logger*
           #:logger-appender
           #:logger-level
           #:with-log-context
           #:log.trace
           #:log.debug
           #:log.info
           #:log.warn
           #:log.error
           #:log.fatal
           #:make-console-appender
           #:<text-formatter>
           #:<json-formatter>))
(in-package #:clails/logger)
