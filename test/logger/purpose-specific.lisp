(defpackage #:clails/test/logger/purpose-specific
  (:use #:cl
        #:rove)
  (:import-from #:clails/logger
                #:register-logger
                #:clear-loggers
                #:log.sql
                #:log.web-access
                #:log.audit
                #:make-console-appender
                #:<text-formatter>))
(in-package #:clails/test/logger/purpose-specific)

(deftest purpose-specific-logging-tests
  (testing "SQL logging"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
                      (register-logger :sql :appender appender :level :debug)
                      (log.sql "SELECT * FROM users WHERE id = ?" :id 123)))))
      (ok (str:contains? "SELECT * FROM users" output) "SQL query is logged")
      (ok (str:contains? "id=123" output) "SQL parameters are logged")
      (ok (str:contains? "DEBUG" output) "SQL logs at DEBUG level")))
  
  (testing "Web access logging"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
                      (register-logger :web-access :appender appender :level :info)
                      (log.web-access "GET /api/users" 
                                      :method "GET" 
                                      :path "/api/users"
                                      :status 200
                                      :duration 45)))))
      (ok (str:contains? "GET /api/users" output) "Request is logged")
      (ok (str:contains? "status=200" output) "Status is logged")
      (ok (str:contains? "duration=45" output) "Duration is logged")
      (ok (str:contains? "INFO" output) "Web access logs at INFO level")))
  
  (testing "Audit logging"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
                      (register-logger :audit :appender appender :level :info)
                      (log.audit "User login" 
                                 :user-id 456
                                 :ip-address "192.168.1.1"
                                 :action "login")))))
      (ok (str:contains? "User login" output) "Audit event is logged")
      (ok (str:contains? "user-id=456" output) "User ID is logged")
      (ok (str:contains? "ip-address=\"192.168.1.1\"" output) "IP address is logged")
      (ok (str:contains? "INFO" output) "Audit logs at INFO level"))))
