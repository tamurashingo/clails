# clails Logging Guide

## Overview

clails Logging provides a flexible logging system.
It manages log output across your entire application in a unified way, supporting automatic logger selection based on package hierarchy, simultaneous output to multiple destinations, and customizable formatting.

## Basic Concepts

- **Logger**: Receives log messages and determines whether to output them based on the configured log level
- **Appender**: Defines log output destinations (console, file, etc.)
- **Formatter**: Defines the output format for log messages (text, JSON)
- **Log Level**: Represents the importance of a log message (TRACE < DEBUG < INFO < WARN < ERROR < FATAL)
- **Logger Hierarchy**: Automatically selects loggers based on package hierarchy

---

## 1. Log Levels

clails supports the following log levels:

| Level | Description | Use Case |
|-------|-------------|----------|
| `:trace` | Most detailed information | Detailed trace information for debugging |
| `:debug` | Debug information | Debug information during development |
| `:info` | General information | Recording normal application operations |
| `:warn` | Warning | Warnings about potential issues |
| `:error` | Error | An error occurred but processing can continue |
| `:fatal` | Fatal error | Critical errors that prevent application execution |
| `:none` | No log output | Suppress all logs |

---

## 2. Logger Configuration

### 2.1. Registering a Logger

Use the `register-logger` function to register a logger.

```common-lisp
(use-package :clails/logger)

;; Register a logger that outputs to console
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; Register a logger that outputs to file
(register-logger :sql
                 :level :debug
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/sql.log"
                            :formatter (make-instance '<json-formatter>)))
```

### 2.2. Hierarchical Loggers

clails automatically selects loggers based on package hierarchy.

```common-lisp
;; Root logger (default for all logs)
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; Logger for CLAILS package
(register-logger :clails
                 :level :debug
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; Logger for CLAILS/MODEL package
(register-logger :clails/model
                 :level :trace
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/model.log"
                            :formatter (make-instance '<json-formatter>)))
```

If a logger is not found, it automatically searches for parent loggers:

- `:clails/model/query` → `:clails/model` → `:clails` → `:root`

### 2.3. Getting a Logger

Use the `get-logger` function to retrieve a logger.

```common-lisp
;; Get logger by name
(get-logger :root)       ; => Root logger
(get-logger :clails)     ; => CLAILS logger

;; Get logger from package name (automatically searches hierarchy)
(get-logger :clails/model/query)  ; => :clails/model or :clails or :root
```

### 2.4. Removing a Logger

Use the `remove-logger` function to remove a logger.

```common-lisp
(remove-logger :sql)
```

### 2.5. Clearing All Loggers

Use the `clear-loggers` function to remove all loggers.

```common-lisp
(clear-loggers)
```

---

## 3. Appender Configuration

### 3.1. Console Appender

Outputs logs to standard output.

```common-lisp
(make-console-appender
 :formatter (make-instance '<text-formatter>))
```

**Features:**
- Thread-safe: Uses the dynamic value of `*standard-output*`
- Safe for concurrent log output from multiple threads

### 3.2. File Appender

Outputs logs to a file.

```common-lisp
(make-file-appender
 :filepath "/var/log/myapp/app.log"
 :formatter (make-instance '<text-formatter>))
```

**Features:**
- Automatically creates the file if it doesn't exist
- Appends to existing files
- File stream is automatically managed

### 3.3. Closing an Appender

When using file appenders, it's recommended to explicitly close them when the application terminates.

```common-lisp
(defvar *file-appender* (make-file-appender
                         :filepath "/var/log/myapp/app.log"
                         :formatter (make-instance '<text-formatter>)))

;; On application shutdown
(close-appender *file-appender*)
```

---

## 4. Formatter Configuration

### 4.1. Text Formatter

Outputs logs in a human-readable text format.

```common-lisp
(make-instance '<text-formatter>)
```

**Output Example:**
```
[2024-01-15T10:30:45.123456+09:00] INFO: User logged in user-id=123 ip-address="192.168.1.1"
[2024-01-15T10:30:46.234567+09:00] ERROR: Database connection failed error="Connection timeout"
```

### 4.2. JSON Formatter

Outputs logs in JSON format. Useful for machine processing and integration with external systems.

```common-lisp
(make-instance '<json-formatter>)
```

**Output Example:**
```json
{"timestamp":"2024-01-15T10:30:45.123456+09:00","level":"info","message":"User logged in","user-id":123,"ip-address":"192.168.1.1"}
{"timestamp":"2024-01-15T10:30:46.234567+09:00","level":"error","message":"Database connection failed","error":"Connection timeout"}
```

---

## 5. Logging Output

### 5.1. Package-Based Logging

Automatically selects a logger based on the current package and outputs logs.

```common-lisp
(in-package #:myapp/model)

;; Output logs at each log level
(log-package.trace "Entering function" :function "find-user" :args '(123))
(log-package.debug "Query executed" :query "SELECT * FROM users WHERE id = ?" :params '(123))
(log-package.info "User found" :user-id 123 :user-name "Alice")
(log-package.warn "Slow query detected" :query-time 5.2 :threshold 3.0)
(log-package.error "Failed to save user" :error "Validation failed" :user-id 123)
(log-package.fatal "Database connection lost" :error "Connection timeout")
```

**Logger Selection:**
- When the current package is `#:myapp/model`
- Searches for loggers in order: `:myapp/model` → `:myapp` → `:root`
- Uses the first logger found

### 5.2. Named Logger Output

Explicitly specify a logger name to output logs.

```common-lisp
(log-to :root :info "Application started")
(log-to :sql :debug "Query executed" :query "SELECT * FROM users")
(log-to :myapp/service :error "Service failed" :service "UserService")
```

### 5.3. Purpose-Specific Logging

Use specialized logging macros for specific purposes.

#### SQL Logging

Used for recording database queries.

```common-lisp
(log.sql "SELECT * FROM users WHERE id = ?"
         :params '(123)
         :query-time 0.05
         :rows-affected 1)
```

- Logger name: `:sql`
- Log level: `:debug`

#### Web Access Logging

Used for recording HTTP requests/responses.

```common-lisp
(log.web-access "GET /users/123"
                :method "GET"
                :path "/users/123"
                :status 200
                :duration 0.123
                :ip-address "192.168.1.1")
```

- Logger name: `:web-access`
- Log level: `:info`

#### Audit Logging

Used for recording security-related events and user actions.

```common-lisp
(log.audit "User login successful"
           :user-id 123
           :username "alice"
           :ip-address "192.168.1.1"
           :action "login")
```

- Logger name: `:audit`
- Log level: `:info`

#### Task Logging

Used for recording task execution events.

```common-lisp
(log.task "Task started"
          :task-name "db:migrate"
          :namespace :db)

(log.task "Task completed"
          :task-name "cleanup"
          :namespace :system
          :duration 1.234
          :status :success)
```

- Logger name: `:task`
- Log level: `:info`

---

## 6. Adding Context

Use the `with-log-context` macro to add common context to all log messages within a block.

```common-lisp
(with-log-context (:request-id "req-12345" :user-id 123)
  (log-package.info "Processing request")
  (log-package.debug "Fetching user data")
  (log-package.info "Request completed"))

;; Output example:
;; [2024-01-15T10:30:45.123456+09:00] INFO: Processing request request-id="req-12345" user-id=123
;; [2024-01-15T10:30:45.234567+09:00] DEBUG: Fetching user data request-id="req-12345" user-id=123
;; [2024-01-15T10:30:45.345678+09:00] INFO: Request completed request-id="req-12345" user-id=123
```

### Nested Context

Contexts can be nested.

```common-lisp
(with-log-context (:request-id "req-12345")
  (log-package.info "Request started")
  
  (with-log-context (:operation "fetch-user" :user-id 123)
    (log-package.debug "Fetching user")
    (log-package.info "User fetched"))
  
  (log-package.info "Request completed"))

;; Output example:
;; [2024-01-15T10:30:45.123456+09:00] INFO: Request started request-id="req-12345"
;; [2024-01-15T10:30:45.234567+09:00] DEBUG: Fetching user request-id="req-12345" operation="fetch-user" user-id=123
;; [2024-01-15T10:30:45.345678+09:00] INFO: User fetched request-id="req-12345" operation="fetch-user" user-id=123
;; [2024-01-15T10:30:45.456789+09:00] INFO: Request completed request-id="req-12345"
```

---

## 7. Dynamic Log Level Changes

Log levels can be changed dynamically during application execution.

### 7.1. Changing Log Level

```common-lisp
;; Change the log level of a logger
(set-logger-level :root :debug)
(set-logger-level :clails/model :trace)
(set-logger-level :sql :info)
```

### 7.2. Checking Log Level

```common-lisp
;; Check if the specified log level is enabled for the current package
(log-level-enabled-p :debug)  ; => T or NIL

;; Check if the specified log level is enabled for a specific package
(log-level-enabled-p :debug :clails/model)  ; => T or NIL
(log-level-enabled-p :trace :sql)           ; => T or NIL
```

### 7.3. Conditional Logging

Execute expensive operations and log only when the log level is enabled.

```common-lisp
(when (log-level-enabled-p :debug)
  (let ((expensive-data (compute-expensive-debug-info)))
    (log-package.debug "Debug info" :data expensive-data)))
```

---

## 8. Dynamic Appender Addition

Appenders can be added to loggers at runtime.

```common-lisp
;; Add an appender to an existing logger
(add-appender :root
              (make-file-appender
               :filepath "/var/log/myapp/debug.log"
               :formatter (make-instance '<text-formatter>)))

;; Logger with multiple appenders
(let ((logger (get-logger :root)))
  ;; This logger outputs to both console and file
  (log-to :root :info "This message goes to both console and file"))
```

---

## 9. Practical Usage Examples

### 9.1. Basic Setup

```common-lisp
(defun setup-logging ()
  "Initialize application logging configuration"
  
  ;; Root logger: Console and general log file
  (register-logger :root
                   :level :info
                   :appender (make-console-appender
                              :formatter (make-instance '<text-formatter>)))
  (add-appender :root
                (make-file-appender
                 :filepath "/var/log/myapp/app.log"
                 :formatter (make-instance '<text-formatter>)))
  
  ;; SQL logger: SQL-specific log file (JSON format)
  (register-logger :sql
                   :level :debug
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/sql.log"
                              :formatter (make-instance '<json-formatter>)))
  
  ;; Web access log: Access log file
  (register-logger :web-access
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/access.log"
                              :formatter (make-instance '<text-formatter>)))
  
  ;; Audit log: Audit-specific file (JSON format)
  (register-logger :audit
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/audit.log"
                              :formatter (make-instance '<json-formatter>)))
  
  ;; Task log: Task execution log file
  (register-logger :task
                   :level :info
                   :appender (make-file-appender
                              :filepath "/var/log/myapp/task.log"
                              :formatter (make-instance '<text-formatter>))))

;; On application startup
(setup-logging)
```

### 9.2. Controller Usage Example

```common-lisp
(in-package #:myapp/controller)

(defun show-user (params)
  "Display user information"
  (let ((user-id (parse-integer (getf params :id))))
    (with-log-context (:action "show-user" :user-id user-id)
      (log-package.info "Fetching user")
      
      (handler-case
          (let ((user (find-user-by-id user-id)))
            (if user
                (progn
                  (log-package.info "User found")
                  (render-user user))
                (progn
                  (log-package.warn "User not found")
                  (render-404))))
        (error (e)
          (log-package.error "Failed to fetch user" :error (princ-to-string e))
          (render-500))))))
```

### 9.3. Model Usage Example

```common-lisp
(in-package #:myapp/model)

(defmethod save ((user <user>))
  "Save user"
  (log-package.debug "Saving user" :user-id (ref user :id))
  
  (when (log-level-enabled-p :trace)
    (log-package.trace "User data" :data (show-model-data user)))
  
  (handler-case
      (progn
        (validate user)
        (if (has-error-p user)
            (progn
              (log-package.warn "Validation failed"
                               :errors (get-all-errors user))
              nil)
            (progn
              (with-transaction
                (if (ref user :id)
                    (update1 user)
                    (insert1 user)))
              (log-package.info "User saved successfully"
                               :user-id (ref user :id))
              user)))
    (error (e)
      (log-package.error "Failed to save user"
                        :error (princ-to-string e)
                        :user-id (ref user :id))
      (error e))))
```

### 9.4. Environment-Specific Configuration

```common-lisp
(defun setup-logging-for-environment ()
  "Configure logging based on environment"
  (ecase (get-environment)
    (:development
     ;; Development: All at DEBUG level, console output
     (register-logger :root
                      :level :debug
                      :appender (make-console-appender
                                 :formatter (make-instance '<text-formatter>))))
    
    (:test
     ;; Test: All at INFO level, file output
     (register-logger :root
                      :level :info
                      :appender (make-file-appender
                                 :filepath "/tmp/test.log"
                                 :formatter (make-instance '<text-formatter>))))
    
    (:production
     ;; Production: INFO level, JSON format file output
     (register-logger :root
                      :level :info
                      :appender (make-file-appender
                                 :filepath "/var/log/myapp/app.log"
                                 :formatter (make-instance '<json-formatter>)))
     
     ;; Error logs go to a separate file
     (register-logger :root
                      :level :error
                      :appender (make-file-appender
                                 :filepath "/var/log/myapp/error.log"
                                 :formatter (make-instance '<json-formatter>))))))
```

---

## 10. Best Practices

### 10.1. Prefer Package-Based Logging

```common-lisp
;; Recommended: Package-based logging
(log-package.info "User created" :user-id 123)

;; Not recommended: Explicitly specifying logger name (unless necessary)
(log-to :myapp/controller :info "User created" :user-id 123)
```

### 10.2. Use Appropriate Log Levels

```common-lisp
;; TRACE: Detailed debug information (usually disabled)
(log-package.trace "Function arguments" :args args)

;; DEBUG: Debug information during development
(log-package.debug "Query result" :count (length results))

;; INFO: Normal operation recording
(log-package.info "User logged in" :user-id 123)

;; WARN: Potential issues
(log-package.warn "Slow query detected" :query-time 5.2)

;; ERROR: Error occurred but can continue
(log-package.error "Failed to send email" :error error-message)

;; FATAL: Critical error
(log-package.fatal "Database connection lost" :error error-message)
```

### 10.3. Use Structured Context

```common-lisp
;; Recommended: Structured with keyword arguments
(log-package.info "User action"
                  :user-id 123
                  :action "update"
                  :resource "profile")

;; Not recommended: Embedding information in message
(log-package.info (format nil "User ~A performed ~A on ~A" 123 "update" "profile"))
```

### 10.4. Check Log Level Before Expensive Operations

```common-lisp
;; Recommended: Log level check
(when (log-level-enabled-p :debug)
  (let ((debug-info (expensive-debug-computation)))
    (log-package.debug "Debug information" :info debug-info)))

;; Not recommended: Always execute expensive operation
(log-package.debug "Debug information"
                   :info (expensive-debug-computation))
```

### 10.5. Utilize Context

```common-lisp
;; Recommended: Share request ID with with-log-context
(with-log-context (:request-id request-id)
  (process-request)
  (save-result)
  (send-response))

;; Not recommended: Manually add to all logs
(log-package.info "Processing" :request-id request-id)
(log-package.info "Saved" :request-id request-id)
(log-package.info "Sent" :request-id request-id)
```

---

## 11. Troubleshooting

### 11.1. Logs Not Being Output

**Cause 1: Logger not registered**

```common-lisp
;; Check: Is the logger registered?
(get-logger :root)  ; => NIL means not registered

;; Solution: Register the logger
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))
```

**Cause 2: Log level not appropriate**

```common-lisp
;; Check: Is the log level enabled?
(log-level-enabled-p :debug)  ; => NIL means disabled

;; Solution: Change log level
(set-logger-level :root :debug)
```

**Cause 3: Hierarchy mismatch**

```common-lisp
;; Logging from :myapp/controller package
(in-package #:myapp/controller)
(log-package.info "Test")  ; => Not output

;; Solution: Register root logger
(register-logger :root
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))

;; Or: Register logger for the package
(register-logger :myapp
                 :level :info
                 :appender (make-console-appender
                            :formatter (make-instance '<text-formatter>)))
```

### 11.2. Not Writing to File

**Cause: Invalid file path**

```common-lisp
;; Solution: Ensure directory exists
(ensure-directories-exist "/var/log/myapp/")

(register-logger :root
                 :level :info
                 :appender (make-file-appender
                            :filepath "/var/log/myapp/app.log"
                            :formatter (make-instance '<text-formatter>)))
```

---

## Summary

The clails Logging system has the following features:

1. **Hierarchical Loggers**: Automatically selects loggers based on package hierarchy
2. **Flexible Output Destinations**: Supports multiple destinations such as console and file
3. **Customizable Formatting**: Text, JSON, and other formats suitable for different purposes
4. **Dynamic Configuration**: Log levels and appenders can be changed at runtime
5. **Context Management**: Structured log output with `with-log-context`
6. **Purpose-Specific Logging**: Logging macros for SQL, web access, audit, tasks, etc.
7. **Thread-Safe**: Safe log output from multiple threads

For detailed API reference, please refer to the docstring of each function.
