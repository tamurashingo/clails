# clails Environment Variables Guide

## Overview

clails applications can customize their behavior through environment variables.
This guide explains environment variables and global variables available to application developers, as well as Middleware configuration.

## Table of Contents

1. [Environment Variables](#1-environment-variables)
   - Database-related
   - Environment Variable Utility Functions
2. [Global Variables](#2-global-variables)
   - Project-related
   - Database-related
   - Routing-related
   - Application Lifecycle-related
3. [Middleware Configuration](#3-middleware-configuration)
   - Middleware Stack
   - Adding Middleware
   - Built-in Middleware
   - Middleware Execution Order
   - Middleware Usage Examples
4. [Setting Environment Variables](#4-setting-environment-variables)
5. [Configuration File Examples](#5-configuration-file-examples)
6. [Best Practices](#6-best-practices)
7. [Troubleshooting](#7-troubleshooting)

---

## 1. Environment Variables

### Database-related

clails applications can configure database connection information via environment variables.
These environment variables are referenced in `app/config/database.lisp`.

#### For SQLite3

| Environment Variable | Description | Default Value (Development) | Default Value (Test) | Production Handling |
|---------------------|-------------|----------------------------|---------------------|-------------------|
| `CLAILS_DB_NAME` | Database file path | `{project-dir}/tmp/{project-name}-develop.sqlite3` | `{project-dir}/tmp/{project-name}-test.sqlite3` | Required (no default) |

**Configuration Example**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                            "./tmp/myapp-develop.sqlite3"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                         "./tmp/myapp-test.sqlite3"))
    :production (:database-name ,(env "CLAILS_DB_NAME"))))
```

#### For MySQL

| Environment Variable | Description | Default Value (Development) | Default Value (Test) | Production Handling |
|---------------------|-------------|----------------------------|---------------------|-------------------|
| `CLAILS_DB_NAME` | Database name | `{project-name}_develop` | `{project-name}_test` | Required (no default) |
| `CLAILS_DB_HOST` | Hostname | `localhost` | `localhost` | Required (no default) |
| `CLAILS_DB_PORT` | Port number | `3306` | `3306` | Required (no default) |
| `CLAILS_DB_USERNAME` | Username | `root` | `root` | Required (no default) |
| `CLAILS_DB_PASSWORD` | Password | `password` | `password` | Required (no default) |

**Configuration Example**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "3306")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_test")
           :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
           :port ,(env-or-default "CLAILS_DB_PORT" "3306")
           :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
           :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD"))))
```

#### For PostgreSQL

| Environment Variable | Description | Default Value (Development) | Default Value (Test) | Production Handling |
|---------------------|-------------|----------------------------|---------------------|-------------------|
| `CLAILS_DB_NAME` | Database name | `{project-name}_develop` | `{project-name}_test` | Required (no default) |
| `CLAILS_DB_HOST` | Hostname | `localhost` | `localhost` | Required (no default) |
| `CLAILS_DB_PORT` | Port number | `5432` | `5432` | Required (no default) |
| `CLAILS_DB_USERNAME` | Username | `postgres` | `postgres` | Required (no default) |
| `CLAILS_DB_PASSWORD` | Password | `password` | `password` | Required (no default) |

**Configuration Example**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "5432")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_test")
           :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
           :port ,(env-or-default "CLAILS_DB_PORT" "5432")
           :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
           :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD"))))
```

### Environment Variable Utility Functions

clails provides utility functions for retrieving environment variables.

#### `env` Function

Retrieves the value of an environment variable. Returns `NIL` if the environment variable is not set.

```lisp
(clails/util:env "CLAILS_DB_NAME")
;; => "myapp_develop" or NIL
```

#### `env-or-default` Function

Retrieves the value of an environment variable. Returns a default value if the environment variable is not set.

```lisp
(clails/util:env-or-default "CLAILS_DB_HOST" "localhost")
;; => "localhost" (if environment variable is not set)
;; => "db.example.com" (if environment variable is set)
```

**Parameters**:
- `env-name` [string] - Environment variable name
- `default-value` [t] - Default value

**Return value**:
- [string] - Environment variable value
- [t] - Default value (if environment variable is not set)

---

## 2. Global Variables

### Project-related

#### `*project-name*`

The project name.

```lisp
clails/environment:*project-name*
;; => :myapp
```

**Type**: `keyword`

**Usage**:
```lisp
;; Get project name
(format t "Project: ~A~%" clails/environment:*project-name*)
```

#### `*project-environment*`

The current environment (development, test, production).

```lisp
clails/environment:*project-environment*
;; => :develop
```

**Type**: `keyword`

**Possible values**:
- `:develop` - Development environment
- `:test` - Test environment
- `:production` - Production environment

**Usage**:
```lisp
;; Switch behavior by environment
(case clails/environment:*project-environment*
  (:develop
   (format t "Development mode~%"))
  (:test
   (format t "Test mode~%"))
  (:production
   (format t "Production mode~%")))
```

### Database-related

#### `*database-type*`

The database type (SQLite3, MySQL, PostgreSQL).

```lisp
clails/environment:*database-type*
;; => #<CLAILS/ENVIRONMENT:<DATABASE-TYPE-SQLITE3>>
```

**Type**: `<database-type>` class instance

**Classes**:
- `<database-type-sqlite3>` - SQLite3
- `<database-type-mysql>` - MySQL
- `<database-type-postgresql>` - PostgreSQL

**Usage**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-type* 
      (make-instance 'clails/environment:<database-type-postgresql>))
```

#### `*database-config*`

Database connection configuration.

```lisp
clails/environment:*database-config*
;; => (:develop (:database-name "myapp_develop" :host "localhost" ...)
;;     :test (:database-name "myapp_test" :host "localhost" ...)
;;     :production (:database-name "myapp_prod" :host "db.example.com" ...))
```

**Type**: `property list`

**Structure**:
```lisp
(list
  :develop (:database-name "..." :host "..." :port "..." ...)
  :test (:database-name "..." :host "..." :port "..." ...)
  :production (:database-name "..." :host "..." :port "..." ...))
```

**Usage**:
```lisp
;; Get current environment's database config
(getf clails/environment:*database-config* 
      clails/environment:*project-environment*)
```

### Routing-related

#### `*routing-tables*`

Routing table configuration.

```lisp
clails/environment:*routing-tables*
;; => ((:path "/" :controller "myapp/controller::<top-controller>")
;;     (:path "/users" :controller "myapp/controller::<users-controller>")
;;     ...)
```

**Type**: `list`

**Structure**:
```lisp
'((:path "/path"
   :controller "package::<controller-class>")
  ...)
```

**Usage**:
```lisp
;; app/config/routes.lisp
(setf clails/environment:*routing-tables*
  '((:path "/"
     :controller "myapp/controller::<top-controller>")
    (:path "/users/:id"
     :controller "myapp/controller::<user-controller>")))

;; Initialize routing tables
(clails/controller/base-controller:initialize-routing-tables)
```

### Application Lifecycle-related

#### `*startup-hooks*`

Functions to execute at application startup.

```lisp
clails/environment:*startup-hooks*
;; => (#<FUNCTION ...> #<FUNCTION ...>)
```

**Type**: `list of functions`

**Usage**:
```lisp
;; Add startup hook
(setf clails/environment:*startup-hooks*
      (list #'(lambda ()
                (format t "Application starting...~%")
                (initialize-cache)
                (connect-external-services))))
```

#### `*shutdown-hooks*`

Functions to execute at application shutdown.

```lisp
clails/environment:*shutdown-hooks*
;; => (#<FUNCTION ...> #<FUNCTION ...>)
```

**Type**: `list of functions`

**Usage**:
```lisp
;; Add shutdown hook
(setf clails/environment:*shutdown-hooks*
      (list #'(lambda ()
                (format t "Application shutting down...~%")
                (cleanup-cache)
                (disconnect-external-services))))
```

---

## 3. Middleware Configuration

### Middleware Stack

Middleware is software that processes HTTP requests and responses.
clails uses Clack's middleware system.

#### `*middleware-stacks*`

Middleware stack configuration.

```lisp
clails/environment:*middleware-stacks*
;; => ((#<FUNCTION ...> :option1 value1) ...)
```

**Type**: `list`

**Structure**:
```lisp
'((middleware-function :option1 value1 :option2 value2)
  (middleware-function)
  ...)
```

### Adding Middleware

Use `add-middleware` to add middleware.

#### `add-middleware` Function

```lisp
(clails/environment:add-middleware middleware-function &rest options)
```

**Parameters**:
- `middleware-function` [function] - Middleware function
- `options` [keyword parameters] - Middleware options

**Usage**:
```lisp
;; Add session middleware
(clails/environment:add-middleware
  #'clails/middleware/session:session-middleware
  :store (make-instance 'clails/middleware/session:<memory-store>))

;; Add custom middleware
(clails/environment:add-middleware
  #'my-custom-middleware)
```

### Built-in Middleware

clails provides the following built-in middleware:

#### Session Middleware

Provides session management.

```lisp
(clails/environment:add-middleware
  #'clails/middleware/session:session-middleware
  :store (make-instance 'clails/middleware/session:<memory-store>)
  :cookie-name "session-id"
  :cookie-path "/"
  :cookie-domain nil
  :cookie-secure nil
  :cookie-http-only t
  :cookie-max-age 86400)
```

**Options**:
- `:store` - Session store (default: `<memory-store>`)
- `:cookie-name` - Cookie name (default: `"session-id"`)
- `:cookie-path` - Cookie path (default: `"/"`)
- `:cookie-domain` - Cookie domain (default: `nil`)
- `:cookie-secure` - HTTPS only (default: `nil`)
- `:cookie-http-only` - HTTP only (default: `t`)
- `:cookie-max-age` - Cookie lifetime in seconds (default: `86400`)

#### CORS Middleware

Provides CORS (Cross-Origin Resource Sharing) support.

```lisp
(clails/environment:add-middleware
  #'clails/middleware/cors:cors-middleware
  :allow-origins '("http://localhost:3000" "https://example.com")
  :allow-methods '(:GET :POST :PUT :DELETE)
  :allow-headers '("Content-Type" "Authorization")
  :allow-credentials t
  :max-age 86400)
```

**Options**:
- `:allow-origins` - Allowed origins (list)
- `:allow-methods` - Allowed HTTP methods (list)
- `:allow-headers` - Allowed headers (list)
- `:allow-credentials` - Allow credentials (boolean)
- `:max-age` - Preflight cache duration in seconds (number)

#### Logger Middleware

Logs HTTP requests.

```lisp
(clails/environment:add-middleware
  #'clails/middleware/logger:logger-middleware
  :output *standard-output*
  :format :combined)
```

**Options**:
- `:output` - Output destination (stream)
- `:format` - Log format (`:combined`, `:common`, `:short`)

### Middleware Execution Order

Middleware is executed in the order it was added.

**Example**:
```lisp
;; First middleware
(clails/environment:add-middleware #'middleware-1)

;; Second middleware
(clails/environment:add-middleware #'middleware-2)

;; Third middleware
(clails/environment:add-middleware #'middleware-3)
```

**Execution order**:
```
Request  → middleware-1 → middleware-2 → middleware-3 → Application
Response ← middleware-1 ← middleware-2 ← middleware-3 ← Application
```

### Middleware Usage Examples

#### Adding Session Support

```lisp
;; app/config/middleware.lisp
(in-package #:myapp/config)

;; Add session middleware
(clails/environment:add-middleware
  #'clails/middleware/session:session-middleware
  :store (make-instance 'clails/middleware/session:<memory-store>)
  :cookie-name "myapp-session"
  :cookie-http-only t
  :cookie-max-age (* 24 60 60))  ; 24 hours
```

#### Adding CORS Support

```lisp
;; app/config/middleware.lisp
(in-package #:myapp/config)

;; Add CORS middleware
(clails/environment:add-middleware
  #'clails/middleware/cors:cors-middleware
  :allow-origins '("http://localhost:3000")
  :allow-methods '(:GET :POST :PUT :DELETE :OPTIONS)
  :allow-headers '("Content-Type" "Authorization")
  :allow-credentials t)
```

#### Adding Request Logging

```lisp
;; app/config/middleware.lisp
(in-package #:myapp/config)

;; Add logger middleware
(clails/environment:add-middleware
  #'clails/middleware/logger:logger-middleware
  :output *standard-output*
  :format :combined)
```

#### Creating Custom Middleware

```lisp
;; app/middleware/custom.lisp
(defun authentication-middleware (app)
  "Authentication middleware"
  (lambda (env)
    (let ((token (getf (getf env :headers) "authorization")))
      (if (valid-token-p token)
          (funcall app env)
          '(401 (:content-type "text/plain") ("Unauthorized"))))))

;; Add to middleware stack
(clails/environment:add-middleware
  #'authentication-middleware)
```

---

## 4. Setting Environment Variables

### Using .env File

Create a `.env` file in the project root.

**.env**:
```bash
CLAILS_DB_NAME=myapp_develop
CLAILS_DB_HOST=localhost
CLAILS_DB_PORT=5432
CLAILS_DB_USERNAME=postgres
CLAILS_DB_PASSWORD=secret
```

**Important**: Add `.env` to `.gitignore` to prevent committing it.

### Setting in Shell

```bash
# Set temporarily
export CLAILS_DB_NAME="myapp_develop"
export CLAILS_DB_HOST="localhost"

# Start application with environment variables
CLAILS_DB_NAME="myapp" CLAILS_DB_HOST="db.example.com" clails server
```

### Setting in Dockerfile

```dockerfile
FROM fukamachi/sbcl:latest

# Set environment variables
ENV CLAILS_DB_NAME=myapp_production
ENV CLAILS_DB_HOST=db
ENV CLAILS_DB_PORT=5432
ENV CLAILS_DB_USERNAME=appuser

# Application setup
COPY . /app
WORKDIR /app
RUN qlot install

CMD ["qlot", "exec", "ros", "run", "--load", "app/application.lisp"]
```

---

## 5. Configuration File Examples

### app/config/database.lisp

```lisp
(in-package #:cl-user)
(defpackage #:myapp/config
  (:use #:cl))

(in-package #:myapp/config)

;; Database type
(setf clails/environment:*database-type* 
      (make-instance 'clails/environment:<database-type-postgresql>))

;; Database connection information
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(clails/util:env-or-default 
                               "CLAILS_DB_NAME" "myapp_develop")
              :host ,(clails/util:env-or-default 
                      "CLAILS_DB_HOST" "localhost")
              :port ,(clails/util:env-or-default 
                      "CLAILS_DB_PORT" "5432")
              :username ,(clails/util:env-or-default 
                          "CLAILS_DB_USERNAME" "postgres")
              :password ,(clails/util:env-or-default 
                          "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(clails/util:env-or-default 
                            "CLAILS_DB_NAME" "myapp_test")
           :host ,(clails/util:env-or-default 
                   "CLAILS_DB_HOST" "localhost")
           :port ,(clails/util:env-or-default 
                   "CLAILS_DB_PORT" "5432")
           :username ,(clails/util:env-or-default 
                       "CLAILS_DB_USERNAME" "postgres")
           :password ,(clails/util:env-or-default 
                       "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(clails/util:env "CLAILS_DB_NAME")
                 :host ,(clails/util:env "CLAILS_DB_HOST")
                 :port ,(clails/util:env "CLAILS_DB_PORT")
                 :username ,(clails/util:env "CLAILS_DB_USERNAME")
                 :password ,(clails/util:env "CLAILS_DB_PASSWORD"))))
```

### app/config/routes.lisp

```lisp
(in-package #:myapp/config)

;; Routing table configuration
(setf clails/environment:*routing-tables*
  '((:path "/"
     :controller "myapp/controller::<top-controller>")
    
    (:path "/users"
     :controller "myapp/controller::<users-controller>")
    
    (:path "/users/:id"
     :controller "myapp/controller::<user-controller>")
    
    (:path "/api/posts"
     :controller "myapp/controller/api::<posts-controller>")
    
    (:path "/api/posts/:id"
     :controller "myapp/controller/api::<post-controller>")))

;; Initialize routing tables
(clails/controller/base-controller:initialize-routing-tables)
```

---

## 6. Best Practices

### Using Environment Variables

1. **Development Environment**: Use default values for easy development
2. **Test Environment**: Use test-specific configuration
3. **Production Environment**: Make environment variables required, don't rely on defaults

### Security

1. **Passwords and Sensitive Information**: Manage via environment variables, don't hardcode in source code
2. **Production Configuration**: Don't include `.env` files in version control
3. **Environment Variable Validation**: Check that required environment variables are set at startup

```lisp
;; Example environment variable check in production
(when (eq clails/environment:*project-environment* :production)
  (unless (clails/util:env "CLAILS_DB_PASSWORD")
    (error "CLAILS_DB_PASSWORD is required in production")))
```

### Configuration Separation

1. **Environment-specific Configuration**: Use environment variables
2. **Environment-independent Configuration**: Write directly in configuration files
3. **Complex Configuration**: Create dedicated initialization functions

---

## 7. Troubleshooting

### Environment Variables Not Reflected

**Cause**: Environment variable set too late or set incorrectly

**Solution**:
```bash
# Set environment variable before starting application
export CLAILS_DB_NAME="myapp"
clails server

# Or set simultaneously
CLAILS_DB_NAME="myapp" clails server
```

### Cannot Connect to Database

**Cause**: Database connection information not configured correctly

**Solution**:
```lisp
;; Check connection information
(format t "Database config: ~A~%" 
        (getf clails/environment:*database-config* 
              clails/environment:*project-environment*))

;; Check environment variables
(format t "DB_NAME: ~A~%" (clails/util:env "CLAILS_DB_NAME"))
```

### Application Won't Start in Production

**Cause**: Required environment variables not set

**Solution**:
```lisp
;; Add startup check
(when (eq clails/environment:*project-environment* :production)
  (let ((required-vars '("CLAILS_DB_NAME" 
                         "CLAILS_DB_HOST" 
                         "CLAILS_DB_USERNAME" 
                         "CLAILS_DB_PASSWORD")))
    (dolist (var required-vars)
      (unless (clails/util:env var)
        (error "Required environment variable ~A is not set" var)))))
```

---

## Summary

clails environment configuration has the following features:

1. **Environment Variable Support**: Manage database connection information etc. via environment variables
2. **Flexible Configuration**: Use different configurations for development, test, and production
3. **Global Variables**: Configuration shared across the entire application
4. **Lifecycle Management**: Initialization and cleanup via startup/shutdown hooks

Proper environment variable configuration enables building secure and maintainable applications.
