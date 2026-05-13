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

**Connection Pool Configuration (Optional)**:

For SQLite3, you can specify additional parameters to fine-tune connection pool behavior.

| Parameter | Description | Type | Default Value |
|-----------|-------------|------|---------------|
| `:initial-size` | Initial pool size (number of connections created at startup) | integer | 10 |
| `:max-size` | Maximum pool size (connection limit) | integer | 10 |
| `:checkout-timeout` | Connection checkout timeout (seconds) | integer | 30 |
| `:idle-timeout` | Idle connection disposal time (seconds) | integer | 600 |
| `:max-lifetime` | Maximum connection lifetime (seconds) | integer | 1800 |
| `:keepalive-interval` | Keepalive execution interval (seconds) | integer | 0 (disabled) |
| `:reaper-interval` | Unused connection reaping interval (seconds) | integer | 60 |

**Note**: SQLite3 does not support the `:validation-query` parameter.

**Configuration Example (Connection Pool Customization)**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                            "./tmp/myapp-develop.sqlite3")
              ;; Connection pool configuration
              :initial-size 10         ; Create 10 connections at startup
              :max-size 10             ; Allow up to 10 connections
              :checkout-timeout 30     ; Error if connection not available after 30 seconds
              :idle-timeout 600        ; Dispose idle connections after 10 minutes
              :max-lifetime 1800       ; Maximum connection lifetime is 30 minutes
              :keepalive-interval 0    ; Keepalive disabled
              :reaper-interval 60)     ; Reap unused connections every 60 seconds
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 ;; Connection pool configuration for production
                 :initial-size 10
                 :max-size 10
                 :checkout-timeout 30
                 :idle-timeout 600
                 :max-lifetime 1800
                 :keepalive-interval 0
                 :reaper-interval 60)))
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

**Connection Pool Configuration (Optional)**:

For MySQL, you can specify additional parameters to fine-tune connection pool behavior.

| Parameter | Description | Type | Default Value |
|-----------|-------------|------|---------------|
| `:initial-size` | Initial pool size (number of connections created at startup) | integer | 3 |
| `:max-size` | Maximum pool size (connection limit) | integer | 10 |
| `:checkout-timeout` | Connection checkout timeout (seconds) | integer | 5 |
| `:idle-timeout` | Idle connection disposal time (seconds) | integer | 600 |
| `:max-lifetime` | Maximum connection lifetime (seconds) | integer | 1800 |
| `:keepalive-interval` | Keepalive execution interval (seconds) | integer | 0 (disabled) |
| `:validation-query` | Connection validation query | string | "SELECT 1" |
| `:reaper-interval` | Unused connection reaping interval (seconds) | integer | 60 |

**Configuration Example (Connection Pool Customization)**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "3306")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password")
              ;; Connection pool configuration
              :initial-size 5          ; Create 5 connections at startup
              :max-size 20             ; Allow up to 20 connections
              :checkout-timeout 10     ; Error if connection not available after 10 seconds
              :idle-timeout 300        ; Dispose idle connections after 5 minutes
              :max-lifetime 3600       ; Maximum connection lifetime is 1 hour
              :keepalive-interval 30   ; Execute keepalive every 30 seconds
              :validation-query "SELECT 1" ; Connection validation query
              :reaper-interval 30)     ; Reap unused connections every 30 seconds
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD")
                 ;; Larger values for production
                 :initial-size 10
                 :max-size 50
                 :checkout-timeout 5
                 :idle-timeout 600
                 :max-lifetime 3600
                 :keepalive-interval 60
                 :reaper-interval 60)))
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

**Connection Pool Configuration (Optional)**:

For PostgreSQL, you can specify additional parameters to fine-tune connection pool behavior.

| Parameter | Description | Type | Default Value |
|-----------|-------------|------|---------------|
| `:initial-size` | Initial pool size (number of connections created at startup) | integer | 3 |
| `:max-size` | Maximum pool size (connection limit) | integer | 10 |
| `:checkout-timeout` | Connection checkout timeout (seconds) | integer | 5 |
| `:idle-timeout` | Idle connection disposal time (seconds) | integer | 600 |
| `:max-lifetime` | Maximum connection lifetime (seconds) | integer | 1800 |
| `:keepalive-interval` | Keepalive execution interval (seconds) | integer | 0 (disabled) |
| `:validation-query` | Connection validation query | string | "SELECT 1" |
| `:reaper-interval` | Unused connection reaping interval (seconds) | integer | 60 |

**Configuration Example (Connection Pool Customization)**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "5432")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password")
              ;; Connection pool configuration
              :initial-size 5          ; Create 5 connections at startup
              :max-size 20             ; Allow up to 20 connections
              :checkout-timeout 10     ; Error if connection not available after 10 seconds
              :idle-timeout 300        ; Dispose idle connections after 5 minutes
              :max-lifetime 3600       ; Maximum connection lifetime is 1 hour
              :keepalive-interval 30   ; Execute keepalive every 30 seconds
              :validation-query "SELECT 1" ; Connection validation query
              :reaper-interval 30)     ; Reap unused connections every 30 seconds
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD")
                 ;; Larger values for production
                 :initial-size 10
                 :max-size 50
                 :checkout-timeout 5
                 :idle-timeout 600
                 :max-lifetime 3600
                 :keepalive-interval 60
                 :reaper-interval 60)))
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

**Default value**: `'((:path "/" :controller "clails/controller/base-controller:<default-controller>"))`

**Configuration location**: `app/config/environment.lisp`

**Route Entry Properties**:

Each route entry is a plist with the following properties:

**Required properties**:
- `:path` [string] - URI path pattern. Supports parameter placeholders like `/users/:id`
- `:controller` [string] - Fully qualified controller class name in format `"package::<class-name>"`

**Optional properties** (for custom routing patterns):
- `:scanner` [string] - Custom regex pattern string for matching request paths. Takes highest priority.
- `:keys` [list of strings] - List of URL parameter names to extract. Used with `:scanner`.
- `:generate-scanner` [function designator] - Function to generate `:scanner` and `:keys` dynamically. Must return a plist with `:scanner` (string) and `:keys` (list).

**Priority order for scanner generation**:
1. `:scanner` (highest priority)
2. `:generate-scanner` (only if `:scanner` not present)
3. Default behavior using `create-scanner-from-uri-path`

**Basic Usage**:
```lisp
;; app/config/environment.lisp
(setf clails/environment:*routing-tables*
  '((:path "/"
     :controller "myapp/controller::<top-controller>")
    (:path "/users/:id"
     :controller "myapp/controller::<user-controller>")))

;; Initialize routing tables
(clails/controller/base-controller:initialize-routing-tables)
```

**Advanced Usage - Custom Routing Patterns**:

```lisp
;; Catch-all route for SPA (Single Page Application)
(setf clails/environment:*routing-tables*
  '((:path "/spa/*"
     :controller "myapp/controller::<spa-controller>"
     :scanner "^/spa/.*$")))

;; Static file serving with parameter extraction
(setf clails/environment:*routing-tables*
  '((:path "/static/*"
     :controller "myapp/controller::<static-controller>"
     :scanner "^/static/(.*)$"
     :keys ("filepath"))))

;; Numeric ID only constraint
(setf clails/environment:*routing-tables*
  '((:path "/users/:id"
     :controller "myapp/controller::<user-controller>"
     :scanner "^/users/([0-9]+)$"
     :keys ("id"))))

;; Custom scanner generator function
(setf clails/environment:*routing-tables*
  '((:path "/api/*"
     :controller "myapp/controller::<api-controller>"
     :generate-scanner (lambda (route-entry)
                         (let ((path (getf route-entry :path)))
                           (list :scanner "^/api/.*$"
                                 :keys nil))))))

;; Mixed patterns
(setf clails/environment:*routing-tables*
  '(;; Default pattern with parameters
    (:path "/posts/:post-id/comments/:comment-id"
     :controller "myapp/controller::<comments-controller>")
    
    ;; Catch-all for SPA
    (:path "/app/*"
     :controller "myapp/controller::<spa-controller>"
     :scanner "^/app/.*$")
    
    ;; Custom pattern with parameter
    (:path "/files/*"
     :controller "myapp/controller::<file-controller>"
     :scanner "^/files/(.*)$"
     :keys ("path"))))

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

clails supports Lack middleware, allowing you to customize the request processing pipeline.

### Middleware Stack

#### `*clails-middleware-stack*`

Holds a list of Lack middlewares. Requests are processed in the order of this stack.

**Package**: `clails/middleware`

**Type**: list of middleware functions

**Default value**: 
```lisp
(list
  *lack-middleware-transaction*
  *lack-middleware-clails-controller*
  #'(lambda (app)
      (funcall *lack-middleware-static*
               app
               :path "/"
               :root #P"./public/")))
```

**Configuration location**: `app/config/environment.lisp`

**Note**: Do not modify this variable directly. Use `add-middleware-before` or `add-middleware-after` functions instead.

### Adding Middleware

#### `add-middleware-before` Function

Adds a middleware to the beginning of the middleware stack. Middleware added at the beginning will be executed before all existing middleware.

**Parameters**:
- `middleware` [function] - Middleware function to add

**Configuration example**:
```lisp
(in-package #:myapp/config/environment)

;; Add Lack's session middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (funcall lack.middleware.session:*lack-middleware-session*
             app
             :state (make-instance 'lack.session.state.cookie:cookie-state))))

;; Add custom middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      ;; Pre-request processing
      (format t "Request started: ~A~%" (getf env :path-info))
      (let ((response (funcall app env)))
        ;; Post-response processing
        (format t "Request completed~%")
        response))))
```

#### `add-middleware-after` Function

Adds a middleware to the end of the middleware stack. Middleware added at the end will be executed after all existing middleware.

**Parameters**:
- `middleware` [function] - Middleware function to add

**Configuration example**:
```lisp
(in-package #:myapp/config/environment)

;; Add logging middleware
(clails/middleware:add-middleware-after
  (lambda (app)
    (lambda (env)
      (let* ((start-time (get-internal-real-time))
             (response (funcall app env))
             (elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (format t "Request time: ~A seconds~%" elapsed)
        response))))
```

#### `*lack-middleware-transaction*`

Middleware that automatically manages database transactions.

**Package**: `clails/middleware/transaction-middleware`

**Features**:
- Acquires a database connection for each request
- Starts a transaction
- Commits if the request completes successfully
- Rolls back if an error occurs
- Returns the connection to the connection pool

**Enable/disable toggle**:
```lisp
;; Disable transaction middleware
(setf clails/middleware/transaction-middleware:*enable-transaction-middleware* nil)

;; Enable transaction middleware (default)
(setf clails/middleware/transaction-middleware:*enable-transaction-middleware* t)
```

#### `*lack-middleware-clails-controller*`

Middleware that handles routing and controller dispatch.

**Package**: `clails/middleware/clails-middleware`

**Features**:
- Searches for controllers based on URL paths
- Calls appropriate methods based on HTTP methods (GET/POST/PUT/DELETE)
- View resolution and rendering
- 404 error handling

**Note**: This middleware is required. Do not remove it.

#### `*lack-middleware-static*`

Middleware that serves static files (CSS, JavaScript, images, etc.).

**Package**: `lack.middleware.static`

**Default configuration**:
```lisp
#'(lambda (app)
    (funcall *lack-middleware-static*
             app
             :path "/"
             :root #P"./public/"))
```

**Customization example**:
```lisp
;; Change static file path
(setf clails/middleware:*clails-middleware-stack*
  (list
    clails/middleware:*lack-middleware-transaction*
    clails/middleware:*lack-middleware-clails-controller*
    #'(lambda (app)
        (funcall lack.middleware.static:*lack-middleware-static*
                 app
                 :path "/static"
                 :root #P"./assets/"))))
```

### Middleware Execution Order

Middleware is executed in the order of `*clails-middleware-stack*`.

```
Request
  ↓
*lack-middleware-transaction*
  ↓
*lack-middleware-clails-controller*
  ↓
*lack-middleware-static*
  ↓
Response
```

### Middleware Usage Examples

#### Session Management

```lisp
(in-package #:myapp/config/environment)

;; Add Lack's session middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (funcall lack.middleware.session:*lack-middleware-session*
             app
             :state (make-instance 'lack.session.state.cookie:cookie-state
                                  :secret "your-secret-key"
                                  :httponly t))))
```

#### CORS Support

```lisp
(in-package #:myapp/config/environment)

;; Add CORS middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (let ((response (funcall app env)))
        ;; Add CORS headers
        (setf (getf (second response) :access-control-allow-origin) "*")
        (setf (getf (second response) :access-control-allow-methods) "GET, POST, PUT, DELETE")
        response))))
```

#### Request Logging

```lisp
(in-package #:myapp/config/environment)

;; Add request logging middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (format t "~A ~A~%"
              (getf env :request-method)
              (getf env :path-info))
      (funcall app env))))
```

#### Authentication

```lisp
(in-package #:myapp/config/environment)

;; Add authentication middleware
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (let ((path (getf env :path-info)))
        ;; Skip authentication for specific paths
        (if (or (string= path "/login")
                (string= path "/public"))
            (funcall app env)
            ;; Check authentication
            (if (authenticated-p env)
                (funcall app env)
                '(401 (:content-type "text/plain") ("Unauthorized"))))))))
```

#### Viewing the Middleware Stack

```lisp
;; Display current middleware stack
(clails/middleware:show-middleware-stack)
```

---

## 4. Setting Environment Variables

### Development Environment

In development, you can set environment variables in the shell or use a `.env` file.

#### Setting in Shell

```bash
# Bash/Zsh
export CLAILS_DB_NAME="myapp_develop"
export CLAILS_DB_HOST="localhost"
export CLAILS_DB_PORT="5432"
export CLAILS_DB_USERNAME="postgres"
export CLAILS_DB_PASSWORD="password"

# Start application
clails server
```

#### Using .env File (with direnv, etc.)

```bash
# .env
export CLAILS_DB_NAME="myapp_develop"
export CLAILS_DB_HOST="localhost"
export CLAILS_DB_PORT="5432"
export CLAILS_DB_USERNAME="postgres"
export CLAILS_DB_PASSWORD="password"
```

### Production Environment

In production, always set environment variables. Do not rely on default values.

```bash
# For Systemd service
[Service]
Environment="CLAILS_DB_NAME=myapp_production"
Environment="CLAILS_DB_HOST=db.example.com"
Environment="CLAILS_DB_PORT=5432"
Environment="CLAILS_DB_USERNAME=app_user"
Environment="CLAILS_DB_PASSWORD=secret_password"

# For Docker Compose
services:
  app:
    environment:
      - CLAILS_DB_NAME=myapp_production
      - CLAILS_DB_HOST=db
      - CLAILS_DB_PORT=5432
      - CLAILS_DB_USERNAME=app_user
      - CLAILS_DB_PASSWORD=secret_password
```

### Test Environment

In test environment, use test-specific configuration.

```bash
# When running tests
export CLAILS_DB_NAME="myapp_test"
export APP_ENV="TEST"

# Run tests
qlot exec rove myapp-test.asd
```

---

## 5. Configuration File Examples

### app/config/environment.lisp

```lisp
(in-package #:myapp/config)

;; Set project name
(setf clails/environment:*project-name* "myapp")

;; Set execution environment
(clails/environment:set-environment 
  (clails/util:env-or-default "APP_ENV" "DEVELOP"))

;; Set startup hooks
(setf clails/environment:*startup-hooks*
  '("clails/model/connection:startup-connection-pool"
    "myapp/initializer:initialize-table-information"
    "myapp/initializer:setup-logger"))

;; Set shutdown hooks
(setf clails/environment:*shutdown-hooks*
  '("myapp/finalizer:cleanup-resources"
    "clails/model/connection:shutdown-connection-pool"))
```

### app/config/database.lisp

```lisp
(in-package #:myapp/config)

;; Set database type
(setf clails/environment:*database-type*
      (make-instance 'clails/environment:<database-type-postgresql>))

;; Set database connection information
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
