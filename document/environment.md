# clails Environment Variables Guide

## Overview

clails applications can customize their behavior through environment variables.
This guide explains environment variables and global variables available to application developers, as well as Middleware configuration.

## Table of Contents

- [Initialization Stages](#initialization-stages)
  - The three stages
  - Which stage each CLI command reaches
  - Practical implication
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
8. [Contributing: Adding New Configuration Variables](#8-contributing-adding-new-configuration-variables)

---

## Initialization Stages

Before application code relies on a value in `clails/environment`, it helps to know *when* that value actually becomes populated. clails initializes state in three stages, and which stages a given `clails` CLI command reaches depends on the command. This is why code that assumes a later-stage variable is available -- most notably `*connection-pool*` -- can work fine under `clails server` but fail unexpectedly under `clails db:seed`, `clails task ...`, or other commands.

### The three stages

**Stage 1 -- File-load time**

Reached simply by loading the `clails` system itself; every invocation of the `clails` CLI (including `--help`) loads `clails`. At this point only the framework's built-in defaults from `src/environment.lisp` are in effect: `*project-environment*` defaults to `:develop`, `*connection-pool*` is `nil`, `*routing-tables*` holds the single built-in default route, `*startup-hooks*`/`*shutdown-hooks*` hold only the framework's own defaults, and so on. No project has been loaded yet.

**Stage 2 -- `clails.boot` execution ("project load") time**

Reached by every command except `new` (and running `clails` with no command / `--help`). Triggered by `roswell/clails.ros`'s `load-project`, which loads the project's `clails.boot`. In order, this:

1. Runs `(ql:quickload :<project>)`, which walks the ASDF package-inferred-system dependency graph rooted at `app/application-loader.lisp` and runs every file's top-level forms. This is where project config files run: `app/config/environment.lisp` sets `*project-name*` and `*routing-tables*`, and pushes onto `*startup-hooks*`/`*shutdown-hooks*`; `app/config/database.lisp` sets `*database-type*`; `app/models/package.lisp` registers models.
2. Sets `*project-dir*`, `*migration-base-dir*`, and `*task-base-dir*` explicitly.
3. Applies `CLAILS_ENV` (via `set-environment`), setting `*project-environment*`.
4. Calls `<project>/config/database:initialize-database-config`, which sets `*database-config*`.

After Stage 2, every `clails/environment` variable is populated **except** the ones the connection pool is responsible for: `*connection-pool*`, the internal `*thread-connection-pool*`, and anything a startup hook would otherwise set up (e.g. `*table-information-initialized*`, unless something calls `initialize-table-information` directly).

**Stage 3 -- runtime startup (`call-startup-hooks`)**

This is where `*connection-pool*` gets created, by running every function in `*startup-hooks*` in order (default: `clails/model/connection:startup-connection-pool`; generated projects also push `initialize-table-information` and a logger initializer onto the front of this list in `app/config/environment.lisp`). **Only `clails server` reaches this stage** -- `call-startup-hooks` is called once, right before the server starts accepting requests, from `clails/cmd:server` (`src/cmd.lisp`). There is a corresponding shutdown stage (`call-shutdown-hooks`, running `*shutdown-hooks*`), reached only by `clails stop` / when the running server is interrupted.

> **`db:seed` and `test` are a special case.** Neither calls `call-startup-hooks`, but both call `clails/model/connection:startup-connection-pool` (and `initialize-table-information`) directly -- hard-coded in `src/cmd.lisp` -- before doing their real work, and shut the pool back down (`shutdown-connection-pool`) afterward. So `*connection-pool*` **is** populated while `db:seed`/`test` run, but any *additional* custom startup hooks a project has added to `*startup-hooks*` (beyond the default connection-pool startup) do **not** run for these two commands, since they bypass `call-startup-hooks` entirely.
>
> `db:create`, `db:migrate`, `db:migrate:up`, `db:migrate:down`, and `db:rollback` don't need the pool at all -- they talk to the database through short-lived direct connections (`with-db-connection-direct`), never through `*connection-pool*`.

### Which stage each CLI command reaches

| Command | Reaches Stage 2 (project loaded)? | Reaches Stage 3 (`*connection-pool*` populated)? | Notes |
|---|---|---|---|
| `new` | No | No | Creates a project on disk; there is no project to load yet. |
| `environment` | Yes | No | Just prints `*project-environment*`. |
| `generate:model` / `:migration` / `:view` / `:controller` / `:scaffold` / `:task` | Yes | No | File generation only; these never touch the database. |
| `db:create` | Yes | No (direct connection) | Uses `with-db-connection-direct`, not the pool. |
| `db:migrate`, `db:migrate:up`, `db:migrate:down`, `db:rollback`, `db:status` | Yes | No (direct connection) | Migrations run over direct connections, not the pool. |
| `db:seed` | Yes | **Yes**, via a direct call to `startup-connection-pool` / `initialize-table-information` (not `call-startup-hooks`) | Custom startup hooks beyond the default pool startup are skipped. Pool is shut down again once seeding finishes. |
| `test` | Yes (environment forced to `:test`) | **Yes**, same direct-call caveat as `db:seed` | Pool is shut down again after the test run. |
| `task` (custom tasks via `clails task ...`) | Yes | No, unless the task itself calls `startup-connection-pool` | The framework does not start the pool for tasks; a task that needs pooled DB access must start (and ideally shut down) the pool itself. |
| `server` | Yes | **Yes**, via `call-startup-hooks` | The only command that runs the full, user-configurable `*startup-hooks*` list. |
| `stop` | Yes | N/A | Only meaningful within the same running server process; a standalone invocation has nothing running to stop. |

### Practical implication

Code that reads `clails/environment:*connection-pool*` -- directly, or indirectly via `clails/model/connection:get-connection` / `with-db-connection` -- must not assume it has been populated just because the project has loaded (Stage 2). It is guaranteed only under `server`, `db:seed`, and `test`. If you write a custom task, migration helper, or other code path that needs a pooled database connection, call `clails/model/connection:startup-connection-pool` yourself first (and `shutdown-connection-pool` when done), or use a direct connection (`with-db-connection-direct`) instead. `clails/model/connection:get-connection` raises a clear error naming the missing initialization step instead of failing with an obscure error from inside the connection-pool library when `*connection-pool*` is `nil`.

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

**Resolution**:

The final value of `*project-environment*` is decided by layering up to three inputs, listed from lowest to highest precedence:

1. **default** - the value set directly in the project's `app/config/environment.lisp` (normally `:develop`).
2. **env-var** - the `CLAILS_ENV` environment variable, applied when the project boots (`clails.boot`).
3. **forced override** - a command that always forces a specific environment, e.g. the `test` command, which always forces `:test`.

This layering is consolidated into a single function, `clails/environment:resolve-project-environment`, instead of being duplicated at each of the call sites above. Every call logs which source determined the resulting value, e.g.:

```
project environment resolved to TEST (source: forced override)
```

#### `resolve-project-environment` Function

Resolves `*project-environment*` from whatever inputs are available at the call site and logs which source won.

```lisp
;; Called from clails.boot after the project's default is already set
(clails/environment:resolve-project-environment :env-var (uiop:getenv "CLAILS_ENV"))
;; => :develop, or the CLAILS_ENV value if it is set and valid

;; Called later by the `test` command to force the test environment
(clails/environment:resolve-project-environment :forced "test")
;; => :test, regardless of the default or CLAILS_ENV
```

**Parameters**:
- `env-var` [string or nil] - Optional value to resolve against (typically read from `CLAILS_ENV`). Overrides the current default when present and valid.
- `forced` [string or nil] - Optional forced override (e.g. `"test"`). Overrides both the default and `env-var` when present and valid.

**Return value**:
- [keyword] - The resolved `*project-environment*` value.

**Note**: This function only decides *which* value wins; it does not change *when* each input becomes available during startup. The default is still set while the project loads, `env-var` is still resolved in `clails.boot`, and a forced override (if any) still happens at its usual point in a command's execution.

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

Functions to execute at application startup, **in list order**. The
framework's own default (`clails/model/connection:startup-connection-pool`)
is already in this list, so any hook you register runs after it.

```lisp
clails/environment:*startup-hooks*
;; => ("clails/model/connection:startup-connection-pool" #<FUNCTION ...> ...)
```

**Type**: `list of functions (or function-name strings)`

**Usage**: Use `add-startup-hook` to register a hook. It appends to the list,
so hooks run in the order they were registered — do not `push` onto
`*startup-hooks*` directly, since `push` prepends and would run your hook
*before* the framework's default (and before any hook registered earlier).

```lisp
;; Add startup hooks (registration order = execution order)
(clails/environment:add-startup-hook
  #'(lambda ()
      (format t "Application starting...~%")
      (initialize-cache)
      (connect-external-services)))
```

#### `*shutdown-hooks*`

Functions to execute at application shutdown, **in list order**. The
framework's own default (`clails/model/connection:shutdown-connection-pool`)
is already in this list, so any hook you register runs after it.

```lisp
clails/environment:*shutdown-hooks*
;; => ("clails/model/connection:shutdown-connection-pool" #<FUNCTION ...> ...)
```

**Type**: `list of functions (or function-name strings)`

**Usage**: Use `add-shutdown-hook` to register a hook. Same append-only
behavior as `add-startup-hook` above.

```lisp
;; Add shutdown hooks (registration order = execution order)
(clails/environment:add-shutdown-hook
  #'(lambda ()
      (format t "Application shutting down...~%")
      (cleanup-cache)
      (disconnect-external-services)))
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

;; Add startup hooks (they run after the framework's own default,
;; clails/model/connection:startup-connection-pool, in the order registered)
(clails/environment:add-startup-hook "myapp/initializer:initialize-table-information")
(clails/environment:add-startup-hook "myapp/initializer:setup-logger")

;; Add shutdown hooks (they run after the framework's own default,
;; clails/model/connection:shutdown-connection-pool, in the order registered)
(clails/environment:add-shutdown-hook "myapp/finalizer:cleanup-resources")
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

### Internal-only Control Variables

A small number of special variables in `clails/environment` (for example
`*%sqlite3-transaction-mode*`, `*%sqlite3-lock-module-loaded*`,
`*%table-information-initialized*`, and `*%query-initialization-callbacks*`)
exist purely as internal control/bookkeeping state for clails itself — they
are rebound or mutated by macros and internal functions (such as
`with-locked-transaction`) and are **not** meant to be read or set from
application code.

These variables are named with a leading `%` (e.g. `*%sqlite3-transaction-mode*`)
to visibly distinguish them from ordinary, user-facing configuration variables
like `*routing-tables*` or `*sqlite3-busy-timeout*`, even though both kinds
are technically exported from the same package. If you are contributing to
clails itself and need to introduce a new internal-only control variable,
please follow this same `%`-prefix convention so the distinction stays clear
for future readers.

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

---

## 8. Contributing: Adding New Configuration Variables

If you are adding a new global variable to `src/environment.lisp` (or any other variable that a
project's `app/config/*.lisp` files are expected to set or override), always define it with
`defvar`, **never** with `defparameter`.

### Why this matters

`defparameter` unconditionally re-initializes the variable's value every time the containing
file is loaded, while `defvar` only sets the initial value if the variable is not already bound.
In a typical development workflow the application is started with the Swank server
(`--swank`) attached, and source files are reloaded from the REPL as you iterate. If a
configuration variable were defined with `defparameter`, every reload of `environment.lisp`
would silently reset it to its hard-coded default, discarding whatever value the project's
`app/config/database.lisp` or `app/config/environment.lisp` had set (for example
`*database-config*` or `*project-name*`). This exact bug happened in practice and was fixed by
switching the affected variable to `defvar`.

### Rule of thumb

- **Use `defvar`** for anything that a project's `app/config/*.lisp` files are expected to read,
  set, or override at startup (e.g. `*project-name*`, `*database-config*`, `*routing-tables*`,
  `*default-lock-mode*`). These represent user-facing configuration and must survive file
  reloads.
- **`defparameter` is still appropriate** for values that are genuinely internal and are never
  meant to be configured by a project — for example fixed constant tables (`+ENVIRONMENT-NAMES+`),
  internal caches that are safe (or even desirable) to reset on reload, or closures/data that are
  fully reconstructed from source and never touched by `app/config/*.lisp`.
- When in doubt, ask: "could a project's config file have already set this before the defining
  file gets reloaded?" If yes, use `defvar`.

This convention was adopted after an audit for [issue #156](https://github.com/tamurashingo/clails/issues/156)
confirmed all current variables in `src/environment.lisp` already follow it. As a possible
future improvement, introducing an explicit configuration-context object for major subsystems
(instead of relying on special variables at all) has been suggested, but is out of scope for
this guideline.
