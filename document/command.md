# clails Command Guide

## Overview

The clails command is a command-line tool that supports clails application development.
It provides features necessary for development such as project creation, code generation, database management, and server startup.

## Basic Concepts

- The clails command is defined in `roswell/clails.ros`
- Each command calls functions in `src/cmd.lisp`
- Implemented as a Roswell script, executable from anywhere
- Project-specific commands (generate, server, etc.) are executed from the project root

---

## Installation and Setup

### Installing Roswell

To use clails, you must first install Roswell.

```bash
# macOS (Homebrew)
brew install roswell

# Linux (Manual installation)
# See https://github.com/roswell/roswell/wiki/Installation
```

### Installing clails

```bash
# Install clails command
ros install tamurashingo/clails

# Verify installation
clails help
```

---

## Command List

### Project Management

| Command | Description |
|---------|-------------|
| `clails new` | Create a new project |
| `clails environment` | Display current environment |
| `clails server` | Start web server |
| `clails stop` | Stop web server |

### Code Generation

| Command | Description |
|---------|-------------|
| `clails generate:model` | Generate Model file |
| `clails generate:migration` | Generate Migration file |
| `clails generate:view` | Generate View file |
| `clails generate:controller` | Generate Controller file |
| `clails generate:scaffold` | Generate Model, View, and Controller together |

### Database

| Command | Description |
|---------|-------------|
| `clails db:create` | Create database |
| `clails db:migrate` | Run migrations |
| `clails db:status` | Display migration status |

---

## 1. Project Management Commands

### `clails new` - Create a New Project

Creates a new clails project.

#### Syntax

```bash
clails new PROJECT_NAME [OPTIONS]
```

#### Options

| Option | Short | Default | Description |
|--------|-------|---------|-------------|
| `--path PATH` | `-p PATH` | Current directory | Directory to create project |
| `--database DB` | `-d DB` | `sqlite3` | Database to use (sqlite3, mysql, postgresql) |

#### Examples

```bash
# Create project with default (SQLite3)
clails new myapp

# Create project with MySQL
clails new myapp -d mysql

# Create project in specific directory
clails new myapp -p /path/to/projects

# Use PostgreSQL
clails new myapp --database postgresql
```

#### Generated Directory Structure

```
myapp/
├── app/
│   ├── application.lisp
│   ├── controllers/
│   ├── models/
│   └── views/
├── config/
│   ├── database.lisp
│   └── routes.lisp
├── db/
│   └── migrate/
├── clails.boot
├── myapp.asd
└── README.md
```

### `clails environment` - Display Current Environment

Displays the current project environment (development, test, production).

#### Syntax

```bash
clails environment
```

#### Examples

```bash
cd myapp
clails environment
# => environment: DEVELOPMENT
```

### `clails server` - Start Web Server

Starts the development web server.

#### Syntax

```bash
clails server [OPTIONS]
```

#### Options

| Option | Short | Default | Description |
|--------|-------|---------|-------------|
| `--port PORT` | `-p PORT` | `5000` | Server port number |
| `--bind ADDRESS` | `-b ADDRESS` | `127.0.0.1` | IP address to bind |

#### Examples

```bash
# Start server with default (localhost:5000)
clails server

# Specify port number
clails server -p 8080

# Listen on all interfaces
clails server -b 0.0.0.0

# Specify both port and address
clails server -p 8080 -b 0.0.0.0
```

#### Server Startup Behavior

1. Initialize routing tables
2. Build middleware stack
3. Start Clack server
4. Execute startup hooks

#### How to Stop

```bash
# In a separate terminal
clails stop

# Or Ctrl+C
```

### `clails stop` - Stop Web Server

Stops the running web server.

#### Syntax

```bash
clails stop
```

#### Examples

```bash
clails stop
```

---

## 2. Code Generation Commands

### `clails generate:model` - Generate Model File

Generates a Model file and optionally a Migration file.

#### Syntax

```bash
clails generate:model MODEL_NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--no-overwrite` | None | Do not overwrite existing files (default: enabled) |
| `--no-migration` | `-n` | Do not generate Migration file |

#### Examples

```bash
# Generate Model and Migration
clails generate:model user

# Generate Model only without Migration
clails generate:model user -n

# Generate with overwrite
clails generate:model user --no-overwrite=false
```

#### Generated Files

```
app/models/user.lisp          # Model file
db/migrate/YYYYMMDD-HHMMSS-user.lisp  # Migration file (without -n)
```

#### Generated Model Example

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/models/user
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel))

(in-package #:myapp/models/user)

(defmodel <user> (<base-model>)
  (:table "users"))
```

### `clails generate:migration` - Generate Migration File

Generates a database Migration file.

#### Syntax

```bash
clails generate:migration MIGRATION_NAME
```

#### Examples

```bash
# Generate Migration
clails generate:migration add-email-to-users

# Generate with timestamp
clails generate:migration create-posts-table
```

#### Generated File

```
db/migrate/YYYYMMDD-HHMMSS-add-email-to-users.lisp
```

#### Generated Migration Example

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/db/migrate/add-email-to-users
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table
                #:add-column
                #:drop-column))

(in-package #:myapp/db/migrate/add-email-to-users)

(defmigration "YYYYMMDD-HHMMSS-add-email-to-users"
  (:up #'(lambda (conn)
           ;; Add your migration code here
           ))
  (:down #'(lambda (conn)
             ;; Add your rollback code here
             )))
```

### `clails generate:view` - Generate View File

Generates a View file and package definition.

#### Syntax

```bash
clails generate:view VIEW_PATH
```

#### Examples

```bash
# Generate View
clails generate:view users/index

# Generate with subdirectories
clails generate:view admin/users/list
```

#### Generated Files

```
app/views/users/index.html       # View file
app/views/users/package.lisp     # Package definition (if not exists)
```

#### Generated View Example

```html
<!DOCTYPE html>
<html>
<head>
  <title>Users Index</title>
</head>
<body>
  <h1>Users Index</h1>
  <!-- Your view content here -->
</body>
</html>
```

### `clails generate:controller` - Generate Controller File

Generates a Controller file.

#### Syntax

```bash
clails generate:controller CONTROLLER_NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--type TYPE` | `-t TYPE` | Controller type (web, rest, base) |
| `--no-overwrite` | None | Do not overwrite existing files |

#### Examples

```bash
# Generate web Controller
clails generate:controller users

# Generate REST API Controller
clails generate:controller api/users -t rest

# Generate base Controller
clails generate:controller admin/dashboard -t base
```

#### Generated File

```
app/controllers/users_controller.lisp
```

#### Generated Controller Example

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/controllers/users-controller
  (:use #:cl)
  (:import-from #:clails/controller/web-controller
                #:<web-controller>
                #:set-view
                #:set-redirect
                #:param))

(in-package #:myapp/controllers/users-controller)

(defclass <users-controller> (<web-controller>)
  ()
  (:documentation "Users controller"))

;; GET /users
(defmethod do-get ((controller <users-controller>))
  (set-view controller "users/index.html" '()))

;; POST /users
(defmethod do-post ((controller <users-controller>))
  (set-redirect controller "/users"))
```

### `clails generate:scaffold` - Generate Scaffold

Generates Model, View, Controller, and Migration files together.

#### Syntax

```bash
clails generate:scaffold RESOURCE_NAME
```

#### Examples

```bash
# Generate scaffold
clails generate:scaffold post

# Generate scaffold for nested resource
clails generate:scaffold admin/article
```

#### Generated Files

```
app/models/post.lisp
app/controllers/post_controller.lisp
app/views/post/index.html
app/views/post/show.html
app/views/post/new.html
app/views/post/edit.html
db/migrate/YYYYMMDD-HHMMSS-create-posts-table.lisp
```

---

## 3. Database Commands

### `clails db:create` - Create Database

Creates the database based on configuration.

#### Syntax

```bash
clails db:create
```

#### Examples

```bash
clails db:create
# => Creating database...
# => Database created successfully
```

#### Behavior

Creates the following databases based on the settings in `config/database.lisp`:

- SQLite3: Create database file
- MySQL: Execute `CREATE DATABASE`
- PostgreSQL: Execute `CREATE DATABASE`

### `clails db:migrate` - Run Migrations

Executes pending migrations.

#### Syntax

```bash
clails db:migrate
```

#### Alias

```bash
clails db:migrate:up
```

#### Examples

```bash
clails db:migrate
# => Running migration: 20241022-143000-create-users-table
# => Migration completed successfully
```

#### Behavior

1. Scan Migration files in `db/migrate/` directory
2. Sort pending migrations by execution order
3. Execute `:up` function of each Migration
4. Save execution records to Migration table

### `clails db:status` - Display Migration Status

Displays the execution status of migrations.

#### Syntax

```bash
clails db:status
```

#### Examples

```bash
clails db:status
# => Migration Status:
# => [X] 20241022-143000-create-users-table
# => [X] 20241022-144500-add-email-to-users
# => [ ] 20241022-150000-add-index-to-users-email
```

#### Output Format

- `[X]` - Executed
- `[ ]` - Pending

---

## 4. Common Usage Patterns

### Starting a New Project

```bash
# 1. Create project
clails new myapp -d postgresql
cd myapp

# 2. Create database
clails db:create

# 3. Run initial Migration
clails db:migrate

# 4. Start server
clails server
```

### Development with Models

```bash
# 1. Generate Model and Migration
clails generate:model user

# 2. Edit Migration file to define columns
# Edit db/migrate/YYYYMMDD-HHMMSS-user.lisp

# 3. Run Migration
clails db:migrate

# 4. Implement code using Model
```

### Creating REST API

```bash
# 1. Generate Controller
clails generate:controller api/users

# 2. Implement REST API logic in Controller
# Edit app/controllers/api/users_controller.lisp

# 3. Configure routing
# Edit config/routes.lisp

# 4. Start server and test
clails server
```

### Rapid Development with Scaffold

```bash
# 1. Generate scaffold
clails generate:scaffold post

# 2. Run Migration
clails db:migrate

# 3. Customize generated files
# app/models/post.lisp
# app/controllers/post_controller.lisp
# app/views/post/*.html

# 4. Start server
clails server
```

---

## 5. Command Options

### Common Options

Options commonly available for many commands.

#### `--no-overwrite`

Prevents overwriting of existing files. Enabled by default.

```bash
# Do not overwrite existing files (default)
clails generate:model user

# Specify explicitly
clails generate:model user --no-overwrite

# To allow overwrite (Warning: may lose data)
# Currently, there is no option to force overwrite
```

---

## 6. Troubleshooting

### Command Not Found

```bash
# If clails command is not found
# Check Roswell path
echo $PATH

# Check Roswell installation
which ros

# Reinstall clails
ros install tamurashingo/clails
```

### Cannot Load Project

```bash
# Verify you are in project root
pwd

# Verify clails.boot file exists
ls clails.boot

# Reinstall dependencies
rm -rf .qlot
qlot install
```

### Cannot Run Migration

```bash
# Check database connection
# Verify config/database.lisp settings

# Check if database is created
clails db:create

# Check for syntax errors in Migration files
# Verify db/migrate/*.lisp files
```

### Server Won't Start

```bash
# Check if port is in use
lsof -i :5000

# Start on different port
clails server -p 8080

# Check error logs
# Review error messages in terminal output
```

---

## 7. Advanced Usage

### Adding Custom Commands

The clails command is extensible. You can add custom commands by editing `roswell/clails.ros`.

```common-lisp
;; Add to roswell/clails.ros
(defun custom/command ()
  (load-project)
  (clails/cmd:custom/command))

;; Add to *config*
(:command "custom:command"
 :function ,#'custom/command)
```

### Startup and Shutdown Hooks

You can execute arbitrary processes when starting or stopping the server.

```common-lisp
;; Define in config/environment.lisp or similar
(setf clails/environment:*startup-hooks*
      (list #'(lambda ()
                (format t "Server starting...~%"))))

(setf clails/environment:*shutdown-hooks*
      (list #'(lambda ()
                (format t "Server stopping...~%"))))
```

---

## Summary

The clails command has the following features:

1. **Unified Interface**: Execute all operations via `clails` command
2. **Auto-generation**: Automatically generate code for Models, Views, Controllers, etc.
3. **Database Management**: Schema version control through Migrations
4. **Development Server**: Built-in web server for immediate testing
5. **Extensibility**: Extendable with custom commands and hooks

For detailed API reference, see the command implementation in `src/cmd.lisp`.
