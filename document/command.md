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

#### Commands

```bash
# macOS (Homebrew)
brew install roswell

# Linux (Manual installation)
# See https://github.com/roswell/roswell/wiki/Installation
```

### Installing Required Libraries

```bash
ros install fukamachi/cl-dbi
ros install tamurashingo/cl-dbi-connection-pool
ros install tamurashingo/cl-batis
ros install tamurashingo/getcmd
```

### Installing clails

```bash
ros install tamurashingo/clails
```

To specify a specific branch or tag, use `/` to specify it.

```bash
# branch
ros install tamurashingo/clails/release/0.0.2

# tag
ros install tamurashingo/clails/v0.0.2
```

#### Verify Installation

```bash
clails --help
```

---

## Command List

### Project Management

| Command | Description |
|---------|-------------|
| `clails new` | Create a new project |
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
| `clails generate:task` | Generate Task file |

### Database

| Command | Description |
|---------|-------------|
| `clails db:create` | Create database |
| `clails db:migrate` | Run migrations |
| `clails db:migrate:up` | Run specific migration up |
| `clails db:migrate:down` | Rollback specific migration |
| `clails db:rollback` | Rollback migrations |
| `clails db:seed` | Seed the database |
| `clails db:status` | Display migration status |

### Task Management

| Command | Description |
|---------|-------------|
| `clails task` | Execute custom tasks |

### Testing

| Command | Description |
|---------|-------------|
| `clails test` | Run tests |

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
| `--swank` | `-s` | None | Start swank server (127.0.0.1:4005) |
| `--swank-address ADDRESS` | None | `127.0.0.1` | Swank server bind address |
| `--swank-port PORT` | None | `4005` | Swank server port number |

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

# Start swank server simultaneously (for REPL connection)
clails server --swank

# Specify swank server address and port
clails server --swank --swank-address 0.0.0.0 --swank-port 4006
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
# Migration for creating table
clails generate:migration create-users-table

# Migration for adding column
clails generate:migration add-email-to-users

# Migration for adding index
clails generate:migration add-index-to-users-email
```

#### Generated File

```
db/migrate/YYYYMMDD-HHMMSS-MIGRATION_NAME.lisp
```

Example: `db/migrate/20241022-143000-create-users-table.lisp`

#### Generated Migration Example

```common-lisp
(in-package #:myapp/db/migrate)

(defmigration "20241022-143000-create-users-table"
  (:up #'(lambda (conn)
           ;; Write table creation code here
           ))
  (:down #'(lambda (conn)
             ;; Write rollback code here
             )))
```

### `clails generate:view` - Generate View File

Generates a View file and package definition.

#### Syntax

```bash
clails generate:view VIEW_NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--no-overwrite` | None | Do not overwrite existing files (default: enabled) |

#### Examples

```bash
# Generate top-level View
clails generate:view index

# Generate nested View
clails generate:view users/index

# Generate View with multiple hierarchies
clails generate:view admin/users/list
```

#### Generated Files

```
app/views/VIEW_NAME.html       # View template
app/views/package.lisp         # Package definition (top level)
app/views/DIRECTORY/package.lisp  # Package definition (per directory)
```

#### Generated View Example

`app/views/users/index.html`:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Users Index</title>
</head>
<body>
  <h1>Users Index</h1>
</body>
</html>
```

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view))

(in-package #:myapp/views/users/package)
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
| `--no-overwrite` | None | Do not overwrite existing files (default: enabled) |

#### Examples

```bash
# Generate Controller
clails generate:controller users

# Generate nested Controller
clails generate:controller admin/users
```

#### Generated File

```
app/controllers/CONTROLLER_NAME_controller.lisp
```

#### Generated Controller Example

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/controllers/users-controller
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:do-get
                #:do-post
                #:do-put
                #:do-delete
                #:set-view
                #:set-redirect
                #:param))

(in-package #:myapp/controllers/users-controller)

(defclass <users-controller> (<web-controller>)
  ()
  (:documentation "Users controller"))

(defmethod do-get ((controller <users-controller>))
  ;; GET request processing
  (set-view controller "users/index.html" '()))

(defmethod do-post ((controller <users-controller>))
  ;; POST request processing
  (set-redirect controller "/users"))
```

### `clails generate:task` - Generate Task File

Generates a task file.

#### Syntax

```bash
clails generate:task TASK_NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--namespace NS` | `-ns NS` | Specify namespace for task |
| `--no-overwrite` | None | Do not overwrite existing files (default: enabled) |

#### Examples

```bash
# Generate task
clails generate:task cleanup

# Generate task with namespace
clails generate:task import --namespace data

# Use short form for namespace
clails generate:task cleanup -ns maintenance
```

#### Generated File

```
app/tasks/NAMESPACE/TASK_NAME.lisp
```

Example: `app/tasks/maintenance/cleanup.lisp`

#### Generated Task Example

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/tasks/maintenance/cleanup
  (:use #:cl)
  (:import-from #:clails/task
                #:deftask))

(in-package #:myapp/tasks/maintenance/cleanup)

(deftask "maintenance:cleanup"
  :description "Task description here"
  :function #'(lambda (&rest args)
                ;; Task implementation here
                ))
```

### `clails generate:scaffold` - Generate Scaffold

Generates Model, View, Controller, and Migration files together.

#### Syntax

```bash
clails generate:scaffold NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--no-overwrite` | None | Do not overwrite existing files (default: enabled) |

#### Examples

```bash
# Generate User scaffold
clails generate:scaffold user
```

#### Generated Files

```
app/models/NAME.lisp
app/controllers/NAME_controller.lisp
app/views/NAME/index.html
app/views/NAME/show.html
app/views/NAME/new.html
app/views/NAME/edit.html
app/views/NAME/package.lisp
db/migrate/YYYYMMDD-HHMMSS-NAME.lisp
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
clails db:migrate [OPTIONS]
```

#### Options

| Option | Description |
|--------|-------------|
| `--version VERSION` | Run migrations up to specific version |

#### Examples

```bash
# Run all pending migrations
clails db:migrate

# Run migrations up to specific version
clails db:migrate --version 20241022143000
```

#### Behavior

1. Scan Migration files in `db/migrate/` directory
2. Sort pending migrations in execution order
3. Execute `:up` function for each migration
4. Save execution record in migration table

### `clails db:migrate:up` - Run Specific Migration Up

Executes a specific version migration.

#### Syntax

```bash
clails db:migrate:up VERSION
```

#### Examples

```bash
clails db:migrate:up 20241022143000
```

### `clails db:migrate:down` - Rollback Specific Migration

Rolls back a specific version migration.

#### Syntax

```bash
clails db:migrate:down VERSION
```

#### Examples

```bash
clails db:migrate:down 20241022143000
```

### `clails db:rollback` - Rollback Migrations

Rolls back the latest migration.

#### Syntax

```bash
clails db:rollback [OPTIONS]
```

#### Options

| Option | Default | Description |
|--------|---------|-------------|
| `--step N` | `1` | Number of migrations to rollback |

#### Examples

```bash
# Rollback 1 latest migration
clails db:rollback

# Rollback 3 latest migrations
clails db:rollback --step 3
```

### `clails db:seed` - Seed the Database

Seeds the database with data.

#### Syntax

```bash
clails db:seed
```

#### Examples

```bash
clails db:seed
# => Seeding database...
# => Seed completed successfully
```

#### Behavior

Executes the seed process defined in `db/seed.lisp`.

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

## 4. Task Management Commands

### `clails task` - Execute Custom Tasks

Executes custom tasks.

#### Syntax

```bash
clails task [TASK] [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--list [NAMESPACE]` | `-l [NAMESPACE]` | List available tasks (optionally filter by namespace) |
| `--info <task>` | None | Show detailed information about a task |

#### Examples

```bash
# List all tasks
clails task --list

# List tasks in specific namespace
clails task --list db

# Show task details
clails task --info maintenance:cleanup

# Execute custom task
clails task maintenance:cleanup
```

---

## 5. Testing Commands

### `clails test` - Run Tests

Runs tests.

#### Syntax

```bash
clails test [PACKAGES...] [OPTIONS]
```

#### Options

| Option | Description |
|--------|-------------|
| `--tag TAG` | Include tests with TAG (can be specified multiple times) |
| `--exclude TAG` | Exclude tests with TAG (can be specified multiple times) |
| `--list-tags` | List all available tags |
| `--list-packages` | List all available packages |
| `--list-tests-tag TAG` | List tests with specific tag |
| `--list-tests-pkg PKG` | List tests in specific package |

#### Examples

```bash
# Run all tests
clails test

# Run tests in specific packages
clails test pkg1 pkg2

# Run tests with specific tag
clails test --tag model

# Run tests with multiple tags
clails test --tag model --tag sqlite3

# Exclude tests with specific tag
clails test --exclude slow

# List all available tags
clails test --list-tags

# Test specific package
clails test todoapp/models/user
```

---

## 6. Common Usage Patterns

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

### Batch Processing with Tasks

```bash
# 1. Generate task
clails generate:task cleanup --namespace maintenance

# 2. Implement task
# Edit app/tasks/maintenance/cleanup.lisp

# 3. Execute task
clails task maintenance:cleanup

# 4. Check available tasks
clails task --list
```

### Running Tests

```bash
# Run all tests
clails test

# Test specific package
clails test myapp/models/user

# Filter tests by tags
clails test --tag model --exclude slow
```

---

## 7. Command Options

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

## 8. Troubleshooting

### Command Not Found

```bash
# If clails command is not found
# Verify that ~/.roswell/bin is in PATH
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

## 9. Advanced Usage

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

### Development with Swank Server

By starting the swank server, you can connect to your application via REPL from Emacs/SLIME or Vim/SLIMV.

```bash
# Start web server with swank server
clails server --swank

# Connect from Emacs
M-x slime-connect RET 127.0.0.1 RET 4005 RET

# Start on different address/port
clails server --swank --swank-address 0.0.0.0 --swank-port 4006
```

---

## Summary

The clails command has the following features:

1. **Unified Interface**: Execute all operations via `clails` command
2. **Auto-generation**: Automatically generate code for Models, Views, Controllers, Tasks, etc.
3. **Database Management**: Schema version control and rollback functionality through Migrations
4. **Development Server**: Built-in web server for immediate testing
5. **Swank Server**: Supports live coding via REPL
6. **Task System**: Execute batch processing with custom tasks
7. **Test Framework**: Test execution with tag and package filtering
8. **Extensibility**: Extendable with custom commands and hooks

For detailed API reference, see the command implementation in `src/cmd.lisp`.
