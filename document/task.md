# clails Task Guide

## Overview

This guide explains the clails task system.
The task system provides functionality to define and execute application-specific operations such as database operations, data seeding, cleanup, and more.

## Basic Concepts

- Tasks are defined using the `deftask` macro
- Related tasks can be grouped using the `defnamespace` macro
- Tasks are executed with the `clails task` command
- Tasks can define dependencies, which are automatically executed before the task
- Task files are placed in the `lib/tasks/` directory
- Custom tasks are automatically loaded when the application starts

---

## 1. Task Project Structure

In a clails project, task files are organized as follows:

```
your-app/
├── app/
│   ├── controllers/
│   ├── models/
│   └── views/
├── lib/
│   └── tasks/           # Custom task directory
│       ├── cleanup.lisp  # Sample task
│       └── seed.lisp     # Data seeding tasks, etc.
├── your-app.asd
└── your-app-test.asd
```

### Task File Naming Conventions

- Task files use the `.lisp` extension
- File names should be descriptive of their content
- Can be placed in any directory under `lib/tasks/` (including subdirectories)

---

## 2. Defining Tasks

### Basic Task Definition with deftask

Use the `deftask` macro to define tasks:

```common-lisp
(defpackage #:your-app/tasks/cleanup
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/cleanup)

(deftask :cleanup
  :description "Clean up temporary files"
  :function (lambda ()
              (log.task "Task started" :task-name "cleanup")
              (format t "Cleaning up temporary files~%")
              ;; Implement cleanup logic here
              (log.task "Task completed" :task-name "cleanup")))
```

### Task Definition with Namespaces

Use the `defnamespace` macro to group related tasks:

```common-lisp
(defpackage #:your-app/tasks/db-tasks
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask
                #:defnamespace)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/db-tasks)

(defnamespace :db
  (deftask :seed
    :description "Load seed data into database"
    :function (lambda ()
                (log.task "Task started" :task-name "db:seed")
                (format t "Loading seed data~%")
                ;; Implement seed logic here
                (log.task "Task completed" :task-name "db:seed")))

  (deftask :reset
    :description "Reset database"
    :function (lambda ()
                (log.task "Task started" :task-name "db:reset")
                (format t "Resetting database~%")
                ;; Implement reset logic here
                (log.task "Task completed" :task-name "db:reset"))))
```

### Task Dependencies

Tasks can define dependencies. Dependent tasks are automatically executed first:

```common-lisp
(deftask :setup
  :description "Setup application"
  :function (lambda ()
              (format t "Setting up application~%")))

(deftask :deploy
  :description "Deploy application"
  :depends-on (:setup)
  :function (lambda ()
              (format t "Deploying application~%")))

;; Dependencies on namespaced tasks
(deftask :full-deploy
  :description "Full deployment with database setup"
  :depends-on ((:db :migrate) (:db :seed) :deploy)
  :function (lambda ()
              (format t "Full deployment completed~%")))
```

### Tasks with Arguments

Tasks can accept arguments:

```common-lisp
(deftask :import-data
  :description "Import data from file"
  :args (&key (file "data.csv") (verbose nil))
  :function (lambda (&key (file "data.csv") (verbose nil))
              (format t "Importing data from: ~A~%" file)
              (when verbose
                (format t "Verbose mode enabled~%"))
              ;; Implement data import logic here
              ))
```

---

## 3. Generating Tasks

### `clails generate:task` - Generate Task File

Generate a new task file.

#### Syntax

```bash
clails generate:task TASK_NAME [OPTIONS]
```

#### Options

| Option | Short | Description |
|--------|-------|-------------|
| `--namespace NS` | `-ns NS` | Specify task namespace |

#### Usage Examples

```bash
# Generate a simple task
clails generate:task cleanup

# Generate a namespaced task
clails generate:task seed -ns db
clails generate:task import --namespace data
```

#### Generated Files

```
# Simple task
lib/tasks/cleanup.lisp

# Namespaced tasks
lib/tasks/db/seed.lisp
lib/tasks/data/import.lisp
```

---

## 4. Running Tasks

### Executing Tasks

Execute tasks with the `clails task` command:

```bash
# Run a simple task
clails task cleanup

# Run a namespaced task
clails task db:seed
clails task db:migrate
```

### Listing Available Tasks

View available tasks:

```bash
# List all tasks
clails task --list

# List tasks in a specific namespace
clails task --list db
```

Sample output:

```
Available tasks:

Global tasks:
  cleanup              Clean up temporary files

db:
  db:seed              Load seed data into database
  db:reset             Reset database
  db:migrate           Run database migrations
```

### Displaying Task Information

View detailed task information:

```bash
# Show task details
clails task --info cleanup
clails task --info db:seed
```

Sample output:

```
Task: db:seed
Description: Load seed data into database
Dependencies: (:create :migrate)
```

---

## 5. Task System Implementation Details

### Task Registry

Tasks are managed by the `clails/task/registry` package:

- `register-task`: Register a task
- `find-task`: Search for a task
- `list-tasks`: Get list of tasks
- `list-namespaces`: Get list of namespaces
- `load-custom-tasks`: Load custom task files

### Task Runner

Task execution is handled by the `clails/task/runner` package:

- Automatic dependency resolution
- Task idempotency (same task executes only once per run)
- Error handling and logging

### Task Information Class

Each task is represented by the `<task-info>` class:

- `name`: Task name
- `namespace`: Namespace (optional)
- `description`: Description text
- `depends-on`: List of dependent tasks
- `args`: Argument list
- `function`: Function to execute

---

## 6. Practical Examples

### Database Seed Task

```common-lisp
(defpackage #:your-app/tasks/db-seed
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask
                #:defnamespace)
  (:import-from #:clails/logger/core
                #:log.task)
  (:import-from #:your-app/models/user
                #:<user>)
  (:import-from #:clails/model/core
                #:make-record
                #:save))

(in-package #:your-app/tasks/db-seed)

(defnamespace :db
  (deftask :seed
    :description "Load initial user data"
    :depends-on ((:db :migrate))
    :function (lambda ()
                (log.task "Task started" :task-name "db:seed")
                
                ;; Create user data
                (let ((users '(("Alice" "alice@example.com")
                             ("Bob" "bob@example.com")
                             ("Charlie" "charlie@example.com"))))
                  (dolist (user-data users)
                    (let ((user (make-record '<user>
                                            :name (first user-data)
                                            :email (second user-data))))
                      (save user)
                      (format t "Created user: ~A~%" (first user-data)))))
                
                (log.task "Task completed" :task-name "db:seed"))))
```

Execution:

```bash
clails task db:seed
```

### Log File Cleanup Task

```common-lisp
(defpackage #:your-app/tasks/log-cleanup
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task)
  (:import-from #:uiop
                #:delete-file-if-exists))

(in-package #:your-app/tasks/log-cleanup)

(deftask :log-cleanup
  :description "Remove old log files"
  :args (&key (days 30))
  :function (lambda (&key (days 30))
              (log.task "Task started" 
                       :task-name "log-cleanup"
                       :days days)
              
              (format t "Removing log files older than ~A days~%" days)
              
              ;; Log file deletion logic
              (let ((log-dir (merge-pathnames "logs/" (uiop:getcwd)))
                    (cutoff-time (- (get-universal-time)
                                   (* days 24 60 60))))
                (dolist (file (uiop:directory-files log-dir "*.log"))
                  (when (< (file-write-date file) cutoff-time)
                    (delete-file-if-exists file)
                    (format t "Deleted: ~A~%" file))))
              
              (log.task "Task completed" :task-name "log-cleanup")))
```

Execution:

```bash
# Default (30 days)
clails task log-cleanup

# Custom days (keyword arguments currently unsupported, planned for future)
```

### Report Generation Task

```common-lisp
(defpackage #:your-app/tasks/report
  (:use #:cl)
  (:import-from #:clails/task/core
                #:deftask)
  (:import-from #:clails/logger/core
                #:log.task))

(in-package #:your-app/tasks/report)

(deftask :generate-report
  :description "Generate monthly report"
  :depends-on (:backup-data)
  :function (lambda ()
              (log.task "Task started" :task-name "generate-report")
              
              (format t "Generating monthly report~%")
              
              ;; Report generation logic
              (let ((report-file (format nil "report-~A.txt"
                                        (local-time:format-timestring
                                         nil
                                         (local-time:now)
                                         :format '(:year :month :day)))))
                (with-open-file (out report-file
                                    :direction :output
                                    :if-exists :supersede)
                  (format out "Monthly Report~%")
                  (format out "Generated: ~A~%"
                         (local-time:format-timestring nil (local-time:now))))
                (format t "Report saved to: ~A~%" report-file))
              
              (log.task "Task completed" :task-name "generate-report")))

(deftask :backup-data
  :description "Backup application data"
  :function (lambda ()
              (format t "Backing up data~%")
              ;; Backup logic
              ))
```

Execution:

```bash
clails task generate-report
```

In this case, since `generate-report` depends on `backup-data`,
`backup-data` will be executed first.

---

## 7. Best Practices

### Task Design Tips

1. **Single Responsibility Principle**: Each task should do one thing
2. **Idempotency**: Design tasks to be safe when executed multiple times
3. **Logging**: Log task start and completion
4. **Error Handling**: Provide appropriate error messages

### Task Organization

1. **Use Namespaces**: Group related tasks with namespaces
2. **Split Files**: Divide large task sets into multiple files
3. **Explicit Dependencies**: Clearly define dependencies between tasks

### Performance Considerations

1. **Long-Running Tasks**: Display progress
2. **Database Access**: Use appropriate transaction management
3. **Batch Processing**: Process large data in chunks

---

## 8. Troubleshooting

### Task Not Found

```
Error: Task not found: mytask
```

**Solutions**:
- Verify task file is placed in `lib/tasks/`
- Check that task name and namespace are correct
- Ensure task is properly defined with `deftask`

### Dependency Task Not Found

```
Dependency task not found: :setup
```

**Solutions**:
- Verify dependent task is defined
- Check that dependent task's file is loaded
- Dependencies on namespaced tasks should use `(:namespace :task-name)` format

### Task File Loading Error

```
Loading task file: /path/to/task.lisp ... failed: <error>
```

**Solutions**:
- Check task file for syntax errors
- Verify required packages are declared with `:import-from`
- Ensure Common Lisp syntax is correct

---

## 9. Summary

The clails task system is a powerful feature for easily defining and executing application-specific operations:

- Easy task definition with `deftask` macro
- Group related tasks with `defnamespace`
- Automatic dependency resolution
- Simple execution with `clails task` command
- Built-in support for logging and error handling

Leverage the task system to efficiently execute various batch processes and maintenance tasks such as
database setup, data seeding, cleanup, report generation, and more.
