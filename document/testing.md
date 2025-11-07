# clails Testing Guide

## Overview

The clails testing framework uses **Rove**, a Common Lisp testing framework that provides simple and expressive test syntax.
Tests are organized by component (model, controller, view, helper, etc.) and can be run using Docker-based environments that support multiple databases.

## Basic Concepts

- Tests use the Rove testing framework
- Test files mirror the source code structure (e.g., `test/model/` for model tests)
- Tests support multiple database backends (SQLite3, MySQL, PostgreSQL)
- Docker-based test environments ensure consistency and isolation
- Makefile commands simplify test execution

---

## 1. Test Structure

### File Organization

Tests are organized in the `test/` directory with the same structure as the source code:

```
test/
├── controller/         # Controller tests
├── helper/             # Helper function tests
├── model/              # Model and database tests
├── view/               # View template tests
├── datetime/           # DateTime utility tests
├── logger/             # Logger tests
├── e2e/                # End-to-end integration tests
└── util.lisp           # Test utilities
```

### Test File Naming

Test files follow the same naming as the source files they test:

- Source: `src/model/query.lisp` → Test: `test/model/query.lisp`
- Source: `src/helper/date-helper.lisp` → Test: `test/helper/date-helper.lisp`

---

## 2. Writing Tests with Rove

### Basic Test Structure

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/component-name
  (:use #:cl
        #:rove
        #:your-app/component-name))

(in-package #:your-app-test/component-name)

(deftest test-name
  (testing "description of test case"
    (ok (= 1 1)
        "1 equals 1")
    (ng (= 1 2)
        "1 does not equal 2")))
```

### Rove Assertions

Rove provides several assertion functions:

#### `ok` - Assert that expression is truthy

```common-lisp
(ok (= 2 (+ 1 1))
    "1 + 1 equals 2")
```

#### `ng` - Assert that expression is falsy

```common-lisp
(ng (= 3 (+ 1 1))
    "1 + 1 does not equal 3")
```

#### `signals` - Assert that code signals an error

```common-lisp
(ok (signals
      (error "test error"))
    "signals an error")
```

### Setup and Teardown

Use `setup` and `teardown` for test initialization and cleanup:

```common-lisp
(setup
  ;; Initialization code
  (initialize-database)
  (seed-test-data))

(teardown
  ;; Cleanup code
  (cleanup-database))

(deftest my-test
  ;; Test runs between setup and teardown
  (ok (test-something)))
```

---

## 3. Running Tests

### Prerequisites

Before running tests, ensure you have:

- Docker and Docker Compose installed
- Make utility installed

### Running All Tests

Build and run all tests:

```bash
make test.build
make test
```

This will:
1. Build the test Docker image
2. Start database containers (MySQL, PostgreSQL, SQLite3)
3. Install dependencies with qlot
4. Run all tests using Rove

### Cleaning Test Environment

Clean up test containers and images:

```bash
make test.down    # Stop database containers
make test.clean   # Remove containers and volumes
```

---

## 4. Testing Models

Model tests typically involve database operations and require database setup.

### Example Model Test

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/model/save
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:save
                #:destroy))

(in-package #:clails-test/model/save)

(setup
  ;; Define test models
  (defmodel <user> (<base-model>)
    (:table "users"))
  
  ;; Initialize database
  (initialize-database)
  (migrate-database))

(teardown
  (cleanup-database))

(deftest save-record-test
  (testing "saving a new record"
    (let ((user (make-record '<user> :name "John")))
      (ok (save user)
          "saves successfully")
      (ok (ref user :id)
          "record has an ID after save"))))
```

### Database Configuration

Tests can be configured for different databases using environment variables:

```common-lisp
(setf clails/environment:*database-type* 
      (make-instance 'clails/environment::<database-type-mysql>))

(setf clails/environment:*database-config* 
      `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
               :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
               :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
               :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
               :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
```

---

## 5. Testing Controllers

Controller tests verify request handling, parameter processing, and response generation.

### Example Controller Test

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/controller/base-controller
  (:use #:cl
        #:rove
        #:clails/controller/base-controller))

(in-package #:clails-test/controller/base-controller)

(defclass <test-controller> (<base-controller>)
  ())

(deftest path-matching-test
  (testing "matches root path"
    (let ((result (match-path "/")))
      (ok result
          "matches root path")))
  
  (testing "matches parameterized path"
    (let ((result (match-path "/users/123")))
      (ok result
          "matches path with parameter")
      (ok (string= (getf result :id) "123")
          "extracts parameter correctly"))))
```

---

## 6. Testing Views

View tests verify template parsing, compilation, and rendering.

### Example View Test

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/view/renderer
  (:use #:cl
        #:rove
        #:clails/view/renderer))

(in-package #:clails-test/view/renderer)

(deftest render-template-test
  (testing "renders simple template"
    (let ((result (render-string "<p>Hello, <%= name %>!</p>"
                                  :name "World")))
      (ok (string= result "<p>Hello, World!</p>")
          "renders template with variables"))))
```

---

## 7. Testing Helpers

Helper tests verify utility functions and helper methods.

### Example Helper Test

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/helper/date-helper
  (:use #:cl
        #:rove
        #:clails/helper/date-helper))

(in-package #:clails-test/helper/date-helper)

(deftest datetime-format-test
  (let ((ut (encode-universal-time 45 34 13 02 01 1998)))
    (ok (string= (view/datetime ut)
                 "1998/01/02 13:34:45")
        "formats datetime with default format")
    
    (ok (string= (view/datetime ut :fmt "%Y")
                 "1998")
        "formats datetime with custom format")))
```

---

## 8. End-to-End (E2E) Testing

E2E tests verify complete application workflows by creating and testing a full application.

### Running E2E Tests

```bash
make e2e.build   # Build E2E test image
make e2e.test    # Run E2E tests
make e2e.clean   # Clean up E2E environment
```

### E2E Test Structure

E2E tests are defined in `test/e2e/` and include:

- `todo-app-e2e.sh` - Shell script that creates and tests a complete application
- `templates/` - Template files for the test application

The E2E test:
1. Creates a new clails application
2. Generates scaffolding
3. Creates migrations and seeds data
4. Runs the application
5. Verifies functionality

---

## 9. Database-Specific Testing

### Testing with SQLite3

```bash
make test.sqlite3
```

This opens an SQLite3 console connected to the test database.

### Testing with MySQL

```bash
make test.mysql
```

This opens a MySQL console connected to the test database.

### Testing with PostgreSQL

```bash
make test.postgresql
```

This opens a PostgreSQL console connected to the test database.

---

## 10. Interactive Test Console

For debugging and interactive testing:

```bash
make test.console
```

This opens a bash shell inside the test container where you can:
- Run individual tests
- Inspect the test environment
- Debug test failures
- Experiment with code

Inside the console, you can run specific tests:

```bash
qlot exec rove test/model/save.lisp
```

---

## 11. Test System Definition

Tests are defined in `clails-test.asd`:

```common-lisp
(defsystem clails-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on (#:rove
               #:clails
               #:clails-test/util
               #:clails-test/model/query
               #:clails-test/controller/base-controller
               ;; ... other test modules
               )
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))
```

To add new tests, include them in the `:depends-on` list.

---

## 12. Best Practices

### Isolation

- Each test should be independent
- Use `setup` and `teardown` to ensure clean state
- Don't rely on execution order

### Descriptive Test Names

```common-lisp
;; Good
(deftest user-validation-requires-email
  ...)

;; Less clear
(deftest test1
  ...)
```

### Testing Edge Cases

Test not just the happy path, but also:
- Empty inputs
- Nil values
- Boundary conditions
- Error conditions

### Use Meaningful Assertions

```common-lisp
;; Good
(ok (= (length users) 3)
    "returns exactly 3 users")

;; Less helpful
(ok (= (length users) 3))
```

### Database Tests

- Use transactions to rollback changes
- Use temporary directories for SQLite databases
- Clean up test data in `teardown`

### Mock External Dependencies

When testing components that depend on external services, consider mocking them to:
- Improve test speed
- Ensure test reliability
- Avoid side effects

---

## 13. Continuous Integration

Tests are designed to run in CI environments using Docker, ensuring:
- Consistent test environments across machines
- Isolated database instances
- Reproducible test results

The test configuration uses docker-compose to orchestrate multiple database containers, making it suitable for CI pipelines.

---

## Summary

The clails testing framework provides:

- **Rove** for expressive, readable tests
- **Docker-based** environments for consistency
- **Multi-database** support for comprehensive testing
- **Make commands** for easy test execution
- **E2E testing** for complete application verification
- **Interactive console** for debugging

By following these guidelines and patterns, you can write effective tests that ensure the quality and reliability of your clails applications.
