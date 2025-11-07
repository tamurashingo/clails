# clails Testing Guide

## Overview

This guide explains how to write and run tests for applications built with clails.
clails provides a testing framework based on **Rove** with additional features like tags and package filtering to organize and run your tests efficiently.

## Basic Concepts

- Tests use the **Rove** testing framework with clails extensions
- Tests are defined using `deftest-suite` macro which supports tagging
- Tests can be filtered and run by tags or packages
- Use the `clails test` command to run tests in your application
- Test files are organized in the `test/` directory, mirroring your application structure

---

## 1. Test Project Structure

When you create a new clails project, the test structure is automatically generated:

```
your-app/
├── app/
│   ├── controllers/
│   ├── models/
│   └── views/
├── test/
│   ├── controllers/      # Controller tests
│   ├── models/           # Model tests
│   ├── views/            # View tests
│   ├── sample.lisp       # Sample test file
│   └── test-loader.lisp  # Test loader configuration
├── your-app.asd          # Application system definition
└── your-app-test.asd     # Test system definition
```

### Test System Definition

The test system is defined in `your-app-test.asd`:

```common-lisp
(defsystem your-app-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("clails"
               "rove"
               "your-app"
               "your-app-test/test-loader")
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))
```

---

## 2. Writing Tests

### Basic Test Structure with deftest-suite

clails extends Rove with the `deftest-suite` macro, which allows you to tag tests:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-creation
  (testing "Create a new user"
    (let ((user (make-record '<user> :name "Alice")))
      (ok (save user) "User saved successfully")
      (ok (ref user :id) "User has an ID after save"))))

(deftest-suite (:model :validation) test-user-validation
  (testing "User validation"
    (let ((user (make-record '<user> :name "")))
      (ok (not (save user)) "Empty name should fail validation")
      (ok (has-error-p user) "User should have validation errors"))))
```

### deftest-suite Syntax

```common-lisp
(deftest-suite tags test-name
  body...)
```

- **tags**: A single keyword (`:model`) or list of keywords (`(:model :validation)`)
- **test-name**: Symbol naming the test
- **body**: Test code using Rove assertions

### Sample Test File

When you create a new project, a sample test file is generated at `test/sample.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/sample
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/sample)

(deftest-suite :sample sample-basic-test
  (testing "Sample basic test"
    (ok t "Always passes")))

(deftest-suite (:sample :number) sample-number-test
  (testing "Sample number test"
    (ok (= 1 1) "1 equals 1")
    (ok (> 2 1) "2 is greater than 1")))

(deftest-suite (:sample :string) sample-string-test
  (testing "Sample string test"
    (ok (stringp "hello world") "String is a string")
    (ok (string= "hello" "hello") "Strings are equal")))
```

---

## 3. Rove Assertions

clails uses Rove's assertion functions:

### ok - Assert that expression is truthy

```common-lisp
(ok (= 2 (+ 1 1))
    "1 + 1 equals 2")
```

### ng - Assert that expression is falsy

```common-lisp
(ng (string= "hello" "world")
    "Strings are not equal")
```

### signals - Assert that code signals an error

```common-lisp
(ok (signals (error "test error"))
    "Should signal an error")
```

### testing - Group related assertions

```common-lisp
(testing "User creation"
  (ok (create-user "Alice"))
  (ok (find-user-by-name "Alice")))
```

---

## 4. Running Tests

### Run All Tests

From your project directory:

```bash
clails test
```

This runs all tests in your application.

### Run Tests by Package

Run tests for specific packages:

```bash
clails test your-app-test/models/user
clails test your-app-test/controllers/user-controller
```

Multiple packages:

```bash
clails test your-app-test/models/user your-app-test/models/post
```

### Run Tests by Tag

Run all tests with a specific tag:

```bash
clails test --tag model
```

Run tests with multiple tags:

```bash
clails test --tag model --tag validation
```

### Exclude Tests by Tag

Exclude slow or specific tests:

```bash
clails test --exclude slow
clails test --exclude integration
```

### Combine Filters

You can combine package and tag filters:

```bash
clails test your-app-test/models --tag validation
clails test --tag model --exclude slow
```

---

## 5. Test Discovery and Listing

### List All Available Tags

```bash
clails test --list-tags
```

Output:
```
Available tags:
  :CONTROLLER
  :MODEL
  :SAMPLE
  :STRING
  :NUMBER
  :VALIDATION
```

### List All Test Packages

```bash
clails test --list-packages
```

Output:
```
Available packages:
  YOUR-APP-TEST/SAMPLE
  YOUR-APP-TEST/MODELS/USER
  YOUR-APP-TEST/CONTROLLERS/USER-CONTROLLER
```

### List Tests with Specific Tag

```bash
clails test --list-tests-tag model
```

Output:
```
Tests with tag :MODEL:
  TEST-USER-CREATION (YOUR-APP-TEST/MODELS/USER)
  TEST-USER-VALIDATION (YOUR-APP-TEST/MODELS/USER)
  TEST-POST-CREATION (YOUR-APP-TEST/MODELS/POST)
```

### List Tests in Specific Package

```bash
clails test --list-tests-pkg your-app-test/models/user
```

Output:
```
Tests in package YOUR-APP-TEST/MODELS/USER:
  TEST-USER-CREATION [:MODEL]
  TEST-USER-VALIDATION [:MODEL :VALIDATION]
```

---

## 6. Generating Tests

### Generate Model Test

When you generate a model, a corresponding test file is created:

```bash
clails generate:model user
```

This creates:
- `app/models/user.lisp` - Model file
- `test/models/user.lisp` - Test file

The generated test file:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-model
  (testing "Test user model"
    (ok (= 1 0) "This test should be replaced with actual test")))
```

### Generate Controller Test

When you generate a controller:

```bash
clails generate:controller user
```

This creates:
- `app/controllers/user-controller.lisp` - Controller file
- `test/controllers/user-controller.lisp` - Test file

---

## 7. Testing Models

### Example Model Test

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test
        #:your-app/models/user))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-save
  (testing "Save a new user"
    (let ((user (make-record '<user> 
                  :name "Alice"
                  :email "alice@example.com")))
      (ok (save user)
          "User saves successfully")
      (ok (ref user :id)
          "User has ID after save")
      (ok (string= (ref user :name) "Alice")
          "User name is preserved"))))

(deftest-suite (:model :query) test-user-query
  (testing "Query users"
    (let ((users (execute-query 
                   (query <user>
                          :as :user
                          :where (:= (:user :name) :name))
                   '(:name "Alice"))))
      (ok (> (length users) 0)
          "Found users with name Alice"))))

(deftest-suite (:model :validation) test-user-validation
  (testing "User validation"
    (let ((user (make-record '<user> :name "" :email "")))
      (ng (save user)
          "Empty name and email should fail")
      (ok (has-error-p user)
          "User has validation errors")
      (ok (ref-error user :name)
          "Name error is set")
      (ok (ref-error user :email)
          "Email error is set"))))
```

---

## 8. Testing Controllers

### Example Controller Test

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/controllers/user-controller
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/controllers/user-controller)

(deftest-suite :controller test-user-controller-list
  (testing "List users"
    ;; Test controller logic
    (ok t "Controller test placeholder")))
```

---

## 9. Test Organization Best Practices

### Organize Tests by Tags

Use tags to categorize your tests:

- `:model` - Model/database tests
- `:controller` - Controller tests
- `:view` - View rendering tests
- `:integration` - Integration tests
- `:unit` - Unit tests
- `:slow` - Slow-running tests
- `:validation` - Validation tests

### Example Tag Usage

```common-lisp
;; Unit test - fast, isolated
(deftest-suite (:model :unit) test-user-name-formatting
  (testing "Format user name"
    (ok (string= (format-name "alice") "Alice"))))

;; Integration test - slower, uses database
(deftest-suite (:model :integration) test-user-with-posts
  (testing "User with posts"
    (let ((user (find-user-by-id 1)))
      (ok (> (length (ref user :posts)) 0)))))

;; Slow test - marked for exclusion in quick runs
(deftest-suite (:model :slow) test-bulk-user-creation
  (testing "Create 1000 users"
    (ok (create-many-users 1000))))
```

### Recommended Test File Structure

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test
        #:your-app/models/user))
(in-package #:your-app-test/models/user)

;; Setup - runs before tests
(setup
  ;; Initialize test data, database, etc.
  )

;; Teardown - runs after tests
(teardown
  ;; Clean up test data
  )

;; Tests grouped by functionality
(deftest-suite :model test-user-creation
  ...)

(deftest-suite :model test-user-update
  ...)

(deftest-suite :model test-user-deletion
  ...)

(deftest-suite (:model :validation) test-user-validation
  ...)
```

---

## 10. Test Loader Configuration

The `test/test-loader.lisp` file ensures all test modules are loaded:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/test-loader
  (:use #:cl)
  (:import-from #:your-app-test/sample)
  (:import-from #:your-app-test/models/user)
  (:import-from #:your-app-test/controllers/user-controller))
(in-package #:your-app-test/test-loader)
```

**Important**: Add imports for each new test file you create to ensure they are loaded when running tests.

---

## 11. Command Reference

### clails test

Run tests with optional filtering.

```bash
clails test [PACKAGES...] [OPTIONS]
```

#### Arguments

- `PACKAGES...` - Package names to test (exact match)

#### Options

- `--tag TAG` - Include tests with TAG (can be specified multiple times)
- `--exclude TAG` - Exclude tests with TAG (can be specified multiple times)
- `--list-tags` - List all available tags
- `--list-packages` - List all available packages
- `--list-tests-tag TAG` - List tests with specific tag
- `--list-tests-pkg PKG` - List tests in specific package
- `-h, --help` - Show help message

#### Examples

```bash
# Run all tests
clails test

# Run tests in specific packages
clails test your-app-test/models/user
clails test your-app-test/models/user your-app-test/models/post

# Run tests with specific tag
clails test --tag model

# Run tests with multiple tags
clails test --tag model --tag validation

# Exclude slow tests
clails test --exclude slow

# Combine filters
clails test --tag model --exclude slow
clails test your-app-test/models --tag validation

# List available tags
clails test --list-tags

# List available packages
clails test --list-packages

# List tests with specific tag
clails test --list-tests-tag model

# List tests in specific package
clails test --list-tests-pkg your-app-test/models/user
```

---

## 12. Best Practices

### Write Descriptive Test Names

```common-lisp
;; Good
(deftest-suite :model test-user-validates-email-format
  ...)

;; Less clear
(deftest-suite :model test1
  ...)
```

### Use Meaningful Assertion Messages

```common-lisp
;; Good
(ok (string= (ref user :name) "Alice")
    "User name should be 'Alice'")

;; Less helpful
(ok (string= (ref user :name) "Alice"))
```

### Tag Tests Appropriately

```common-lisp
;; Tag by component
(deftest-suite :model ...)
(deftest-suite :controller ...)

;; Tag by test type
(deftest-suite :unit ...)
(deftest-suite :integration ...)

;; Multiple tags for flexibility
(deftest-suite (:model :validation :slow) ...)
```

### Keep Tests Independent

Each test should be able to run independently:

```common-lisp
;; Good - each test creates its own data
(deftest-suite :model test-user-creation
  (let ((user (make-record '<user> :name "Test")))
    (ok (save user))))

(deftest-suite :model test-user-deletion
  (let ((user (make-record '<user> :name "Test")))
    (save user)
    (ok (destroy user))))
```

### Use Setup and Teardown

For tests that share initialization:

```common-lisp
(setup
  (clails/model/connection:startup-connection-pool)
  (seed-test-data))

(teardown
  (clean-test-data)
  (clails/model/connection:shutdown-connection-pool))
```

### Test Both Success and Failure Cases

```common-lisp
(deftest-suite :model test-user-validation
  (testing "Valid user"
    (let ((user (make-record '<user> 
                  :name "Alice" 
                  :email "alice@example.com")))
      (ok (save user) "Valid user saves")))
  
  (testing "Invalid user - empty name"
    (let ((user (make-record '<user> 
                  :name "" 
                  :email "alice@example.com")))
      (ng (save user) "Empty name fails validation")))
  
  (testing "Invalid user - invalid email"
    (let ((user (make-record '<user> 
                  :name "Alice" 
                  :email "not-an-email")))
      (ng (save user) "Invalid email fails validation"))))
```

---

## Summary

clails provides a comprehensive testing framework for your applications:

- **deftest-suite** - Define tests with tags for easy filtering
- **clails test** - Run tests with flexible filtering options
- **Tags and packages** - Organize tests for efficient execution
- **Auto-generation** - Test files generated with models and controllers
- **Rove integration** - Use familiar Rove assertions and syntax

By following this guide and best practices, you can write effective tests that ensure the quality and reliability of your clails applications.
