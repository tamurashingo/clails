# clails Model Guide

## Overview

The clails Model is a database access layer inspired by Ruby on Rails' ActiveRecord.
However, unlike Rails, all column information is managed in hash tables, and dedicated functions are used to access columns.

## Basic Concepts

- Models correspond to database tables
- Column information is not directly written in class definitions but automatically retrieved from the database
- Use the `ref` function to access columns and `(setf ref)` to set values
- Only modified columns are updated (tracked by dirty flags)

---

## 1. Writing Migration Files

Migration files manage database schema changes.
They are placed in the `db/migrate/` directory.

### File Naming Convention

```
YYYYmmdd-HHMMSS-description.lisp
```

Example: `20240101-120000-create-users-table.lisp`

### Creating Tables

```common-lisp
(in-package #:your-app/db/migrate)

(defmigration "20240101-120000-create-users-table"
  (:up #'(lambda (conn)
           (create-table conn :table "users"
                              :columns '(("name" :type :string
                                                 :not-null T)
                                         ("email" :type :string
                                                  :not-null T)
                                         ("age" :type :integer
                                                :not-null NIL)
                                         ("is-active" :type :boolean
                                                      :default-value T))))
   :down #'(lambda (conn)
             (drop-table conn :table "users"))))
```

### Adding Columns

```common-lisp
(defmigration "20240102-130000-add-phone-to-users"
  (:up #'(lambda (conn)
           (add-column conn :table "users"
                            :columns '(("phone" :type :string
                                                :not-null NIL))))
   :down #'(lambda (conn)
             (drop-column conn :table "users"
                               :column "phone"))))
```

### Adding Indexes

```common-lisp
(defmigration "20240103-140000-add-index-to-users"
  (:up #'(lambda (conn)
           (add-index conn :table "users"
                           :index "idx-users-email"
                           :columns '("email"))))
   :down #'(lambda (conn)
             (drop-index conn :table "users"
                              :index "idx-users-email"))))
```

### Column Types

- `:string` - String (VARCHAR)
- `:text` - Text (TEXT)
- `:integer` - Integer
- `:float` - Floating point number
- `:decimal` - Fixed point number
- `:datetime` - Date and time
- `:date` - Date
- `:time` - Time
- `:boolean` - Boolean

### Column Options

- `:not-null` - `T` if NULL is not allowed, `NIL` if allowed
- `:default-value` - Specify a default value

### Running Migrations

```common-lisp
;; Create database
(clails/model/migration:db-create)

;; Run migrations
(clails/model/migration:db-migrate)

;; Check migration status
(clails/model/migration:db-status)

;; Rollback the last migration
(clails/model/migration:db-rollback)

;; Rollback the last N migrations
(clails/model/migration:db-rollback :step 3)

;; Seed the database with initial data
(clails/model/migration:db-seed)
```

#### db-rollback

Rollback the last N migrations. This is useful when you need to undo recent database changes.

```common-lisp
;; Rollback the last migration
(clails/model/migration:db-rollback)

;; Rollback the last 3 migrations
(clails/model/migration:db-rollback :step 3)
```

#### db-seed

Seed the database with initial data from `db/seeds.lisp`. This is typically used to populate the database with test data or initial application data.

```common-lisp
;; Create db/seeds.lisp with your seed data
;; Example db/seeds.lisp:
;; (let ((user (make-record '<user>
;;                         :name "Admin User"
;;                         :email "admin@example.com")))
;;   (save user))

;; Run the seed command
(clails/model/migration:db-seed)
```

---

## 2. Defining Models

### Basic Model Definition

```common-lisp
(in-package #:your-app/model)

(defmodel <user> (<base-model>)
  (:table "users"))
```

This alone defines a Model corresponding to the `users` table.
Column information is automatically retrieved from the database.

### Initializing Table Information

Execute once at application startup.

```common-lisp
(clails/model/base-model:initialize-table-information)
```

---

## 3. Defining Models with Parent-Child Relationships

### Types of Relations

- `:belongs-to` - Reference to parent (side with foreign key)
- `:has-many` - Reference to children (side without foreign key)

### Example: Company and Department Relationship

```common-lisp
;; Parent Model
(defmodel <company> (<base-model>)
  (:table "company"
   :relations ((:has-many "your-app/model::<department>"
                 :as :departments
                 :foreign-key :company-id))))

;; Child Model
(defmodel <department> (<base-model>)
  (:table "department"
   :relations ((:belongs-to "your-app/model::<company>"
                 :column :company
                 :key :company-id)
               (:has-many "your-app/model::<employee>"
                 :as :employees
                 :foreign-key :department-id))))

;; Grandchild Model
(defmodel <employee> (<base-model>)
  (:table "employee"
   :relations ((:belongs-to "your-app/model::<department>"
                 :column :department
                 :key :department-id))))
```

### Relation Parameters

#### For `:has-many`

- `:as` - Alias for accessing the relation (keyword)
- `:foreign-key` - Foreign key held by the child table (keyword)

#### For `:belongs-to`

- `:column` - Alias for accessing the parent (keyword)
- `:key` - Foreign key held by this table (keyword)

---

## 4. Creating Data (make-record)

Create a new record instance.

```common-lisp
;; Create new instance
(defvar *user* (make-record '<user>
                            :name "Taro Yamada"
                            :email "taro@example.com"
                            :age 30
                            :is-active T))

;; Get column values
(ref *user* :name)      ; => "Taro Yamada"
(ref *user* :email)     ; => "taro@example.com"
(ref *user* :age)       ; => 30
(ref *user* :is-active) ; => T
```

---

## 5. Saving Data (save)

Save a new record or update an existing record.

```common-lisp
;; Save (INSERT or UPDATE)
(save *user*)

;; After saving, ID is automatically set
(ref *user* :id) ; => 1
```

### Update Behavior

- Only modified columns (dirty flags) are updated
- `updated-at` is automatically updated if the column exists

```common-lisp
;; Modify a column
(setf (ref *user* :name) "Jiro Yamada")

;; Only the :name column is updated
(save *user*)
```

---

## 6. Retrieving Data

### Building Queries

Use the `query` function to build queries.

```common-lisp
;; Simple retrieval
(defvar *users* (execute-query
                  (query <user>
                         :as :user)
                  '()))

;; Conditional retrieval
(defvar *active-users* (execute-query
                         (query <user>
                                :as :user
                                :where (:= (:user :is-active) T))
                         '()))

;; With parameters
(defvar *user* (execute-query
                 (query <user>
                        :as :user
                        :where (:= (:user :id) :id))
                 (list :id 1)))
```

### Sorting

```common-lisp
;; Sort by name in ascending order
(execute-query
  (query <user>
         :as :user
         :order-by ((:user :name)))
  '())

;; Sort by created_at in descending order
(execute-query
  (query <user>
         :as :user
         :order-by ((:user :created-at :desc)))
  '())
```

### LIMIT and OFFSET

```common-lisp
;; Get first 10 records
(execute-query
  (query <user>
         :as :user
         :limit 10)
  '())

;; Get 10 records starting from 21st
(execute-query
  (query <user>
         :as :user
         :limit 10
         :offset 20)
  '())
```

### WHERE Conditions

```common-lisp
;; Equal
(:= (:user :age) 30)
(:= (:user :age) :age)  ; With parameter

;; Not equal
(:<> (:user :age) 30)

;; Greater than
(:> (:user :age) 30)

;; Greater than or equal
(:>= (:user :age) 30)

;; Less than
(:< (:user :age) 30)

;; Less than or equal
(:<= (:user :age) 30)

;; LIKE
(:like (:user :name) "%Yamada%")
(:like (:user :name) :keyword)  ; With parameter

;; NOT LIKE
(:not-like (:user :name) "%test%")
(:not-like (:user :email) :pattern)  ; With parameter

;; IN
(:in (:user :id) (1 2 3))
(:in (:user :status) :statuses)  ; With parameter

;; NOT IN
(:not-in (:user :status) ("inactive" "deleted"))
(:not-in (:user :id) :excluded-ids)  ; With parameter

;; BETWEEN
(:between (:user :age) 20 30)
(:between (:user :created-at) :start-date :end-date)  ; With parameters

;; NOT BETWEEN
(:not-between (:user :age) 0 17)
(:not-between (:user :score) :min :max)  ; With parameters

;; IS NULL
(:null (:user :deleted-at))

;; IS NOT NULL
(:not-null (:user :email))

;; AND
(:and (:= (:user :is-active) T)
      (:>= (:user :age) 20))

;; OR
(:or (:= (:user :status) "active")
     (:= (:user :status) "pending"))
```

---

## 7. Joining Tables

### Basic JOIN

```common-lisp
;; Get departments with company information
(defvar *departments* (execute-query
                        (query <department>
                               :as :dept
                               :join ((:table "company" :as :comp
                                       :on (:= (:dept :company-id) (:comp :id)))))
                        '()))

;; Access joined data
(loop for dept in *departments*
      do (format t "Department: ~A, Company: ~A~%"
                 (ref dept :name)
                 (ref dept :comp.name)))  ; Access company name with "comp.name"
```

### Multiple JOINs

```common-lisp
;; Get employees with department and company information
(defvar *employees* (execute-query
                      (query <employee>
                             :as :emp
                             :join ((:table "department" :as :dept
                                     :on (:= (:emp :department-id) (:dept :id)))
                                    (:table "company" :as :comp
                                     :on (:= (:dept :company-id) (:comp :id)))))
                      '()))

;; Access joined data
(loop for emp in *employees*
      do (format t "Employee: ~A, Department: ~A, Company: ~A~%"
                 (ref emp :name)
                 (ref emp :dept.name)
                 (ref emp :comp.name)))
```

### Loading Related Data via JOINs

When using JOINs with defined relations, related data is automatically populated in the model instances.

```common-lisp
;; Load blogs with their comments (has-many relation)
(defvar *blogs* (execute-query
                  (query <blog>
                         :as :blog
                         :joins ((:left-join :comments)))
                  '()))

;; Access related data through the relation alias
(loop for blog in *blogs*
      do (let ((comments (ref blog :comments)))
           (format t "Blog: ~A has ~A comments~%"
                   (ref blog :title)
                   (length comments))))

;; Load employee with department and company (belongs-to and through relations)
(defvar *employees* (execute-query
                      (query <employee>
                             :as :emp
                             :joins ((:inner-join :department)
                                    (:inner-join :company :through :department)))
                      '()))

;; Access related data
(loop for emp in *employees*
      do (format t "Employee: ~A, Department: ~A, Company: ~A~%"
                   (ref emp :name)
                   (ref (ref emp :department) :name)
                   (ref (ref emp :company) :name)))
```

Note: Use `ref-in` for more concise nested access:

```common-lisp
(loop for emp in *employees*
      do (format t "Employee: ~A, Department: ~A, Company: ~A~%"
                   (ref emp :name)
                   (ref-in emp :department :name)
                   (ref-in emp :company :name)))
```

---

## 8. Validation

### Built-in Validators

Define validators using the `:validators` option.

```common-lisp
(defmodel <user> (<base-model>)
  (:table "users"
   :validators ((:name :presence T
                       :min-length 1
                       :max-length 50)
                (:email :presence T
                        :format "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
                (:age :numericality (:>= 0 :<= 150)))))
```

### Available Validators

- `:presence` - Check if value is not NIL or empty string
- `:min-length` - Minimum string length
- `:max-length` - Maximum string length
- `:format` - Regular expression pattern
- `:numericality` - Numeric range validation

### Custom Validators

```common-lisp
(defmodel <user> (<base-model>)
  (:table "users"
   :validators ((:email :custom #'validate-email-uniqueness))))

(defun validate-email-uniqueness (instance column-name value)
  "Validate email uniqueness"
  (when value
    (let ((existing (execute-query
                      (query <user>
                             :as :user
                             :where (:and (:= (:user :email) :email)
                                          (:<> (:user :id) :id)))
                      (list :email value
                            :id (or (ref instance :id) 0)))))
      (when existing
        (add-error instance column-name "Email already exists")))))
```

### Validation Execution

Validation is automatically executed during `save`.

```common-lisp
(defvar *user* (make-record '<user>
                            :name ""  ; Empty (validation error)
                            :email "invalid"))  ; Invalid format

(save *user*)  ; => NIL (validation failed)

;; Check errors
(has-error-p *user*)  ; => T
(get-error *user* :name)  ; => "Name is required"
(get-error *user* :email)  ; => "Email format is invalid"
```

---

## 9. Optimistic Locking

Detect simultaneous updates by multiple users or processes using version numbers.

### Setup

Add a `lock_version` column to the table.

```common-lisp
(defmigration "20240104-150000-add-lock-version-to-users"
  (:up #'(lambda (conn)
           (add-column conn :table "users"
                            :columns '(("lock_version" :type :integer
                                                       :not-null T
                                                       :default-value 0))))
   :down #'(lambda (conn)
             (drop-column conn :table "users"
                               :column "lock_version"))))
```

### Usage

```common-lisp
;; User A retrieves a record
(defvar *user-a* (first (execute-query
                         (query <user>
                                :as :user
                                :where (:= (:user :id) 1))
                         '())))

;; User B retrieves the same record
(defvar *user-b* (first (execute-query
                         (query <user>
                                :as :user
                                :where (:= (:user :id) 1))
                         '())))

;; User A updates the record
(setf (ref *user-a* :name) "Updated by A")
(save *user-a*)  ; => T (successful)

;; User B tries to update the same record
(setf (ref *user-b* :name) "Updated by B")
(save *user-b*)  ; => NIL (failed due to version mismatch)

;; Check for errors
(has-error-p *user-b*)  ; => T
(get-error *user-b* :lock-version)  ; => "Record has been modified by another user"
```

---

## 10. Transactions

Use `with-transaction` to wrap multiple operations in a transaction.

### Basic Usage

```common-lisp
(with-transaction
  ;; All operations succeed or all fail
  (save user1)
  (save user2)
  (save user3))
```

### Error Handling

If an error occurs during the transaction, it is automatically rolled back.

```common-lisp
(handler-case
    (with-transaction
      (save user1)
      (error "Something went wrong")
      (save user2))  ; Not executed
  (error (e)
    (format t "Transaction aborted: ~A~%" e)))
```

### Nested Transactions (Savepoints)

Nested `with-transaction` blocks use savepoints.

```common-lisp
(with-transaction
  ;; Outer transaction
  (save user1)
  
  (handler-case
      (with-transaction
        ;; Inner transaction (savepoint)
        (save user2)
        (error "Inner error"))
    (error (e)
      ;; Inner transaction is rolled back
      (format t "Inner transaction failed: ~A~%" e)))
  
  ;; Outer transaction continues
  (save user3))
;; => user1 and user3 are saved, user2 is not saved
```

### Practical Example: Complex Operation

```common-lisp
(defun transfer-employee (employee-id new-department-id)
  "Transfer employee to a new department"
  (with-transaction
    (let ((emp (first (execute-query
                       (query <employee>
                              :as :emp
                              :where (:= (:emp :id) :emp-id))
                       (list :emp-id employee-id))))
          (new-dept (first (execute-query
                            (query <department>
                                   :as :dept
                                   :where (:= (:dept :id) :dept-id))
                            (list :dept-id new-department-id)))))
      (unless (and emp new-dept)
        (error "Employee or department not found"))
      
      ;; Update employee's department
      (setf (ref emp :department-id) new-department-id)
      (unless (save emp)
        (error "Failed to update employee"))
      
      ;; Log the transfer
      (let ((log (make-record '<transfer-log>
                             :employee-id employee-id
                             :old-department-id (ref emp :department-id)
                             :new-department-id new-department-id
                             :transferred-at (local-time:now))))
        (unless (save log)
          (error "Failed to create transfer log")))
      
      emp)))

;; Usage
(handler-case
    (transfer-employee 123 456)
  (error (e)
    (format t "Transaction aborted: ~A~%" e)))
```

### Notes

1. **Nesting depth**: Avoid excessive nesting; use only when necessary
2. **Long transactions**: Design transactions to complete quickly
3. **Deadlocks**: Be cautious of deadlocks when multiple transactions access the same resources
4. **Connection management**: `with-transaction` uses thread-local connections. The same connection is reused within the same thread

---

## 11. Deleting Data

### Deleting a Single Record

```common-lisp
(defvar *user* (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) 1))
                        '())))

;; Delete
(destroy *user*)

;; After deletion, frozen-p becomes T and changes are not allowed
(frozen-p *user*) ; => T
(save *user*)     ; => NIL (cannot save because it's frozen)
```

### Deleting Multiple Records

```common-lisp
(defvar *users* (execute-query
                  (query <user>
                         :as :user
                         :where (:= (:user :is-active) NIL))
                  '()))

;; Batch delete
(destroy *users*)
```

### Cascade Delete

Specifying `:cascade T` also deletes related records defined with `:has-many`.

```common-lisp
;; Deleting a company also deletes all departments belonging to that company
(destroy *company* :cascade T)

;; Deleting a department also deletes all employees belonging to that department
(destroy *department* :cascade T)
```

### Deleting Within Transactions

Delete operations can also be executed within transactions.

```common-lisp
(with-transaction
  ;; Delete multiple records
  (destroy user1)
  (destroy user2)
  ;; If either fails, both are rolled back
  )
```

---

## 12. Other Useful Features

### Checking Dirty Flags

```common-lisp
(defvar *user* (make-record '<user> :name "Test"))
(has-dirty-p *user*) ; => T (dirty on new creation)

(save *user*)
(has-dirty-p *user*) ; => NIL (dirty flag cleared after save)

(setf (ref *user* :name) "New Name")
(has-dirty-p *user*) ; => T (dirty because of changes)
```

### Clearing Errors

```common-lisp
(clear-error *user*)
(has-error-p *user*) ; => NIL
```

### Clearing Dirty Flags

Manually clear dirty flags without saving. This is useful when you want to discard changes.

```common-lisp
(setf (ref *user* :name) "New Name")
(has-dirty-p *user*) ; => T

(clear-dirty-flag *user*)
(has-dirty-p *user*) ; => NIL
```

### Debugging Utilities

#### show-model-data

Display all column values in a model instance for debugging purposes.

```common-lisp
(show-model-data *user*)
;; Outputs:
;; ID: 1
;; NAME: "Taro Yamada"
;; EMAIL: "taro@example.com"
;; AGE: 30
;; ...
```

#### show-model-columns

Display all column definitions for a model.

```common-lisp
(show-model-columns *user*)
;; Outputs column information including names, types, and conversion functions
```

#### debug-table-information

Display the complete table information registry for debugging model definitions.

```common-lisp
(debug-table-information)
;; Outputs all registered model table information
```

### Converting to JSON

Model instances can be automatically converted to JSON.

```common-lisp
(jonathan:to-json *user*)
;; => "{\"ID\":1,\"NAME\":\"Taro Yamada\",\"EMAIL\":\"taro@example.com\",...}"
```

---

## 13. Common Usage Patterns

### Pagination

```common-lisp
(defun get-users-page (page per-page)
  (let ((offset (* (1- page) per-page)))
    (execute-query
      (query <user>
             :as :user
             :order-by ((:user :created-at :desc))
             :limit per-page
             :offset offset)
      '())))

;; Usage
(get-users-page 1 20)  ; Page 1 (records 1-20)
(get-users-page 2 20)  ; Page 2 (records 21-40)
```

### Search

```common-lisp
(defun search-users-by-name (keyword)
  (execute-query
    (query <user>
           :as :user
           :where (:like (:user :name) :keyword)
           :order-by ((:user :name)))
    (list :keyword (format nil "%~A%" keyword))))
```

### Batch Processing with Transactions

When processing large amounts of data, use transactions appropriately.

```common-lisp
(defun import-users (user-data-list)
  "Bulk import user data"
  (with-transaction
    (loop for user-data in user-data-list
          do (let ((user (make-record '<user>
                                     :name (getf user-data :name)
                                     :email (getf user-data :email))))
               (unless (save user)
                 (error "Failed to save user: ~A" (getf user-data :name)))))))

;; Usage
(import-users '((:name "User1" :email "user1@example.com")
                (:name "User2" :email "user2@example.com")
                (:name "User3" :email "user3@example.com")))
;; => All succeed or all fail
```

### Conditional Save

```common-lisp
(defun update-user-if-not-modified (user-id new-name expected-version)
  "Update user only if version matches"
  (with-transaction
    (let ((user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        (list :id user-id)))))
      (when user
        (if (= (ref user :lock-version) expected-version)
            (progn
              (setf (ref user :name) new-name)
              (save user)
              T)
            (progn
              (format t "User was modified by another process~%")
              NIL))))))
```

---

## Summary

clails Models have the following features:

1. **Explicit Design**: Column information is automatically retrieved, access uses dedicated functions
2. **Efficient Updates**: Only modified columns are updated using dirty flags
3. **Flexible Queries**: Query construction via DSL, supporting JOIN and complex conditions
4. **Relation Management**: Define parent-child relationships with `:belongs-to` and `:has-many`
5. **Safety**: Support for validation, optimistic locking, and transactions
6. **Transaction Management**: Easy transaction control with `with-transaction` and nested transaction (savepoint) support

For detailed API reference, please refer to the docstring of each function.
