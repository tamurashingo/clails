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

### Return Values from Database

The format of values retrieved from the database differs depending on the column type.

- `:datetime` - Returned as Universal Time (integer)
- `:date` - Returned as Universal Time (integer). The time part is 00:00:00
- `:time` - Returned as seconds elapsed from 00:00:00 (integer)

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

## 5. Query Building

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

## 6. JOIN Queries

### INSERT and UPDATE

The `save` function automatically determines whether to INSERT or UPDATE.

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

### Default Value Handling

When inserting new records, only columns explicitly set (with dirty flag) are included in the INSERT statement, allowing database default values to be applied to unset columns.

```common-lisp
;; Create instance with some columns unset
(defvar *user* (make-record '<user>
                            :name "Taro"
                            :email "taro@example.com"))
;; :age column is not set, database default will be used

(save *user*)
;; => INSERT INTO users (name, email, created_at, updated_at, version)
;;    VALUES ('Taro', 'taro@example.com', ..., ..., 0)
```

### Validation

Define the `validate` method to validate data before saving.

```common-lisp
(defmethod validate ((inst <user>))
  (when (or (null (ref inst :name))
            (string= (ref inst :name) ""))
    (setf (ref-error inst :name)
          "Name is required"))

  (when (or (null (ref inst :email))
            (string= (ref inst :email) ""))
    (setf (ref-error inst :email)
          "Email is required")))

;; Save returns NIL if validation errors exist
(defvar *invalid-user* (make-record '<user> :name "" :email ""))
(save *invalid-user*) ; => NIL

;; Check errors
(has-error-p *invalid-user*) ; => T
(ref-error *invalid-user* :name) ; => "Name is required"
(ref-error *invalid-user* :email) ; => "Email is required"
```

### Optimistic Locking

To use optimistic locking, create a version management column in the migration and specify the `:version` option in `defmodel`.

```common-lisp
;; Migration: Create version management column
(defmigration "20251001-000000-create-posts-table"
  (:up #'(lambda (conn)
           (create-table conn :table "posts"
                              :columns '(("title" :type :string :not-null T)
                                         ("content" :type :text)
                                         ("lock-version" :type :integer
                                                         :not-null T
                                                         :default-value 0))))
   :down #'(lambda (conn)
             (drop-table conn :table "posts"))))

;; Model: Specify version management column with :version option
(defmodel <post> (<base-model>)
  (:table "posts"
   :version :lock-version))
```

When optimistic locking is enabled, an `optimistic-lock-error` is raised if the version doesn't match during update.

```common-lisp
;; Retrieve the same record in two instances
(defvar *post1* (first (execute-query
                         (query <post>
                                :as :post
                                :where (:= (:post :id) 1))
                         '())))
(defvar *post2* (first (execute-query
                         (query <post>
                                :as :post
                                :where (:= (:post :id) 1))
                         '())))

;; Update post1 (succeeds)
(setf (ref *post1* :title) "Updated by user 1")
(save *post1*)
(ref *post1* :lock-version) ; => 1

;; Update post2 (fails: version is old)
(setf (ref *post2* :title) "Updated by user 2")
(handler-case
    (save *post2*)
  (clails/condition:optimistic-lock-error ()
    (format t "Record was modified by another process~%")))
```

Note: Even if a column named `lock-version` exists, optimistic locking won't be enabled unless you specify the `:version` option in `defmodel`.

---

## 7. Saving Data

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

## 8. Pessimistic Locking

Pessimistic locking allows you to lock records or the entire database at the database level to prevent concurrent updates by other transactions.

In clails, pessimistic locking is achieved using the `with-locked-transaction` macro.

### Basic Usage

```common-lisp
;; Lock and update a record
(clails/model/lock:with-locked-transaction (user
                                           (query <user>
                                                  :as :user
                                                  :where (:= (:user :id) :user-id))
                                           (list :user-id 1))
  ;; Safely update user within this transaction
  (setf (ref user :balance) (+ (ref user :balance) 100))
  (save user))
```

### Query Specification Methods

`with-locked-transaction` accepts queries in two formats.

#### 1. Using clails query DSL

```common-lisp
(with-locked-transaction (users
                         (query <user>
                                :as :user
                                :where (:= (:user :status) :status))
                         (list :status "active"))
  ;; Process users
  (loop for user in users
        do (process-user user)))
```

#### 2. Using cl-batis SQL definition

```common-lisp
(use-package :cl-batis)

;; Define SQL with cl-batis
(defparameter *get-active-users*
  (@select ("SELECT * FROM users WHERE status = :status")))

;; Use with with-locked-transaction
(with-locked-transaction (users
                         *get-active-users*
                         (list :status "active"))
  (loop for user in users
        do (process-user user)))
```

### Lock Modes

Supported lock modes differ by database.

#### PostgreSQL Lock Modes

- `:for-update` - Lock row for update or delete (default)
- `:for-share` - Lock row for read
- `:for-no-key-update` - Lock for non-key updates
- `:for-key-share` - Lock for key reads

```common-lisp
;; Lock with FOR UPDATE
(with-locked-transaction (user
                         (query <user> :as :user :where (:= (:user :id) :id))
                         (list :id 1)
                         :mode :for-update)
  (setf (ref user :status) "processing")
  (save user))

;; Lock with FOR SHARE
(with-locked-transaction (user
                         (query <user> :as :user :where (:= (:user :id) :id))
                         (list :id 1)
                         :mode :for-share)
  ;; Read-only processing
  (ref user :name))
```

#### MySQL Lock Modes

- `:for-update` - Lock row for update or delete (default)
- `:for-share` - Lock row for read

```common-lisp
(with-locked-transaction (user
                         (query <user> :as :user :where (:= (:user :id) :id))
                         (list :id 1)
                         :mode :for-update)
  (setf (ref user :balance) (+ (ref user :balance) 100))
  (save user))
```

#### SQLite3 Lock Modes

SQLite3 does not support row-level locking, so it locks at the database level.

- `:immediate` - Allow reads, lock writes (default, recommended)
- `:exclusive` - Lock all access (use with caution)
- `:deferred` - Defer lock until first query

```common-lisp
;; IMMEDIATE mode (recommended)
(with-locked-transaction (user
                         (query <user> :as :user :where (:= (:user :id) :id))
                         (list :id 1)
                         :mode :immediate)
  (setf (ref user :balance) (+ (ref user :balance) 100))
  (save user))
```

**Note**: Using `:exclusive` mode in SQLite3 will cause all connections to error, including those not using locks. For most cases, `:immediate` mode is recommended.

### NOWAIT Option

Returns an error immediately if the lock cannot be acquired without waiting (supported by PostgreSQL and MySQL).

```common-lisp
(handler-case
    (with-locked-transaction (user
                             (query <user> :as :user :where (:= (:user :id) :id))
                             (list :id 1)
                             :nowait T)
      (setf (ref user :balance) (+ (ref user :balance) 100))
      (save user))
  (error (e)
    (format t "Could not acquire lock: ~A~%" e)))
```

### SKIP LOCKED Option

Skips locked rows and retrieves only unlocked rows (supported by PostgreSQL and MySQL).

```common-lisp
;; Retrieve and process unlocked users
(with-locked-transaction (users
                         (query <user>
                                :as :user
                                :where (:= (:user :status) :status))
                         (list :status "pending")
                         :skip-locked T)
  (loop for user in users
        do (progn
             (setf (ref user :status) "processing")
             (save user))))
```

### Usage Notes

1. **Transaction**: `with-locked-transaction` automatically starts a transaction, commits on success, and rolls back on error
2. **Deadlocks**: When locking multiple records, always lock in the same order to avoid deadlocks
3. **Lock Duration**: Keep transaction processing as short as possible
4. **SQLite3 Limitations**: SQLite3 does not support row-level locking, so the entire database is locked
5. **Retry Logic**: SQLite3 automatically retries on lock errors (with exponential backoff)

---

## 9. Native Query

In clails, you can execute native SQL queries using cl-batis.
You can write complex aggregations and queries that are difficult to express with the query builder directly in SQL.

### About cl-batis

cl-batis is a SQL mapper library inspired by MyBatis.
You can write SQL directly in Common Lisp code and bind parameters dynamically.

For details, see [cl-batis documentation](https://github.com/tamurashingo/cl-batis).

### Defining and Executing SELECT Queries

#### Basic SELECT

cl-batis allows you to define queries in two formats.

**Annotation Style (@select):**

```common-lisp
(use-package :cl-batis)
(cl-syntax:use-syntax :annot)

;; Define SELECT query
@select
("SELECT * FROM users WHERE id = :id")
(defsql get-user-by-id (id))

;; Execute query
(defvar *user* (first (execute-query get-user-by-id
                                     (list :id 1))))
```

**Function Style (select):**

```common-lisp
(use-package :cl-batis)

;; Define SELECT query
(select
 ("SELECT * FROM users WHERE id = :id")
 (defsql get-user-by-id (id)))

;; Execute query
(defvar *user* (first (execute-query get-user-by-id
                                     (list :id 1))))
```

#### Queries with Multiple Conditions

**Annotation Style:**

```common-lisp
@select
("SELECT * FROM users"
 (sql-where
   (sql-cond (not (null name))
             " AND name LIKE :name ")
   (sql-cond (not (null email))
             " AND email = :email "))
 "ORDER BY created_at DESC")
(defsql search-users (name email))
```

**Function Style:**

```common-lisp
(select
 ("SELECT * FROM users"
  (sql-where
    (sql-cond (not (null name))
              " AND name LIKE :name ")
    (sql-cond (not (null email))
              " AND email = :email "))
  "ORDER BY created_at DESC")
 (defsql search-users (name email)))
```

**Usage Examples:**

```common-lisp
;; Search by name
(execute-query search-users
               (list :name "%yamada%" :email nil))

;; Search by name and email
(execute-query search-users
               (list :name "%yamada%" :email "yamada@example.com"))

;; No conditions
(execute-query search-users
               (list :name nil :email nil))
```

#### Queries with JOIN

```common-lisp
;; Annotation style
@select
("SELECT u.*, d.name as department_name
  FROM users u
  INNER JOIN departments d ON u.department_id = d.id
  WHERE u.is_active = :is_active")
(defsql get-users-with-departments (is_active))

;; Function style
(select
 ("SELECT u.*, d.name as department_name
   FROM users u
   INNER JOIN departments d ON u.department_id = d.id
   WHERE u.is_active = :is_active")
 (defsql get-users-with-departments (is_active)))

(execute-query get-users-with-departments
               (list :is_active T))
```

#### Aggregation Queries

```common-lisp
@select
("SELECT department_id, COUNT(*) as user_count
  FROM users
  WHERE created_at >= :start_date
    AND created_at < :end_date
  GROUP BY department_id
  HAVING COUNT(*) > :min_count")
(defsql count-users-by-department (start_date end_date min_count))

(execute-query count-users-by-department
               (list :start_date "2024-01-01"
                     :end_date "2024-12-31"
                     :min_count 5))
```

### Defining and Executing UPDATE Queries

Use `@update` or `update` for UPDATE queries.

#### Basic UPDATE

**Annotation Style:**

```common-lisp
@update
("UPDATE users
  SET status = :status,
      updated_at = :updated_at
  WHERE id = :id")
(defsql update-user-status (status updated_at id))
```

**Function Style:**

```common-lisp
(update
 ("UPDATE users
   SET status = :status,
       updated_at = :updated_at
   WHERE id = :id")
 (defsql update-user-status (status updated_at id)))
```

**Usage Example:**

```common-lisp
;; Execute query (returns number of affected rows)
(defvar *affected-rows*
  (execute-query update-user-status
                 (list :status "active"
                       :updated_at (get-universal-time)
                       :id 1)))
```

#### Conditional UPDATE

Use `sql-set` to dynamically build SET clauses based on conditions.

```common-lisp
@update
("UPDATE users"
 (sql-set
   (sql-cond (not (null name))
             " name = :name, ")
   (sql-cond (not (null email))
             " email = :email, ")
   " updated_at = :updated_at ")
 (sql-where
   " id = :id "))
(defsql update-user-fields (name email updated_at id))

;; Update name only
(execute-query update-user-fields
               (list :id 1
                     :name "New Name"
                     :email nil
                     :updated_at (get-universal-time)))

;; Update name and email
(execute-query update-user-fields
               (list :id 1
                     :name "New Name"
                     :email "newemail@example.com"
                     :updated_at (get-universal-time)))
```

#### Batch UPDATE

```common-lisp
@update
("UPDATE users
  SET is_active = FALSE,
      updated_at = :updated_at
  WHERE last_login_at < :threshold_date
    AND is_active = TRUE")
(defsql deactivate-old-users (updated_at threshold_date))

(execute-query deactivate-old-users
               (list :threshold_date "2023-01-01"
                     :updated_at (get-universal-time)))
```

### INSERT/DELETE Queries

INSERT and DELETE queries can also be defined with `@update` or `update` (regardless of SQL type, use `@update`/`update` for non-SELECT queries).

```common-lisp
;; INSERT (annotation style)
@update
("INSERT INTO logs (user_id, action, created_at)
  VALUES (:user_id, :action, :created_at)")
(defsql insert-log (user_id action created_at))

;; INSERT (function style)
(update
 ("INSERT INTO logs (user_id, action, created_at)
   VALUES (:user_id, :action, :created_at)")
 (defsql insert-log (user_id action created_at)))

(execute-query insert-log
               (list :user_id 1
                     :action "login"
                     :created_at (get-universal-time)))

;; DELETE
@update
("DELETE FROM logs
  WHERE created_at < :threshold_date")
(defsql delete-old-logs (threshold_date))

(execute-query delete-old-logs
               (list :threshold_date "2023-01-01"))
```

### cl-batis Dynamic SQL

cl-batis provides dynamic SQL generation using `sql-cond`, `sql-where`, and `sql-set`.

#### Using sql-where and sql-cond

Controls whether to include parts of the WHERE clause based on conditions.

```common-lisp
@select
("SELECT * FROM users"
 (sql-where
   (sql-cond (not (null name))
             " AND name = :name ")
   (sql-cond (not (null min_age))
             " AND age >= :min_age ")))
(defsql find-users (name min_age))

;; Only name specified
(execute-query find-users (list :name "Alice" :min_age nil))
;; => SELECT * FROM users WHERE AND name = ?

;; Both specified
(execute-query find-users (list :name "Alice" :min_age 20))
;; => SELECT * FROM users WHERE AND name = ? AND age >= ?

;; No conditions
(execute-query find-users (list :name nil :min_age nil))
;; => SELECT * FROM users
```

#### Using sql-set

Builds SET clauses conditionally in UPDATE statements.

```common-lisp
@update
("UPDATE users"
 (sql-set
   (sql-cond (not (null name))
             " name = :name, ")
   (sql-cond (not (null email))
             " email = :email, ")
   " updated_at = :updated_at ")
 (sql-where
   " id = :id "))
(defsql update-user (name email updated_at id))
```

### Using with Transactions

Native queries can also be used within transactions like regular queries.

```common-lisp
(with-transaction
  ;; Execute multiple queries atomically
  (execute-query update-user-status
                 (list :status "active" :id 1))
  (execute-query insert-log
                 (list :user_id 1
                       :action "status_changed"
                       :created_at (get-universal-time))))
```

### Usage Notes

1. **SQL Injection Protection**: Always use `:param_name` format for parameters, do not concatenate strings directly
2. **Return Value Types**:
   - SELECT queries: Returns a list of plists
   - UPDATE/INSERT/DELETE queries: Returns the number of affected rows
3. **Parameter Names**: Use keyword symbols (`:id`, `:name`, etc.)
4. **Database Dependencies**: When using native queries, be aware of database-specific SQL syntax
5. **@select/@update Macros**: `@select` and `@update` are annotation-style macros that require `(cl-syntax:use-syntax :annot)`

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

## 14. Bulk Operations

Bulk operations are designed for efficiently processing large amounts of data. They are intended for batch processing and background jobs, not for online processing.

### 14.1. SELECT: `with-query-cursor`

Efficiently processes large SELECT results in batch units. Performs streaming processing without loading all records into memory.

#### Features

- **Database-specific optimizations**
  - PostgreSQL: Server-side cursor (automatic transaction management)
  - MySQL: Streaming result set
  - SQLite3: LIMIT/OFFSET pagination
- Configurable batch size (default: 1000)
- Supports both `query` macro and cl-batis

#### Basic Usage

```common-lisp
;; Using query macro
(with-query-cursor (user-rows
                    (query <user> :as :u :order-by ((:u :id)))
                    nil
                    :batch-size 500)
  (dolist (row user-rows)
    (format t "User: ~A, Email: ~A~%"
            (getf row :name)
            (getf row :email))))
```

#### Query with Parameters

```common-lisp
(with-query-cursor (user-rows
                    (query <user>
                           :as :u
                           :where (:> (:u :age) :age)
                           :order-by ((:u :id)))
                    '(:age 20)
                    :batch-size 1000)
  (dolist (row user-rows)
    (process-user-row row)))
```

#### Query with JOIN

```common-lisp
(with-query-cursor (blog-rows
                    (query <blog>
                           :as :blog
                           :joins ((:left-join :account))
                           :order-by ((:blog :id)))
                    nil
                    :batch-size 500)
  (dolist (row blog-rows)
    (format t "Blog: ~A, Author: ~A~%"
            (getf row :|BLOG.TITLE|)
            (getf row :|ACCOUNT.NAME|))))
```

#### Using cl-batis

```common-lisp
(select ("SELECT * FROM users WHERE age > :age ORDER BY id")
  (defsql find-users-by-age-greater-than (age)))

(with-query-cursor (user-rows
                    find-users-by-age-greater-than
                    '(:age 20)
                    :batch-size 500)
  (dolist (row user-rows)
    (format t "User: ~A, Age: ~A~%"
            (getf row :name)
            (getf row :age))))
```

#### Using within Transactions

```common-lisp
;; PostgreSQL: Explicitly start transaction (recommended)
(with-transaction
  (with-query-cursor (user-rows
                      (query <user> :as :u :order-by ((:u :id)))
                      nil
                      :batch-size 500)
    (dolist (row user-rows)
      (process-user-row row)))
  
  ;; Other operations can be executed in the same transaction
  (save summary-data))

;; Using external connection
(with-db-connection (conn)
  (with-transaction-using-connection conn
    (with-query-cursor (user-rows
                        (query <user> :as :u :order-by ((:u :id)))
                        nil
                        :batch-size 500
                        :connection conn)
      (dolist (row user-rows)
        (process-user-row row)))))
```

#### Checking SQL (for debugging)

```common-lisp
(show-query-sql (query <user>
                       :as :u
                       :where (:and (:= (:u :status) :status)
                                   (:> (:u :age) :age))
                       :order-by ((:u :id)))
                '(:status "active" :age 20))
;; => "SELECT U.ID as \"U.ID\", U.NAME as \"U.NAME\", ...
;;     WHERE U.STATUS = 'active' AND U.AGE > 20 ORDER BY U.ID"
```

#### Important Notes

1. **ORDER BY is required**: It is strongly recommended to specify ORDER BY for all databases. Without ORDER BY, the order of results between batches is not guaranteed, which may cause duplicates or missing records.

2. **Database-specific notes**:
   - **PostgreSQL**: Automatically starts a transaction when using cursors
   - **MySQL**: Connection is occupied while using streaming result set
   - **SQLite3**: Performance degrades as OFFSET increases

3. **Choosing batch size**: Default is 1000 rows. Consider the trade-off between memory usage and database round trips (recommended range: 100-5000 rows)

### 14.2. INSERT: `insert-all` and `insert-bulk`

Provides two functions for efficient bulk insertion.

#### `insert-all`: Insert one by one (with ID write-back)

Executes `insert1` for each model instance, setting id, created_at, and updated_at on each instance after INSERT.

```common-lisp
(let* ((users (list (make-record '<user> :name "Alice" :age 30 :email "alice@example.com")
                    (make-record '<user> :name "Bob" :age 25 :email "bob@example.com")
                    (make-record '<user> :name "Charlie" :age 35 :email "charlie@example.com")))
       (inserted-users (insert-all users)))
  
  ;; ID is set on each instance
  (dolist (user inserted-users)
    (format t "Inserted ID: ~A, Name: ~A~%"
            (slot-value user 'id)
            (ref user :name))))
;; Output:
;; Inserted ID: 1, Name: Alice
;; Inserted ID: 2, Name: Bob
;; Inserted ID: 3, Name: Charlie
```

**Execute within transaction:**

```common-lisp
(with-transaction (conn)
  (let ((users (list (make-record '<user> :name "Alice" :age 30)
                     (make-record '<user> :name "Bob" :age 25))))
    (insert-all users :connection conn)))
```

#### `insert-bulk`: Bulk INSERT (fast, no ID write-back)

Executes bulk INSERT and returns only the count of inserted rows. Accepts a list of plists or models.

**Using plist:**

```common-lisp
(insert-bulk '<user>
             '(:name :age :email)
             '((:name "Alice" :age 30 :email "alice@example.com")
               (:name "Bob" :age 25 :email "bob@example.com")
               (:name "Charlie" :age 35 :email "charlie@example.com"))
             :batch-size 100)
;; => 3
```

**Using model instances:**

```common-lisp
(let ((users (list (make-record '<user> :name "Alice" :age 30 :email "alice@example.com")
                   (make-record '<user> :name "Bob" :age 25 :email "bob@example.com")
                   (make-record '<user> :name "Charlie" :age 35 :email "charlie@example.com"))))
  (insert-bulk '<user>
               '(:name :age :email)
               users
               :batch-size 100))
;; => 3
```

**Automatic timestamp setting:**

```common-lisp
;; created_at and updated_at are automatically set even if not specified
(insert-bulk '<user>
             '(:name :age)  ; created_at and updated_at are automatically added
             '((:name "Alice" :age 30)
               (:name "Bob" :age 25))
             :batch-size 100)
```

**Execute within transaction:**

```common-lisp
(with-transaction (conn)
  (insert-bulk '<user>
               '(:name :age :email)
               large-data-list
               :connection conn
               :use-transaction nil))  ; External transaction management
```

#### Comparison

| Function | Use Case | Return Value | ID Write-back | Speed |
|----------|----------|--------------|---------------|-------|
| `insert-all` | Use models after INSERT | List of models | ✅ | Slow |
| `insert-bulk` | Fast insertion like CSV import | Count (integer) | ❌ | Fast |

### 14.3. UPDATE: `update-all` and `update-bulk`

Provides two functions for efficient bulk updates.

#### `update-all`: Update one by one

Executes `update1` for each model instance. Only columns marked with dirty-flag are updated.

```common-lisp
(let* ((users (list user1 user2 user3)))
  ;; Modify each user's information
  (setf (ref user1 :name) "Alice Updated")
  (setf (ref user2 :age) 26)
  (setf (ref user3 :email) "charlie-new@example.com")
  
  ;; Bulk update
  (let ((updated-users (update-all users)))
    (dolist (user updated-users)
      (format t "Updated ID: ~A, updated-at: ~A~%"
              (slot-value user 'id)
              (ref user :updated-at)))))
```

**Mixed model classes:**

```common-lisp
(let* ((user (make-record '<user> :name "Alice" :age 30))
       (post (make-record '<post> :title "Hello" :content "World"))
       (inserted-user (first (insert-all (list user))))
       (inserted-post (first (insert-all (list post)))))
  
  ;; Modify both instances
  (setf (ref inserted-user :age) 31)
  (setf (ref inserted-post :title) "Hello World")
  
  ;; Bulk update (user goes to users table, post goes to posts table)
  (let ((updated (update-all (list inserted-user inserted-post))))
    (format t "Updated ~A records~%" (length updated))))
;; => Updated 2 records
```

**Execute within transaction:**

```common-lisp
(with-transaction (conn)
  (let ((users (list user1 user2)))
    (update-all users :connection conn)))
```

#### `update-bulk`: Batch UPDATE

Executes bulk UPDATE and returns the count of updated rows. Explicitly specifies columns to update.

```common-lisp
(let ((users (list user1 user2 user3)))
  ;; Modify each user's information
  (setf (ref user1 :name) "Alice Updated")
  (setf (ref user2 :age) 26)
  (setf (ref user3 :email) "charlie-new@example.com")
  
  ;; Bulk update (specify columns)
  (let ((count (update-bulk '<user> '(:name :age :email) users)))
    (format t "Updated ~A records~%" count)))
;; => Updated 3 records
```

**updated_at is automatically updated:**

```common-lisp
;; updated_at is automatically updated even if not specified
(update-bulk '<user> '(:name :age) users)
;; => name, age, updated_at are updated
```

**id and created_at are excluded:**

```common-lisp
;; id and created_at are ignored even if specified
(update-bulk '<user> '(:id :name :created-at :age) users)
;; => name, age, updated_at are updated (id and created_at are excluded)
```

**Handling type mismatch:**

```common-lisp
;; Default is :error - raises error if different types exist
(update-bulk '<user>
             '(:name :age)
             (list user1 post1 user2))  ; post1 is a <post> instance
;; => type-mismatch-error is raised

;; :skip - Skip instances of different types
(update-bulk '<user>
             '(:name :age)
             (list user1 post1 user2)
             :on-type-mismatch :skip)
;; => 2 (only user1 and user2 are updated, post1 is skipped)
```

#### Comparison

| Function | Use Case | Return Value | Column Specification | Speed |
|----------|----------|--------------|---------------------|-------|
| `update-all` | Update based on dirty-flag | List of models | dirty-flag | Slow |
| `update-bulk` | Update with explicit columns | Count (integer) | Explicit | Medium |

### 14.4. DELETE: `delete-all` and `delete-bulk`

Provides two functions for efficient bulk deletion.

#### `delete-all`: Delete one by one

Executes `destroy` for each model instance. Supports cascade deletion.

```common-lisp
;; Basic deletion
(let ((users (list user1 user2 user3)))
  (let ((deleted-users (delete-all users)))
    (format t "Deleted ~A users~%" (length deleted-users))))
;; => Deleted 3 users
```

**Cascade deletion:**

```common-lisp
;; Enable cascade deletion
(let ((users (list user1 user2)))
  (delete-all users :cascade t))
;; => Deletes user1, user2 and their related records
```

**Execute within transaction:**

```common-lisp
(with-transaction (conn)
  (let ((users (list user1 user2)))
    (delete-all users :connection conn)))
```

#### `delete-bulk`: Batch DELETE

Executes bulk DELETE and returns the count of deleted rows. Implemented with IN clause optimization.

```common-lisp
;; Basic usage
(let ((users (list user1 user2 user3)))
  (let ((count (delete-bulk '<user> users)))
    (format t "Deleted ~A records~%" count)))
;; => Deleted 3 records
```

**Custom batch size:**

```common-lisp
(delete-bulk '<user> large-user-list :batch-size 50)
```

**Handling type mismatch:**

```common-lisp
;; Default is :error - raises error if different types exist
(delete-bulk '<user> (list user1 product1 user2))
;; => type-mismatch-error is raised

;; :skip - Skip instances of different types
(delete-bulk '<user> (list user1 product1 user2) :on-type-mismatch :skip)
;; => 2 (only user1 and user2 are deleted, product1 is skipped)
```

**Execute within transaction:**

```common-lisp
(with-transaction (conn)
  (delete-bulk '<user> users
               :connection conn
               :use-transaction nil))  ; External transaction management
```

#### Comparison

| Function | Use Case | Return Value | Cascade | Speed |
|----------|----------|--------------|---------|-------|
| `delete-all` | When cascade deletion is needed | List of models | ✅ | Slow |
| `delete-bulk` | Fast deletion | Count (integer) | ❌ | Fast |

### 14.5. Error Handling

#### `type-mismatch-error`

Error raised when type mismatch occurs.

```common-lisp
(handler-case
    (update-bulk '<user>
                 '(:name :age)
                 (list user1 post1 user2))
  (type-mismatch-error (e)
    (format t "Type mismatch: ~A~%" e)))
```

#### `:on-type-mismatch` Option

- `:error`: Raise error (default)
- `:skip`: Skip and continue

```common-lisp
;; Skip without raising error
(let ((count (update-bulk '<user>
                          '(:name :age)
                          (list user1 post1 user2)
                          :on-type-mismatch :skip)))
  (format t "Updated ~A records~%" count))
;; => Updated 2 records
```

### 14.6. Best Practices for Bulk Operations

#### 1. Specify ORDER BY

It is strongly recommended to specify ORDER BY for all databases.

```common-lisp
;; Recommended
(with-query-cursor (rows
                    (query <user> :as :u :order-by ((:u :id)))
                    nil)
  (dolist (row rows)
    (process-row row)))

;; Not recommended: Without ORDER BY (order not guaranteed)
(with-query-cursor (rows
                    (query <user> :as :u)
                    nil)
  (dolist (row rows)
    (process-row row)))
```

#### 2. Choosing Batch Size

Consider the trade-off between memory usage and database round trips.

```common-lisp
;; Recommended range: 100-5000 rows
(with-query-cursor (rows
                    (query <user> :as :u :order-by ((:u :id)))
                    nil
                    :batch-size 1000)  ; Default
  ...)
```

#### 3. Transaction Management

Use `with-transaction` when consistency is required.

```common-lisp
(with-transaction
  (with-query-cursor (rows
                      (query <user> :as :u :order-by ((:u :id)))
                      nil)
    (dolist (row rows)
      (process-row row)))
  
  ;; Other operations in the same transaction
  (save summary-data))
```

#### 4. Not Recommended for Online Processing

Bulk operations occupy connections for long periods and maintain transactions, so they are not recommended for online processing. They are intended for batch processing and background jobs.

```common-lisp
;; Not recommended: Use in online processing
(defun show-all-users ()
  (with-query-cursor (rows
                      (query <user> :as :u :order-by ((:u :id)))
                      nil
                      :batch-size 10000)  ; Batch size too large
    (render-html rows)))  ; Display all results on screen

;; Recommended: Use in batch processing
(defun batch-process-users ()
  (with-query-cursor (rows
                      (query <user> :as :u :order-by ((:u :id)))
                      nil
                      :batch-size 1000)
    (dolist (row rows)
      (process-user-row row))))
```

---

## Summary

clails Models have the following features:

1. **Explicit Design**: Column information is automatically retrieved, access uses dedicated functions
2. **Efficient Updates**: Only modified columns are updated using dirty flags
3. **Flexible Queries**: Query construction via DSL, supporting JOIN and complex conditions
4. **Relation Management**: Define parent-child relationships with `:belongs-to` and `:has-many`
5. **Safety**: Support for validation, optimistic locking, pessimistic locking, and transactions
6. **Transaction Management**: Easy transaction control with `with-transaction` and nested transaction (savepoint) support
7. **Native Queries**: Flexible SQL query execution using cl-batis
8. **Bulk Operations**: Support for streaming processing and batch operations for efficient handling of large data

For detailed API reference, please refer to the docstring of each function.
