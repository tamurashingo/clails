(in-package #:cl-user)
(defpackage #:clails-test/model/migration
  (:use #:cl
        #:rove)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool
                #:with-db-connection-direct)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate
                #:migrate-up-version
                #:migrate-down-version
                #:db-rollback
                #:db-status))

(defpackage #:clails-test/model/db/migration
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/model/migration)


(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0007" "/app/test/data/0007-migration-kaizen-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (db-create)
  (clean-test-database)
  (startup-connection-pool))

(teardown
  (clean-test-database)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (shutdown-connection-pool))


;;; Helper functions

(defun clean-test-database ()
  "Drop all tables except migration table, then clean migration table."
  (with-db-connection-direct (connection)
    (let* ((tables-query (dbi:prepare connection "show tables"))
           (tables-result (dbi:execute tables-query))
           (tables (loop for row = (dbi:fetch tables-result)
                        while row
                        collect (getf row :|Tables_in_clails_test|))))
      (dolist (table tables)
        (unless (string= table "migration")
          (dbi:do-sql connection (format nil "drop table if exists ~A" table))))
      (dbi:do-sql connection "delete from migration"))))

(defun get-migration-names (connection)
  "Get all migration names from the migration table."
  (let ((result (dbi-cp:fetch-all
                  (dbi-cp:execute
                    (dbi-cp:prepare connection "select migration_name from migration order by migration_name")
                    '()))))
    (mapcar #'(lambda (row) (getf row :|migration_name|)) result)))

(defun table-exists-p (connection table-name)
  "Check if a table exists in the database."
  (let ((result (dbi-cp:fetch
                  (dbi-cp:execute
                    (dbi-cp:prepare connection "show tables like ?")
                    (list table-name)))))
    (not (null result))))

(defun column-exists-p (connection table-name column-name)
  "Check if a column exists in a table."
  (let ((result (dbi-cp:fetch
                  (dbi-cp:execute
                    (dbi-cp:prepare connection
                                    "select column_name from information_schema.columns where table_schema = database() and table_name = ? and column_name = ?")
                    (list table-name column-name)))))
    (not (null result))))


;;; Test: db-migrate (all migrations)

(deftest db-migrate-all-test
  (testing "db-migrate applies all pending migrations"
    (clean-test-database)
    (db-migrate)

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 5 (length migrations))
            "Should have 5 migrations applied")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should have users table migration")
        (ok (member "20250102-000000-add-age-to-users" migrations :test #'string=)
            "Should have add age migration")
        (ok (member "20250103-000000-create-posts-table" migrations :test #'string=)
            "Should have posts table migration")
        (ok (member "20250104-000000-add-published-to-posts" migrations :test #'string=)
            "Should have add published migration")
        (ok (member "20250105-000000-create-comments-table" migrations :test #'string=)
            "Should have comments table migration"))

      (ok (table-exists-p connection "users")
          "Users table should exist")
      (ok (table-exists-p connection "posts")
          "Posts table should exist")
      (ok (table-exists-p connection "comments")
          "Comments table should exist")
      (ok (column-exists-p connection "users" "age")
          "Users table should have age column")
      (ok (column-exists-p connection "posts" "published")
          "Posts table should have published column"))))


;;; Test: db-migrate with version (migrate to specific version)

(deftest db-migrate-to-version-test
  (testing "db-migrate with version migrates up to specified version"
    (clean-test-database)
    (db-migrate :version "20250103-000000-create-posts-table")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 3 (length migrations))
            "Should have 3 migrations applied")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should have users table migration")
        (ok (member "20250102-000000-add-age-to-users" migrations :test #'string=)
            "Should have add age migration")
        (ok (member "20250103-000000-create-posts-table" migrations :test #'string=)
            "Should have posts table migration")
        (ok (not (member "20250104-000000-add-published-to-posts" migrations :test #'string=))
            "Should not have add published migration")
        (ok (not (member "20250105-000000-create-comments-table" migrations :test #'string=))
            "Should not have comments table migration"))

      (ok (table-exists-p connection "users")
          "Users table should exist")
      (ok (table-exists-p connection "posts")
          "Posts table should exist")
      (ok (not (table-exists-p connection "comments"))
          "Comments table should not exist")
      (ok (not (column-exists-p connection "posts" "published"))
          "Posts table should not have published column yet"))))


;;; Test: migrate-up-version (specific version only)

(deftest migrate-up-version-test
  (testing "migrate-up-version applies only the specified migration"
    (clean-test-database)
    ;; First, apply first three migrations to create posts table
    (db-migrate :version "20250103-000000-create-posts-table")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 3 (length migrations))
            "Should have 3 migrations applied initially")))

    ;; Now apply only the 5th migration (skipping the 4th)
    (migrate-up-version "20250105-000000-create-comments-table")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 4 (length migrations))
            "Should have 4 migrations after migrate-up-version")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should still have users table migration")
        (ok (member "20250102-000000-add-age-to-users" migrations :test #'string=)
            "Should still have add age migration")
        (ok (member "20250103-000000-create-posts-table" migrations :test #'string=)
            "Should still have posts table migration")
        (ok (not (member "20250104-000000-add-published-to-posts" migrations :test #'string=))
            "Should not have add published migration (it was skipped)")
        (ok (member "20250105-000000-create-comments-table" migrations :test #'string=)
            "Should have comments table migration")))))


;;; Test: migrate-up-version (already applied)

(deftest migrate-up-version-already-applied-test
  (testing "migrate-up-version on already applied migration shows message"
    (clean-test-database)
    (db-migrate :version "20250102-000000-add-age-to-users")

    ;; Try to apply the same migration again
    (ok (not (signals (migrate-up-version "20250102-000000-add-age-to-users")))
        "Should not raise error when applying already applied migration")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 2 (length migrations))
            "Should still have only 2 migrations")))))


;;; Test: migrate-up-version (not found)

(deftest migrate-up-version-not-found-test
  (testing "migrate-up-version with non-existent version raises error"
    (clean-test-database)
    (ok (signals (migrate-up-version "20250199-000000-nonexistent") 'error)
        "Should raise error for non-existent migration")))


;;; Test: migrate-down-version (rollback specific version)

(deftest migrate-down-version-test
  (testing "migrate-down-version rolls back only the specified migration"
    (clean-test-database)
    ;; Apply all migrations
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations before rollback")
      (ok (column-exists-p connection "posts" "published")
          "Posts table should have published column before rollback"))

    ;; Rollback only the 4th migration (add published to posts)
    (migrate-down-version "20250104-000000-add-published-to-posts")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 4 (length migrations))
            "Should have 4 migrations after migrate-down-version")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should still have users table migration")
        (ok (member "20250102-000000-add-age-to-users" migrations :test #'string=)
            "Should still have add age migration")
        (ok (member "20250103-000000-create-posts-table" migrations :test #'string=)
            "Should still have posts table migration")
        (ok (not (member "20250104-000000-add-published-to-posts" migrations :test #'string=))
            "Should not have add published migration after rollback")
        (ok (member "20250105-000000-create-comments-table" migrations :test #'string=)
            "Should still have comments table migration (later migration)"))

      (ok (table-exists-p connection "posts")
          "Posts table should still exist")
      (ok (not (column-exists-p connection "posts" "published"))
          "Posts table should not have published column after rollback")
      (ok (table-exists-p connection "comments")
          "Comments table should still exist (later migration not affected)"))))


;;; Test: migrate-down-version (not yet applied)

(deftest migrate-down-version-not-applied-test
  (testing "migrate-down-version on not yet applied migration shows message"
    (clean-test-database)
    ;; Apply only first 2 migrations
    (db-migrate :version "20250102-000000-add-age-to-users")

    (with-db-connection-direct (connection)
      (ok (= 2 (length (get-migration-names connection)))
          "Should have 2 migrations before attempting rollback"))

    ;; Try to rollback a migration that hasn't been applied
    (ok (not (signals (migrate-down-version "20250103-000000-create-posts-table")))
        "Should not raise error when rolling back unapplied migration")

    (with-db-connection-direct (connection)
      (ok (= 2 (length (get-migration-names connection)))
          "Should still have 2 migrations (no change)"))))


;;; Test: migrate-down-version (not found)

(deftest migrate-down-version-not-found-test
  (testing "migrate-down-version with non-existent version raises error"
    (clean-test-database)
    (db-migrate)
    (ok (signals (migrate-down-version "20250199-000000-nonexistent") 'error)
        "Should raise error for non-existent migration")))


;;; Test: migrate-down-version (middle migration)

(deftest migrate-down-version-middle-migration-test
  (testing "migrate-down-version can rollback a middle migration"
    (clean-test-database)
    ;; Apply all migrations
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations initially")
      (ok (column-exists-p connection "users" "age")
          "Users table should have age column initially"))

    ;; Rollback the 2nd migration (add age to users)
    (migrate-down-version "20250102-000000-add-age-to-users")

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 4 (length migrations))
            "Should have 4 migrations after rollback")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should still have users table migration")
        (ok (not (member "20250102-000000-add-age-to-users" migrations :test #'string=))
            "Should not have add age migration after rollback")
        (ok (member "20250103-000000-create-posts-table" migrations :test #'string=)
            "Should still have posts table migration")
        (ok (member "20250104-000000-add-published-to-posts" migrations :test #'string=)
            "Should still have add published migration")
        (ok (member "20250105-000000-create-comments-table" migrations :test #'string=)
            "Should still have comments table migration"))

      (ok (table-exists-p connection "users")
          "Users table should still exist")
      (ok (not (column-exists-p connection "users" "age"))
          "Users table should not have age column after rollback")
      (ok (table-exists-p connection "posts")
          "Posts table should still exist")
      (ok (table-exists-p connection "comments")
          "Comments table should still exist"))))


;;; Test: db-rollback (single step)

(deftest db-rollback-single-step-test
  (testing "db-rollback rolls back the last migration"
    (clean-test-database)
    ;; Apply all migrations
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations before rollback")
      (ok (table-exists-p connection "comments")
          "Comments table should exist before rollback"))

    ;; Rollback once
    (db-rollback)

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 4 (length migrations))
            "Should have 4 migrations after rollback")
        (ok (not (member "20250105-000000-create-comments-table" migrations :test #'string=))
            "Should not have comments table migration after rollback"))

      (ok (not (table-exists-p connection "comments"))
          "Comments table should not exist after rollback")
      (ok (table-exists-p connection "posts")
          "Posts table should still exist"))))


;;; Test: db-rollback (multiple steps)

(deftest db-rollback-multiple-steps-test
  (testing "db-rollback with step parameter rolls back multiple migrations"
    (clean-test-database)
    ;; Apply all migrations
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations before rollback"))

    ;; Rollback 3 steps
    (db-rollback :step 3)

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 2 (length migrations))
            "Should have 2 migrations after rollback")
        (ok (member "20250101-000000-create-users-table" migrations :test #'string=)
            "Should still have users table migration")
        (ok (member "20250102-000000-add-age-to-users" migrations :test #'string=)
            "Should still have add age migration")
        (ok (not (member "20250103-000000-create-posts-table" migrations :test #'string=))
            "Should not have posts table migration")
        (ok (not (member "20250104-000000-add-published-to-posts" migrations :test #'string=))
            "Should not have add published migration")
        (ok (not (member "20250105-000000-create-comments-table" migrations :test #'string=))
            "Should not have comments table migration"))

      (ok (table-exists-p connection "users")
          "Users table should still exist")
      (ok (column-exists-p connection "users" "age")
          "Users table should still have age column")
      (ok (not (table-exists-p connection "posts"))
          "Posts table should not exist")
      (ok (not (table-exists-p connection "comments"))
          "Comments table should not exist"))))


;;; Test: db-rollback (rollback more than available)

(deftest db-rollback-exceeds-available-test
  (testing "db-rollback stops when no more migrations to rollback"
    (clean-test-database)
    ;; Apply only 2 migrations
    (db-migrate :version "20250102-000000-add-age-to-users")

    (with-db-connection-direct (connection)
      (ok (= 2 (length (get-migration-names connection)))
          "Should have 2 migrations before rollback"))

    ;; Try to rollback 5 steps (more than available)
    (db-rollback :step 5)

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 0 (length migrations))
            "Should have 0 migrations after rolling back all")
        (ok (not (table-exists-p connection "users"))
            "Users table should not exist after complete rollback")))))


;;; Test: Idempotent migrations

(deftest idempotent-migration-test
  (testing "Running db-migrate twice should be idempotent"
    (clean-test-database)
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations after first run"))

    ;; Run again
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should still have 5 migrations after second run")
      (ok (table-exists-p connection "users")
          "Users table should still exist")
      (ok (table-exists-p connection "posts")
          "Posts table should still exist")
      (ok (table-exists-p connection "comments")
          "Comments table should still exist"))))


;;; Test: Rollback and re-migrate

(deftest rollback-and-remigrate-test
  (testing "Can rollback and then re-migrate"
    (clean-test-database)
    ;; Apply all migrations
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations initially"))

    ;; Rollback 2 steps
    (db-rollback :step 2)

    (with-db-connection-direct (connection)
      (ok (= 3 (length (get-migration-names connection)))
          "Should have 3 migrations after rollback"))

    ;; Re-migrate
    (db-migrate)

    (with-db-connection-direct (connection)
      (ok (= 5 (length (get-migration-names connection)))
          "Should have 5 migrations after re-migration")
      (ok (table-exists-p connection "comments")
          "Comments table should exist again"))))
