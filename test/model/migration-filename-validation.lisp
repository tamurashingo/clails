(in-package #:cl-user)
(defpackage #:clails-test/model/migration-filename-validation
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
                #:valid-migration-filename-p))

(defpackage #:clails-test/model/db/migration-filename-validation
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/migration-filename-validation)


(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0014" "/app/test/data/0014-migration-filename-validation-test"))
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


;;; Test: valid-migration-filename-p (pure predicate, no database needed)

(deftest valid-migration-filename-p-test
  (testing "valid-migration-filename-p accepts and rejects filenames as expected"
    (ok (valid-migration-filename-p "20250612-093015-create-users-table.lisp")
        "hyphenated date-time-name convention (as documented) is valid")
    (ok (valid-migration-filename-p "20250612093015_create-users-table.lisp")
        "compact 14-digit timestamp with underscore separator (as gen-unique-name produces) is valid")
    (ok (valid-migration-filename-p "20250612093015-create-users-table.lisp")
        "compact 14-digit timestamp with hyphen separator is valid")
    (ok (not (valid-migration-filename-p "create-users-table.lisp"))
        "filename with no timestamp prefix at all is invalid")
    (ok (not (valid-migration-filename-p "2025061-093015-create-users-table.lisp"))
        "filename with a truncated date part is invalid")
    (ok (not (valid-migration-filename-p "20250612-09301-create-users-table.lisp"))
        "filename with a truncated time part is invalid")
    (ok (not (valid-migration-filename-p "20250612093015create-users-table.lisp"))
        "filename missing a separator before the name is invalid")
    (ok (not (valid-migration-filename-p "20250612-093015-create-users-table.txt"))
        "non-.lisp extension is invalid")))


;;; Test: db-migrate warns about, but still loads, a badly-named migration file

(deftest db-migrate-warns-on-bad-filename-test
  (testing "db-migrate warns about a migration file with a bad name but still applies it"
    (clean-test-database)
    (let ((output (with-output-to-string (*standard-output*)
                    (db-migrate))))
      (ok (search "badname.lisp" output)
          "warning output should mention the badly-named migration file")
      (ok (search "does not match the expected naming convention" output)
          "warning output should explain the problem")
      (ok (not (search "20250101-000000-create-widgets-table.lisp does not match" output))
          "warning should not be emitted for the well-named migration file"))

    (with-db-connection-direct (connection)
      (let ((migrations (get-migration-names connection)))
        (ok (= 2 (length migrations))
            "both the well-named and badly-named migrations should still be applied")
        (ok (member "20250101-000000-create-widgets-table" migrations :test #'string=)
            "the well-named migration should be applied")
        (ok (member "badname-migration" migrations :test #'string=)
            "the badly-named migration should still be applied despite the warning")))))
