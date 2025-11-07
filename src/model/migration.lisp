(in-package #:cl-user)
(defpackage #:clails/model/migration
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*
                #:*migration-base-dir*)
  (:import-from #:clails/util
                #:kebab->snake)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct
                #:with-db-connection
                #:startup-connection-pool
                #:shutdown-connection-pool)
  (:import-from #:clails/model/base-model
                #:initialize-table-information)
  (:import-from #:clails/logger
                #:log.sql)
  (:export #:defmigration
           #:create-table
           #:add-column
           #:add-index
           #:drop-table
           #:drop-column
           #:drop-index
           #:create-table-impl
           #:add-column-impl
           #:add-index-impl
           #:drop-table-impl
           #:drop-column-impl
           #:drop-index-impl
           #:db-create
           #:db-migrate
           #:migrate-up-version
           #:migrate-down-version
           #:db-rollback
           #:check-type-valid
           #:db-seed))
(in-package #:clails/model/migration)

;;; migration list
(defparameter *migrations* '()
  "List of all registered migrations.")

;; tables
(defparameter *tables* '()
  "List of table definitions tracked during migrations.")

;;; Types used in migration
(defparameter *type-list*
  '(:string :text :integer :float :decimal :datetime :date :time :boolean)
  "Valid column types for database migrations.")

;;; dummy connection
(defparameter *dummy-connection*
  (make-instance 'clails/environment::<database-type-dummy>)
  "Dummy database connection for testing.")

;;;
;;; Migration DSL
;;; up: migration up function. Takes CL-DBI:DBI-CONNECTION as an argument
;;; down: migration down function. Takes CL-DBI:DBI-CONNECTION as an argument
;;;
;;; usage:
;;; (defmigration "create-table-todo"
;;;   (:up #'(lambda (conn)
;;;            (create-table conn :table "todo"
;;;                               :columns '(("title" :type :string
;;;                                                   :not-null T)
;;;                                          ("done" :type :boolean
;;;                                                  :default-value NIL)))
;;;            (add-index conn :table "todo"
;;;                            :index "idx-title"
;;;                            :columns '("title")))
;;;    :down #'(lambda (conn)
;;;              (drop-table conn :table "todo"))))
;;;
(defmacro defmigration (migration-name body)
  "Define a database migration with up and down functions.

   @param migration-name [string] Unique name for this migration
   @param body [plist] Property list with :up and :down function specifications
   "
  `(uiop:appendf clails/model/migration::*migrations*
                 (list (list :migration-name ,migration-name
                             :up ,(getf body :up)
                             :down ,(getf body :down)))))


;;;;
;;;; Functions dependent on the implementation of each Database
;;;;

(defgeneric create-table-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of create table"))

(defgeneric add-column-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of add column"))

(defgeneric add-index-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of add index"))

(defgeneric drop-table-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of drop table"))

(defgeneric drop-column-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of drop column"))

(defgeneric drop-index-impl (database-type conn &rest args &key &allow-other-keys)
  (:documentation "Implementation of drop index"))

(defgeneric ensure-database-impl (database-type connection)
  (:documentation "Implementation of ensure database"))

(defgeneric ensure-migration-table-impl (database-type connection)
  (:documentation "Implementation of ensure migration table"))


;;;;
;;;; exported functions
;;;;

(defun create-table (conn &rest args &key &allow-other-keys)
  "Create a new database table according to the database implementation.

   Tracks the table definition and executes the database-specific implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @param columns [list] List of column specifications
   "
  (push `(:table ,(getf args :table)
          :columns ,(getf args :columns))
        *tables*)
  (apply #'create-table-impl *database-type* conn args))

(defun add-column (conn &rest args &key &allow-other-keys)
  "Add column to existing table according to the database implementation.

   Updates the tracked table definition and executes the database-specific implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @param columns [list] List of column specifications to add
   "
  (let ((table-name (getf args :table)))
    (loop for table in *tables*
          when (string= table-name
                        (getf table :table))
            do (uiop:appendf (getf table :columns)
                             (getf args :columns))
               (return nil)))
  (apply #'add-column-impl *database-type* conn args))

(defun add-index (conn &rest args &key &allow-other-keys)
  "Add index to table according to the database implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @param index [string] Index name
   @param columns [list] List of column names for the index
   "
  (apply #'add-index-impl *database-type* conn args))

(defun drop-table (conn &rest args &key &allow-other-keys)
  "Drop table according to the database implementation.

   Removes the table from tracked definitions and executes the database-specific implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name to drop
   "
  (let ((table-name (getf args :table)))
    (setf *tables*
          (remove-if #'(lambda (r)
                         (string-equal (getf r :table)
                                       table-name))
                     *tables*)))
  (apply #'drop-table-impl *database-type* conn args))

(defun drop-column (conn &rest args &key &allow-other-keys)
  "Drop column from table according to the database implementation.

   Updates the tracked table definition and executes the database-specific implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @param column [string] Column name to drop
   "
  (let* ((table-name (getf args :table))
         (column-name (getf args :column))
         (table (find-if #'(lambda (e)
                             (string-equal (getf e :table)
                                           table-name))
                         *tables*))
         (columns (getf table :columns)))
    (setf columns
          (remove-if #'(lambda (c)
                         (string-equal (car c)
                                       column-name))
                     columns))
    (setf (getf table :columns) columns))
  (apply #'drop-column-impl *database-type* conn args))

(defun drop-index (conn &rest args &key &allow-other-keys)
  "Drop index from table according to the database implementation.

   @param conn [dbi:<dbi-connection>] Database connection
   @param table [string] Table name
   @param index [string] Index name to drop
   "
  (apply #'drop-index-impl *database-type* conn args))

(defun db-create ()
  "Create database and migration table.

   Implementation of db/create command.
   "
  (ensure-database)
  (ensure-migration-table))

(defun db-migrate (&key version)
  "Load migration files and apply pending migrations.

   Implementation of db/migrate command. Also exports schema file after migration.

   @param version [string] Migration name to migrate up to (optional). If nil, runs all pending migrations.
   "
  (ensure-migration-table)
  (setf *migrations* nil)
  (load-migration-files)
  (if version
      (migrate-to-version version)
      (migrate-all))
  (export-schema-file))

(defun db-status ()
  "Display migration status.

   Shows which migrations have been applied and which are pending.
   "
  (setf *migrations* nil)
  (load-migration-files)
  (format t " STATUS    MIGRATION NAME~%")
  (format t "-----------------------------------------~%")
  (format t "~{~{ ~A~11T~A~}~%~}~%" (migrated-status)))

(defun check-type-valid (type)
  "Check if column type is valid.

   @param type [keyword] Column type to validate
   @condition error Signaled when type is not in *type-list*
   "
  (when (not (find type *type-list*))
    (error "type error: ~A" type)))


;;;;
;;;; internal functions
;;;;

(defun ensure-database ()
  (with-db-connection-direct (connection :no-database T)
    (ensure-database-impl *database-type* connection)))

(defun ensure-migration-table ()
  (with-db-connection-direct (connection)
    (ensure-migration-table-impl *database-type* connection)))

(defun load-migration-files ()
  (let ((files (directory (format NIL "~A/db/migrate/**/*.lisp" *migration-base-dir*))))
    (dolist (file files)
      (format t "loading migration file: ~A" file)
      (load file)
      (format t " ... done~%"))))

(defun migrated-status ()
  (with-db-connection-direct (connection)
    (loop for migration in *migrations*
          for migration-name = (getf migration :migration-name)
          collect (if (not-migrated-p connection migration-name)
                      `("DOWN" ,migration-name)
                      `("UP" ,migration-name)))))

(defun migrate-all ()
  (with-db-connection-direct (connection)
    (ensure-migration-table-impl *database-type* connection)
    (loop for migration in *migrations*
          do (let ((*database-type* (if (not-migrated-p connection (getf migration :migration-name) t)
                                        *database-type*
                                        *dummy-connection*)))
               (apply-migration connection migration)))))

(defun apply-migration (connection migration)
  (let* ((migration-name (getf migration :migration-name))
         (up-fn (getf migration :up)))
    (funcall up-fn connection)
    (insert-migration connection migration-name)))

(defun migrate-to-version (version)
  "Apply migrations up to specified version.

   @param version [string] Migration name to migrate up to
   "
  (with-db-connection-direct (connection)
    (ensure-migration-table-impl *database-type* connection)
    (loop for migration in *migrations*
          for migration-name = (getf migration :migration-name)
          do (let ((*database-type* (if (not-migrated-p connection migration-name t)
                                        *database-type*
                                        *dummy-connection*)))
               (apply-migration connection migration)
               (when (string= migration-name version)
                 (return))))))

(defun migrate-up-version (version)
  "Apply specific migration version only.

   @param version [string] Migration name to apply
   @condition error Migration not found
   "
  (ensure-migration-table)
  (setf *migrations* nil)
  (load-migration-files)
  (with-db-connection-direct (connection)
    (ensure-migration-table-impl *database-type* connection)
    (let ((migration (find-if #'(lambda (m)
                                  (string= (getf m :migration-name) version))
                              *migrations*)))
      (if migration
          (if (not-migrated-p connection version t)
              (progn
                (apply-migration connection migration)
                (format t "Applied migration: ~A~%" version))
              (format t "Migration already applied: ~A~%" version))
          (error "Migration not found: ~A" version))))
  (export-schema-file))

(defun migrate-down-version (version)
  "Rollback specific migration version only.

   @param version [string] Migration name to rollback
   @condition error Migration not found or down function not defined
   "
  (ensure-migration-table)
  (setf *migrations* nil)
  (load-migration-files)
  (with-db-connection-direct (connection)
    (ensure-migration-table-impl *database-type* connection)
    (let ((migration (find-if #'(lambda (m)
                                  (string= (getf m :migration-name) version))
                              *migrations*)))
      (if migration
          (if (not-migrated-p connection version t)
              (format t "Migration not yet applied: ~A~%" version)
              (progn
                (rollback-migration connection version)
                (format t "Rolled back migration: ~A~%" version)))
          (error "Migration not found: ~A" version))))
  (export-schema-file))

(defun db-rollback (&key (step 1))
  "Rollback the last N database migrations.

   @param step [integer] Number of migrations to rollback (default: 1)
   "
  (ensure-migration-table)
  (setf *migrations* nil)
  (load-migration-files)
  (with-db-connection-direct (connection)
    (loop for i from 1 to step
          for migration-name = (get-last-migrated-name connection)
          while migration-name
          do (rollback-migration connection migration-name)))
  (export-schema-file))

(defun get-last-migrated-name (connection)
  "Get the last applied migration name.

   @param connection [dbi:<dbi-connection>] Database connection
   @return [string] Last migration name
   @return [nil] If no migrations have been applied
   "
  (let* ((query (dbi:prepare connection
                             "select migration_name from migration order by created_at desc, migration_name desc limit 1"))
         (result (dbi:execute query))
         (row (dbi:fetch result)))
    (when row
      (getf row :|migration_name|))))

(defun rollback-migration (connection migration-name)
  "Rollback a specific migration.

   @param connection [dbi:<dbi-connection>] Database connection
   @param migration-name [string] Migration name to rollback
   @condition error Migration not found or down function not defined
   "
  (let ((migration (find-if #'(lambda (m)
                                (string= (getf m :migration-name) migration-name))
                            *migrations*)))
    (if migration
        (let ((down-fn (getf migration :down)))
          (when down-fn
            (log.sql (format nil "Rolling back migration: ~A" migration-name))
            (funcall down-fn connection)
            (delete-migration connection migration-name)
            (format t "Rolled back migration: ~A~%" migration-name)))
        (error "Migration not found: ~A" migration-name))))

(defun delete-migration (connection migration-name)
  "Delete migration record from migration table.

   @param connection [dbi:<dbi-connection>] Database connection
   @param migration-name [string] Migration name to delete
   "
  (dbi:do-sql connection "delete from migration where migration_name = ?" (list migration-name)))

(defun not-migrated-p (connection migration-name &optional verbose)
  (when verbose
    (log.sql (format nil "checking migration: ~A" migration-name)))
  (let* ((query (dbi:prepare connection "select * from migration where migration_name = ?"))
         (result (dbi:execute query (list migration-name))))
    (null (dbi:fetch result))))

(defun insert-migration (connection migration-name)
  (when (not (eq *database-type* *dummy-connection*))
    (dbi:do-sql connection "insert into migration (migration_name) values (?)" (list migration-name))))


(defun export-schema-file ()
  (clails/project/generate:gen/schema *tables*))

(defun db-seed ()
  "Seed the database with initial data from db/seeds.lisp.

   Implementation of db/seed command.
   Loads all model files before executing seeds to ensure models are available.
   "
  (let ((seeds-file (format nil "~A/db/seeds.lisp" *migration-base-dir*))
        (models-dir (format nil "~A/app/models/" *migration-base-dir*)))
    ;; Load all model files
    (when (probe-file models-dir)
      (dolist (model-file (uiop:directory-files models-dir "*.lisp"))
        (load model-file)))
    ;; Load seeds file
    (when (probe-file seeds-file)
      (format t "Seeding database from: ~A~%" seeds-file)
      ;; Load and execute seeds file
      (with-db-connection (_connection)
        (load seeds-file)))))
