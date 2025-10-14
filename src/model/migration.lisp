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
                #:with-db-connection)
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
           #:check-type-valid))
(in-package #:clails/model/migration)

;;; migration list
(defparameter *migrations* '())

;; tables
(defparameter *tables* '())

;;; Types used in migration
(defparameter *type-list*
  '(:string :text :integer :float :decimal :datetime :date :time :boolean))

;;; dummy connection
(defparameter *dummy-connection*
  (make-instance 'clails/environment::<database-type-dummy>))

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
  "Migration DSL"
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
  "Run create table query according to the database implementation"
  (push `(:table ,(getf args :table)
          :columns ,(getf args :columns))
        *tables*)
  (apply #'create-table-impl *database-type* conn args))

(defun add-column (conn &rest args &key &allow-other-keys)
  "Run add column query according to the database implementation"
  (let ((table-name (getf args :table)))
    (loop for table in *tables*
          when (string= table-name
                        (getf table :table))
            do (uiop:appendf (getf table :columns)
                             (getf args :columns))
               (return nil)))
  (apply #'add-column-impl *database-type* conn args))

(defun add-index (conn &rest args &key &allow-other-keys)
  "Run add index query according to the database implementation"
  (apply #'add-index-impl *database-type* conn args))

(defun drop-table (conn &rest args &key &allow-other-keys)
  "Run drop table query according to the database implementation"
  (let ((table-name (getf args :table)))
    (setf *tables*
          (remove-if #'(lambda (r)
                         (string-equal (getf r :table)
                                       table-name))
                     *tables*)))
  (apply #'drop-table-impl *database-type* conn args))

(defun drop-column (conn &rest args &key &allow-other-keys)
  "Run drop column query according to the database implementation"
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
  "Run drop index query according to the database implementation"
  (apply #'drop-index-impl *database-type* conn args))

(defun db-create ()
  "implementation of `db/create`. create database and migration table"
  (ensure-database)
  (ensure-migration-table))

(defun db-migrate ()
  "implementation of `db/migrate`. load migration files and apply migrations"
  (ensure-migration-table)
  (setf *migrations* nil)
  (load-migration-files)
  (migrate-all)
  (export-schema-file))

(defun db-status ()
  (setf *migrations* nil)
  (load-migration-files)
  (log.sql " STATUS    MIGRATION NAME")
  (log.sql "-----------------------------------------")
  (log.sql (format nil "~{~{ ~A~11T~A~}~%~}" (migrated-status))))

(defun check-type-valid (type)
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
      (log.sql (format nil "loading migration file: ~A" file))
      (load file)
      (log.sql " ... done"))))

(defun migrated-status ()
  (with-db-connection-direct (connection)
    (loop for migration in *migrations*
          for migration-name = (getf migration :migration-name)
          collect (if (not-migrated-p connection migration-name)
                      `("DOWN" ,migration-name)
                      `("UP" ,migration-name)))))

(defun migrate-all ()
  (with-db-connection-direct (connection)
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

