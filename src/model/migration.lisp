(in-package #:cl-user)
(defpackage #:clails/model/migration
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/util
                #:kebab->snake)
  (:import-from #:clails/model/connection
                #:with-db-connection-direct)
  (:export #:migrate-all))
(in-package #:clails/model/migration)

(defparameter *tbl* '())

(defparameter *type-list*
  '(:string :text :integer :float :decimal :datetime :date :time :boolean))



(defun %db/create ()
  (with-db-connection-direct (connection :no-database T)
    (apply-create-database connection))
  (with-db-connection-direct (connection)
    (apply-create-migration-table connection)))


(defun apply-create-database (connection)
  (apply-create-database-impl *database-type* connection))

(defun apply-create-migration-table (connection)
  (apply-create-migration-table-impl *database-type* connection))

(defgeneric apply-create-database-impl (database-type connection)
  (:documentation "Implementation of create database"))

(defgeneric apply-create-migration-table-impl (database-type connection)
  (:documentation "Implementation of create migration table"))


;; db/migrate
;; - find . -type f -name '*.lisp' | sort
;; - already patched?
;;   - generate sql
;;   - apply sql
;;   - insert migration table
(defun %db/migrate (basedir)
  (with-db-connection-direct (connection)
    (let ((files (directory (format NIL "~A/db/migrate/**/*.lisp" basedir))))
      (dolist (file files)
        (let ((filename (file-namestring file))
              (migration-name (pathname-name file)))
          ;; TODO: log
          (format T "cheking migration: ~A~%" migration-name)
          (when (not-migrated-p connection migration-name)
            ;; TODO: log
            (format T "  ... need migration: ~A~%" migration-name)
            (with-open-file (in file)
              (loop for sexp = (read in nil nil)
                    while sexp
                    do (let ((sql (parse-migration sexp)))
                         (apply-migration connection sql migration-name))))))))))


(defun apply-sql (connection sql)
  (dbi:do-sql connection sql))

(defun not-migrated-p (connection migration-name)
  (let* ((query (dbi:prepare connection "select * from migration where migration_name = ?"))
         (result (dbi:execute query (list migration-name))))
    (null (dbi:fetch result))))

(defun apply-migration (connection sql migration-name)
  (dbi:do-sql connection sql)
  (dbi:do-sql connection "insert into migration (migration_name) values (?)" (list migration-name)))


(defun find-table (table)
  (assoc table *tbl* :test #'string=))

(defun parse-migration (s)
  "Parses the S-expression for migration, adds its content to *tbl*, and returns the SQL."
  (when (null s)
    (error "empty"))
  (when (atom s)
    (error "atom:~A" s))
  (let ((op (car s)))
    (cond ((string= "CREATE-TABLE" (symbol-name op))
           ;; (create-table "todo"
           ;;   (("title" :type :string)
           ;;    ("done" :type :boolean)))
           (let ((table-name (cadr s))
                 (body (caddr s)))
              (update-tbl-create-table table-name body)
              (generate-create-table table-name body)))
          ((string= "ADD-COLUMN" (symbol-name op))
           ;; (add-column "todo"
           ;;   (("done-at" :type :datetime)))
           (let ((table-name (cadr s))
                 (body (caddr s)))
              (update-tbl-add-column table-name body)
              (generate-add-column table-name body)))
          (t (error "unknown operation: ~A" op)))))

(defun generate-create-table (table body)
  (let* ((base '(("id" :type :integer
                      :not-null T
                      :primary-key T
                      :auto-increment T)
                  ("created-at" :type :datetime
                              :not-null T)
                  ("updated-at" :type :datetime
                              :not-null T)))
         (body (append base body)))
    (format NIL "create table ~A ( ~{ ~A~^, ~} )"
            table
            (parse-create-table body))))

(defun generate-add-column (table columns)
  (format nil "alter table ~A ~A" table (parse-add-column columns)))

(defun update-tbl-create-table (table columns)
  (when (find-table table)
    (error "~A already defined" table))
  (setq *tbl* (push (cons table columns) *tbl*)))

(defun update-tbl-add-column (table columns)
  (when (null (find-table table))
    (error "~A not defined yet" table))
  (let ((table-columns (cdr (find-table table))))
     (nconc table-columns columns)))

(defun parse-create-table (body)
  (when (null body)
    (error "emtpy body"))
  (loop for col in body
        collect (parse-column col)))

(defun parse-add-column (body)
  (when (null body)
    (error "emtpy body"))
  (parse-add-column-impl *database-type* (loop for col in body
                                               collect (parse-column col))))
  
(defun parse-column (col)
  (when (null col)
    (error "empty column"))
  (let* ((column_name (kebab->snake (car col)))
          (attr (cdr col))
          (type (getf attr :type))
          (not-null-p (getf attr :not-null))
          (primary-key-p (getf attr :primary-key))
          (auto-increment-p (getf attr :auto-increment)))
    (check-type-valid type)
    (create-column column_name type not-null-p primary-key-p auto-increment-p)))
  
(defun check-type-valid (type)
  (when (not (find type *type-list*))
    (error "type error: ~A" type)))

(defun create-column (column_name type not-null-p primary-key-p auto-increment-p)
  (create-column-impl *database-type* :column-name column_name
                                      :type type
                                      :not-null-p not-null-p
                                      :primary-key-p primary-key-p
                                      :auto-increment-p auto-increment-p))

(defgeneric parse-add-column-impl (database-type columns)
  (:documentation "Implementation of parse add column"))

