(in-package #:cl-user)
(defpackage #:clails/model/impl/postgresql
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-postgresql>
                #:*database-config*)
  (:import-from #:clails/model/migration
                #:create-table-impl
                #:add-column-impl
                #:add-index-impl
                #:drop-table-impl
                #:drop-column-impl
                #:drop-index-impl
                #:ensure-database-impl
                #:ensure-migration-table-impl
                #:check-type-valid)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl)
  (:import-from #:clails/util
                #:kebab->snake
                #:mandatory-check))
(in-package #:clails/model/impl/postgresql)

(defparameter *postgresql-type-convert*
  '((:string . "varchar(255)")
    (:text . "text")
    (:integer . "integer")
    (:float . "float")
    (:decimal . "decimal")
    (:datetime . "timestamp")
    (:date . "date")
    (:time . "time")
    (:boolean . "boolean")))

(defun type-convert (type)
  (cdr (assoc type *postgresql-type-convert*)))

(defmethod create-table-impl ((database-type <database-type-postgresql>) connection &key table columns constraints)
  (mandatory-check table columns)
  (let ((query (gen-create-table table (append '(("id" :type :integer
                                                       :not-null T
                                                       :primary-key T
                                                       :auto-increment T)
                                                 ("created-at" :type :datetime
                                                               :not-null T)
                                                 ("updated-at" :type :datetime
                                                               :not-null T))
                                               columns))))
    (dbi:do-sql connection query)))

(defmethod add-column-impl ((database-type <database-type-postgresql>) connection &key table columns)
  (mandatory-check table columns)
  (let ((query (gen-add-column table columns)))
    (dbi:do-sql connection query)))
  
(defmethod add-index-impl ((database-type <database-type-postgresql>) connection &key table index columns)
  (mandatory-check table index columns)
  (let ((query (gen-add-index table index columns)))
    (dbi:do-sql connection query)))

(defmethod drop-table-impl ((database-type <database-type-postgresql>) connection &key table)
  (mandatory-check table)
  (let ((query (gen-drop-table table)))
    (dbi:do-sql connection query)))

(defmethod drop-column-impl ((database-type <database-type-postgresql>) connection &key table column)
  (mandatory-check table column)
  (let ((query (gen-drop-column table column)))
    (dbi:do-sql connection query)))

(defmethod drop-index-impl ((database-type <database-type-postgresql>) connection &key table index)
  (mandatory-check table index)
  (let ((query (gen-drop-index table index)))
    (dbi:do-sql connection query)))


(defun gen-create-table (table columns)
  (format NIL "CREATE TABLE ~A (~{~A~^, ~})" (kebab->snake table)
              (loop for col in columns
                    collect (parse-column col))))

(defun gen-add-column (table columns)
  (format NIL "ALTER TABLE ~A ~{ ADD COLUMN ~A ~^, ~}"
              table
              (loop for col in columns
                    collect (parse-column col))))

(defun gen-drop-table (table)
  (format NIL "DROP TABLE ~A" (kebab->snake table)))

(defun gen-add-index (table index columns)
  (format NIL "CREATE INDEX ~A ON ~A (~{~A~^, ~})"
              (kebab->snake index)
              (kebab->snake table)
              (loop for col in columns
                    collect (kebab->snake col))))

(defun gen-drop-column (table columns)
  (format NIL "ALTER TABLE ~A ~{ DROP COLUMN ~A ~^, ~}"
              (kebab->snake table)
              (loop for col in columns
                    collect (kebab->snake col))))

(defun gen-drop-index (table index)
  (format NIL "DROP INDEX ~A ON ~A"
              (kebab->snake index)
              (kebab->snake table)))


(defun parse-column (col)
  (let* ((column-name (kebab->snake (first col)))
         (attr (rest col))
         (type (getf attr :type))
         (not-null-p (getf attr :not-null))
         (primary-key-p (getf attr :primary-key))
         (auto-increment-p (getf attr :auto-increment))
         (default-value (getf attr :default-value)))
    (check-type-valid type)
    (create-column column-name type not-null-p primary-key-p auto-increment-p default-value)))


(defun create-column (column-name type not-null-p primary-key-p auto-increment-p default-value)
  (format NIL " ~A ~:[~A~;SERIAL~*~]~@[ NOT NULL~*~]~@[ PRIMARY KEY~*~]~@[ DEFAULT ~A~]"
              column-name
              auto-increment-p
              (type-convert type)
              not-null-p
              primary-key-p
              default-value))


(defmethod get-connection-direct-impl ((database-type <database-type-postgresql>) &key no-database)
  (let ((database-name (getf *database-config* :database))
        (host (getf *database-config* :host))
        (port (parse-integer (getf *database-config* :port)))
        (username (getf *database-config* :user))
        (password (getf *database-config* :password)))
    (if no-database
        (dbi:connect :postgres
                     :database-name "postgres"
                     :host host
                     :port port
                     :username username
                     :password password)
        (dbi:connect :postgres
                     :database-name database-name
                     :host host
                     :port port
                     :username username
                     :password password))))


(defparameter CREATE-DATABASE
  "CREATE DATABASE ~A ENCODING = 'UTF8'")

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE IF NOT EXISTS migration (~
                 migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                 created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))


(defmethod ensure-database-impl ((database-type <database-type-postgresql>) connection)
  (let ((database-name (getf *database-config* :database)))
    (dbi:do-sql connection (format NIL CREATE-DATABASE database-name))))

(defmethod ensure-migration-table-impl ((database-type <database-type-postgresql>) connection)
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

