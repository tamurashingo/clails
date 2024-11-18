(in-package #:cl-user)
(defpackage #:clails/model/impl/mysql
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-mysql>
                #:*database-config*)
  (:import-from #:clails/model/migration
                #:create-table-impl
                #:add-column-impl
                #:add-index-impl
                #:drop-table-impl
                #:drop-column-impl
                #:drop-index-impl
                #:ensure-database-impl
                #:ensure-migration-table-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl)
  (:import-from #:clails/util
                #:kebab->snake
                #:mandatory-check))
(in-package #:clails/model/impl/mysql)

(defparameter *mysql-type-convert*
  '((:string . "varchar(255)")
    (:text . "text")
    (:integer . "integer")
    (:float . "float")
    (:decimal . "decimal")
    (:datetime . "datetime")
    (:date . "date")
    (:time . "time")
    (:boolean . "boolean")))

(defun type-convert (type)
  (cdr (assoc type *mysql-type-convert*)))


(defmethod create-table-impl ((database-type <database-type-mysql>) connection &key table columns constraints)
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

(defmethod add-column-impl ((database-type <database-type-mysql>) connection &key table columns)
  (mandatory-check table columns)
  (let ((query (gen-add-column table columns)))
    (dbi:do-sql connection query)))
  
(defmethod add-index-impl ((database-type <database-type-mysql>) connection &key table index columns)
  (mandatory-check table index columns)
  (let ((query (gen-add-index table index columns)))
    (dbi:do-sql connection query)))

(defmethod drop-table-impl ((database-type <database-type-mysql>) connection &key table)
  (mandatory-check table)
  (let ((query (gen-drop-table table)))
    (dbi:do-sql connection query)))

(defmethod drop-column-impl ((database-type <database-type-mysql>) connection &key table column)
  (mandatory-check table column)
  (let ((query (gen-drop-column table column)))
    (dbi:do-sql connection query)))

(defmethod drop-index-impl ((database-type <database-type-mysql>) connection &key table index)
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
    ;(check-type-valid type)
    (create-column column-name type not-null-p primary-key-p auto-increment-p default-value)))


(defun create-column (column-name type not-null-p primary-key-p auto-increment-p default-value)
  (format NIL " ~A ~A~@[ NOT NULL~*~]~@[ PRIMARY KEY~*~]~@[ AUTO_INCREMENT~*~]~@[ DEFAULT ~A~]"
              column-name
              (type-convert type)
              not-null-p
              primary-key-p
              auto-increment-p
              default-value))


(defmethod get-connection-direct-impl ((database-type <database-type-mysql>) &key no-database)
  (let ((database-name (getf *database-config* :database))
        (host (getf *database-config* :host))
        (port (parse-integer (getf *database-config* :port)))
        (username (getf *database-config* :user))
        (password (getf *database-config* :password)))
    (if no-database
        (dbi:connect :mysql
                     :host host
                     :port port
                     :username username
                     :password password)
        (dbi:connect :mysql
                     :database-name database-name
                     :host host
                     :port port
                     :username username
                     :password password))))


(defparameter CREATE-DATABASE
  "CREATE DATABASE IF NOT EXISTS ~A CHARACTER SET utf8mb4 COLLATE utf8mb4_bin")

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE IF NOT EXISTS migration (~
                 migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                 created_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))


(defmethod ensure-database-impl ((database-type <database-type-mysql>) connection)
  (let ((database-name (getf *database-config* :database)))
    (dbi:do-sql connection (format NIL CREATE-DATABASE database-name))))

(defmethod ensure-migration-table-impl ((database-type <database-type-mysql>) connection)
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

