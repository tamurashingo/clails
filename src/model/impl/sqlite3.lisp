(in-package #:cl-user)
(defpackage #:clails/model/impl/sqlite3
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-sqlite3>
                #:*project-environment*
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
  (:import-from #:clails/model/base-model
                #:fetch-columns-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl
                #:create-connection-pool-impl)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab
                #:mandatory-check))
(in-package #:clails/model/impl/sqlite3)

(defparameter *sqlite3-type-convert*
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
  (cdr (assoc type *sqlite3-type-convert*)))

(defmethod create-table-impl ((database-type <database-type-sqlite3>) connection &key table columns constraints)
  (declare (ignore database-type))
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

(defmethod add-column-impl ((database-type <database-type-sqlite3>) connection &key table columns)
  (declare (ignore database-type))
  (mandatory-check table columns)
  (let ((query (gen-add-column table columns)))
    (dbi:do-sql connection query)))

(defmethod add-index-impl ((database-type <database-type-sqlite3>) connection &key table index columns)
  (declare (ignore database-type))
  (mandatory-check table index columns)
  (let ((query (gen-add-index table index columns)))
    (dbi:do-sql connection query)))

(defmethod drop-table-impl ((database-type <database-type-sqlite3>) connection &key table)
  (declare (ignore database-type))
  (mandatory-check table)
  (let ((query (gen-drop-table table)))
    (dbi:do-sql connection query)))

(defmethod drop-column-impl ((database-type <database-type-sqlite3>) connection &key table column)
  (declare (ignore database-type))
  (mandatory-check table column)
  (let ((query (gen-drop-column table column)))
    (dbi:do-sql connection query)))

(defmethod drop-index-impl ((database-type <database-type-sqlite3>) connection &key table index)
  (declare (ignore database-type))
  (mandatory-check table index)
  (let ((query (gen-drop-index table index)))
    (dbi:do-sql connection query)))

(defmethod fetch-columns-impl ((database-type <database-type-sqlite3>) connection table)
  (declare (ignore database-type))
  (let* ((query (dbi:prepare connection (format NIL "pragma table_info('~A')" table)))
         (result (dbi:execute query '())))
    (loop for row = (dbi:fetch result)
          while row
          collect (intern (snake->kebab (string-upcase (getf row :|name|))) :KEYWORD))))

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
  (format NIL " ~A ~A~@[ NOT NULL~*~]~@[ PRIMARY KEY~*~]~@[ AUTOINCREMENT~*~]~@[ DEFAULT ~A~]"
              column-name
              (type-convert type)
              not-null-p
              primary-key-p
              auto-increment-p
              default-value))


(defmethod get-connection-direct-impl ((database-type <database-type-sqlite3>) &key no-database)
  (declare (ignore database-type))
  (let ((database-name (getf (getf *database-config* *project-environment*)
                             :database-name)))
    (dbi:connect :sqlite3
                 :database-name database-name)))

(defmethod create-connection-pool-impl ((database-type <database-type-sqlite3>))
  (declare (ignore database-type))
  (let ((database-name (getf (getf *database-config* *project-environment*)
                             :database-name)))
    (dbi-cp:make-dbi-connection-pool :sqlite3
                                     :database-name database-name)))


(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE IF NOT EXISTS migration (~
                  migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                  created_at dateteime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))

(defmethod ensure-database-impl ((database-type <database-type-sqlite3>) connection)
  (declare (ignore database-type connection))
  'NOP)

(defmethod ensure-migration-table-impl ((database-type <database-type-sqlite3>) connection)
  (declare (ignore database-type))
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))

