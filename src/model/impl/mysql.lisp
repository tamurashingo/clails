(in-package #:cl-user)
(defpackage #:clails/model/impl/mysql
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-mysql>
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
                #:fetch-columns-and-types-impl
                :fetch-columns-and-types-plist-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl
                #:create-connection-pool-impl)
  (:import-from #:clails/model/query
                #:get-last-id-impl)
  (:import-from #:clails/logger
                #:log.sql)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab
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

(defparameter *mysql-type-convert-functions*
  `(("varchar" . (:type :string
                  :db-cl-fn ,#'identity
                  :cl-db-fn ,#'identity))
    ("text" . (:type :text
               :db-cl-fn ,#'(lambda (txt) (when txt
                                            (babel:octets-to-string txt)))
               :cl-db-fn ,#'identity))
    ("int" . (:type :integer
              :db-cl-fn ,#'identity
              :cl-db-fn ,#'identity))
    ("float" . (:type :float
                :db-cl-fn ,#'identity
                :cl-db-fn ,#'identity))
    ("decimal" . (:type :decimal
                  :db-cl-fn ,#'(lambda (v) (when v
                                             (coerce v 'double-float)))
                  :cl-db-fn ,#'identity))
    ("datetime" . (:type :datetime
                   :db-cl-fn ,#'identity
                   :cl-db-fn ,#'(lambda (ut)
                                  (when ut
                                    (multiple-value-bind (sec min hour date month year day daylight-p zone)
                                        (decode-universal-time ut)
                                      (format nil "~4,\'0d-~2,\'0d-~2,\'0d ~2,\'0d:~2,\'0d:~2,\'0d" year month date hour min sec))))))
    ("date" . (:type :date
               :db-cl-fn ,#'identity
               :cl-db-fn ,#'identity))
    ("time" . (:type :time
               :db-cl-fn ,#'identity
               :cl-db-fn ,#'identity))
    ("tinyint" . (:type :boolean
                  :db-cl-fn ,#'(lambda (v)
                                 (cond ((null v) nil)
                                       ((and (numberp v)
                                             (= v 0))
                                        nil)
                                       (t t)))
                  :cl-db-fn ,#'(lambda (v)
                                 (cond ((or (and (numberp v)
                                                (> v 0))
                                           (eq v t))
                                        1)
                                       (t 0)))))))

(defparameter *mysql-type-convert-unknown-type* :string)
(defparameter *mysql-type-convert-unknown-function* #'identity)


(defun type-convert (type)
  (cdr (assoc type *mysql-type-convert*)))


(defun convert-default-value (value column-type)
  "Convert Lisp value to SQL DEFAULT clause string for MySQL.

   @param value [t] Default value in Lisp representation
   @param column-type [keyword] Column type (:string, :integer, :boolean, etc.)
   @return [string] SQL DEFAULT clause value
   "
  (cond
    ;; Special keywords
    ((eq value :null) "NULL")
    ((eq value :current-timestamp) "CURRENT_TIMESTAMP")

    ;; Boolean type - MySQL uses TINYINT(1)
    ((eq column-type :boolean)
     (cond ((eq value t) "1")
           ((null value) "0")
           (t (error "Invalid boolean default value: ~A. Use T or NIL." value))))

    ;; Numeric types
    ((numberp value) (format nil "~A" value))

    ;; String types - need to quote the value
    ((and (or (eq column-type :string)
              (eq column-type :text))
          (stringp value))
     (format nil "'~A'" value))

    ;; Datetime types
    ((and (or (eq column-type :datetime)
              (eq column-type :date)
              (eq column-type :time))
          (stringp value))
     (format nil "'~A'" value))

    ;; Fallback for other string values
    ((stringp value) value)

    ;; Error for unsupported types
    (t (error "Unsupported default value type: ~A for column type ~A"
              value column-type))))

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
    (log.sql query :table table :columns columns :constraints constraints)
    (dbi:do-sql connection query)))

(defmethod add-column-impl ((database-type <database-type-mysql>) connection &key table columns)
  (declare (ignore database-type))
  (mandatory-check table columns)
  (let ((query (gen-add-column table columns)))
    (log.sql query :table table :columns columns)
    (dbi:do-sql connection query)))

(defmethod add-index-impl ((database-type <database-type-mysql>) connection &key table index columns)
  (declare (ignore database-type))
  (mandatory-check table index columns)
  (let ((query (gen-add-index table index columns)))
    (log.sql query :table table :index index :columns columns)
    (dbi:do-sql connection query)))

(defmethod drop-table-impl ((database-type <database-type-mysql>) connection &key table)
  (declare (ignore database-type))
  (mandatory-check table)
  (let ((query (gen-drop-table table)))
    (log.sql query :table table)
    (dbi:do-sql connection query)))

(defmethod drop-column-impl ((database-type <database-type-mysql>) connection &key table column)
  (declare (ignore database-type))
  (mandatory-check table column)
  (let ((query (gen-drop-column table column)))
    (log.sql query :table table :column column)
    (dbi:do-sql connection query)))

(defmethod drop-index-impl ((database-type <database-type-mysql>) connection &key table index)
  (declare (ignore database-type))
  (mandatory-check table index)
  (let ((query (gen-drop-index table index)))
    (log.sql query :table table :index index)
    (dbi:do-sql connection query)))

(defmethod fetch-columns-and-types-impl ((database-type <database-type-mysql>) connection table)
  (declare (ignore database-type))
  ;; select column_name, column_type, data_type, character_maximum_length, NUMERIC_PRECISION, NUMERIC_SCALE, DATETIME_PRECISION from information_schema.columns where table_name = 'todo'
  (let ((sql "select column_name, data_type from information_schema.columns where table_name = ? order by ordinal_position"))
    (log.sql sql :table table)
    (let* ((query (dbi:prepare connection sql))
           (result (dbi:execute query (list table))))
      (loop for row = (dbi:fetch result)
            while row
            collect (let* ((name (intern (snake->kebab (string-upcase (getf row :COLUMN_NAME))) :KEYWORD))
                          (access (intern (string-upcase (getf row :COLUMN_NAME)) :KEYWORD))
                          (inv (cdr (assoc (string-downcase (babel:octets-to-string (getf row :DATA_TYPE))) *mysql-type-convert-functions* :test #'string=)))
                          (type (if inv (getf inv :type)
                                          *mysql-type-convert-unknown-type*))
                          (db-cl-fn (if inv (getf inv :db-cl-fn)
                                            *mysql-type-convert-unknown-function*))
                          (cl-db-fn (if inv (getf inv :cl-db-fn)
                                            *mysql-type-convert-unknown-function*)))
                      (list :name name
                            :access access
                            :type type
                            :db-cl-fn db-cl-fn
                            :cl-db-fn cl-db-fn))))))

(defmethod fetch-columns-and-types-plist-impl ((database-type <database-type-mysql>) connection table)
  (declare (ignore database-type))
  ;; select column_name, column_type, data_type, character_maximum_length, NUMERIC_PRECISION, NUMERIC_SCALE, DATETIME_PRECISION from information_schema.columns where table_name = 'todo'
  (let ((sql "select column_name, data_type from information_schema.columns where table_name = ? order by ordinal_position"))
    (log.sql sql :table table)
    (let* ((query (dbi:prepare connection sql))
          (result (dbi:execute query (list table))))
      (loop for row = (dbi:fetch result)
            while row
            append (let* ((name (intern (snake->kebab (string-upcase (getf row :COLUMN_NAME))) :KEYWORD))
                          (access (intern (string-upcase (getf row :COLUMN_NAME)) :KEYWORD))
                          (inv (cdr (assoc (string-downcase (babel:octets-to-string (getf row :DATA_TYPE))) *mysql-type-convert-functions* :test #'string=)))
                          (type (if inv (getf inv :type)
                                          *mysql-type-convert-unknown-type*))
                          (db-cl-fn (if inv (getf inv :db-cl-fn)
                                            *mysql-type-convert-unknown-function*))
                          (cl-db-fn (if inv (getf inv :cl-db-fn)
                                            *mysql-type-convert-unknown-function*)))
                      (list name (list :name name
                                      :access access
                                      :type type
                                      :db-cl-fn db-cl-fn
                                      :cl-db-fn cl-db-fn)))))))

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
  "Parse a column definition for MySQL CREATE/ALTER TABLE statement.

   Converts Lisp-style column definition to SQL column specification,
   including automatic conversion of default values using convert-default-value.

   @param col [list] Column definition: (name :type type :not-null bool :default-value value ...)
   @return [string] SQL column specification
   "
  (let* ((column-name (kebab->snake (first col)))
         (attr (rest col))
         (type (getf attr :type))
         (not-null-p (getf attr :not-null))
         (primary-key-p (getf attr :primary-key))
         (auto-increment-p (getf attr :auto-increment))
         (default-value-raw (getf attr :default-value))
         ;; Convert default value to SQL string
         (default-value (when default-value-raw
                          (convert-default-value default-value-raw type)))
         (precision (when (or (eq type :float)
                              (eq type :decimal))
                      (getf attr :precision)))
         (scale (when (or (eq type :float)
                          (eq type :decimal))
                  (getf attr :scale))))
    (check-type-valid type)
    (create-column column-name type precision scale not-null-p primary-key-p auto-increment-p default-value)))


(defun create-column (column-name type precision scale not-null-p primary-key-p auto-increment-p default-value)
  (format NIL " ~A ~A~:[~2*~;(~A,~A)~]~@[ NOT NULL~*~]~@[ PRIMARY KEY~*~]~@[ AUTO_INCREMENT~*~]~@[ DEFAULT ~A~]"
              column-name
              (type-convert type)
              (and precision scale)
              precision
              scale
              not-null-p
              primary-key-p
              auto-increment-p
              default-value))


(defmethod get-connection-direct-impl ((database-type <database-type-mysql>) &key no-database)
  (declare (ignore database-type))
  (let* ((config (getf *database-config* *project-environment*))
         (database-name (getf config :database-name))
         (host (getf config :host))
         (port (parse-integer (getf config :port)))
         (username (getf config :username))
         (password (getf config :password)))
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

(defmethod create-connection-pool-impl ((database-type <database-type-mysql>))
  (declare (ignore database-type))
  (let* ((config (getf *database-config* *project-environment*))
         (database-name (getf config :database-name))
         (host (getf config :host))
         (port (parse-integer (getf config :port)))
         (username (getf config :username))
         (password (getf config :password)))
    (dbi-cp:make-dbi-connection-pool :mysql
                                     :database-name database-name
                                     :host host
                                     :port port
                                     :username username
                                     :password password)))


(defparameter CREATE-DATABASE
  "CREATE DATABASE IF NOT EXISTS ~A CHARACTER SET utf8mb4 COLLATE utf8mb4_bin")

(defparameter CREATE-MIGRATION-TABLE
  (format NIL "CREATE TABLE IF NOT EXISTS migration (~
                 migration_name varchar(255) NOT NULL PRIMARY KEY, ~
                 created_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))


(defmethod ensure-database-impl ((database-type <database-type-mysql>) connection)
  (declare (ignore database-type))
  (let ((database-name (getf (getf *database-config* *project-environment*)
                             :database-name)))
    (dbi:do-sql connection (format NIL CREATE-DATABASE database-name))))

(defmethod ensure-migration-table-impl ((database-type <database-type-mysql>) connection)
  (declare (ignore database-type))
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))


(defmethod get-last-id-impl ((databse-type <database-type-mysql>) connection)
  (declare (ignore databse-type))

  (let ((result (dbi-cp:execute
                 (dbi-cp:prepare connection "select last_insert_id()")
                 '())))
    (getf (dbi-cp:fetch result) :|last_insert_id()|)))


