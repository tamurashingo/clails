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
                #:fetch-columns-and-types-impl
                #:fetch-columns-and-types-plist-impl)
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

(defparameter *sqlite3-type-convert-functions*
  `(("varchar" . (:type :string
                  :db-cl-fn ,#'identity
                  :cl-db-fn ,#'identity))
    ("text" . (:type :text
               :db-cl-fn ,#'identity
               :cl-db-fn ,#'identity))
    ("integer" . (:type :integer
                  :db-cl-fn ,#'identity
                  :cl-db-fn ,#'identity))
    ("float" . (:type :float
                :db-cl-fn ,#'identity
                :cl-db-fn ,#'identity))
    ("decimal" . (:type :decimal
                  :db-cl-fn ,#'identity
                  :cl-db-fn ,#'identity))
    ("datetime" . (:type :datetime
                   :db-cl-fn ,#'(lambda (v)
                                  (when v
                                    ; 0123456789012345678
                                    ; yyyy-mm-dd hh:mi:ss
                                    (encode-universal-time
                                     (parse-integer (subseq v 17 19))
                                     (parse-integer (subseq v 14 16))
                                     (parse-integer (subseq v 11 13))
                                     (parse-integer (subseq v 8 10))
                                     (parse-integer (subseq v 5 7))
                                     (parse-integer (subseq v 0 4)))))
                   :cl-db-fn ,#'(lambda (ut)
                                  (when ut
                                    (multiple-value-bind (sec min hour date month year day daylight-p zone)
                                        (decode-universal-time ut)
                                      (format nil "~4,\'0d-~2,\'0d-~2,\'0d ~2,\'0d:~2,\'0d:~2,\'0d" year month date hour min sec))))))
    ("date" . (:type :date
               :db-cl-fn ,#'(lambda (v)
                              (when v
                                ; 0123456789
                                ; yyyy-mm-dd
                                (encode-universal-time
                                 0
                                 0
                                 0
                                 (parse-integer (subseq v 8 10))
                                 (parse-integer (subseq v 5 7))
                                 (parse-integer (subseq v 0 4)))))
               :cl-db-fn ,#'identity))
    ("time" . (:type :time
               :db-cl-fn ,#'(lambda (v)
                              (when v
                                ; 012345
                                ; hh:mi:ss
                                (+ (* 60 60 (parse-integer (subseq v 0 2)))
                                   (* 60 (parse-integer (subseq v 3 5)))
                                   (parse-integer (subseq v 6 8)))))
               :cl-db-fn ,#'identity))
    ("boolean" . (:type :boolean
                  :db-cl-fn ,#'(lambda (v)
                                 (if (or (null v)
                                         (eq v 0)) nil
                                                   t))
                  :cl-db-fn ,#'(lambda (v)
                                 (if v 1
                                       0))))))

(defparameter *sqlite3-type-convert-unknown-type* :string)
(defparameter *sqlite3-type-convert-unknown-function* #'identity)


(defun type-convert (type)
  (cdr (assoc type *sqlite3-type-convert*)))


(defun convert-default-value (value column-type)
  "Convert Lisp value to SQL DEFAULT clause string for SQLite3.

   @param value [t] Default value in Lisp representation
   @param column-type [keyword] Column type (:string, :integer, :boolean, etc.)
   @return [string] SQL DEFAULT clause value
   "
  (cond
    ;; Special keywords
    ((eq value :null) "NULL")
    ((eq value :current-timestamp) "CURRENT_TIMESTAMP")

    ;; Boolean type - SQLite stores as INTEGER (0 or 1)
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
    (log.sql query :table table :columns columns :constraints constraints)
    (dbi:do-sql connection query)))

(defmethod add-column-impl ((database-type <database-type-sqlite3>) connection &key table columns)
  (declare (ignore database-type))
  (mandatory-check table columns)
  (let ((query (gen-add-column table columns)))
    (log.sql query :table table :columns columns)
    (dbi:do-sql connection query)))

(defmethod add-index-impl ((database-type <database-type-sqlite3>) connection &key table index columns)
  (declare (ignore database-type))
  (mandatory-check table index columns)
  (let ((query (gen-add-index table index columns)))
    (log.sql query :table table :index index :columns columns)
    (dbi:do-sql connection query)))

(defmethod drop-table-impl ((database-type <database-type-sqlite3>) connection &key table)
  (declare (ignore database-type))
  (mandatory-check table)
  (let ((query (gen-drop-table table)))
    (log.sql query :table table)
    (dbi:do-sql connection query)))

(defmethod drop-column-impl ((database-type <database-type-sqlite3>) connection &key table column)
  (declare (ignore database-type))
  (mandatory-check table column)
  (let ((columns (if (listp column) column (list column))))
    (loop for col in columns
          do (let ((query (gen-drop-column table (list col))))
               (log.sql query :table table :column col)
               (dbi:do-sql connection query)))))

(defmethod drop-index-impl ((database-type <database-type-sqlite3>) connection &key table index)
  (declare (ignore database-type))
  (mandatory-check table index)
  (let ((query (gen-drop-index table index)))
    (log.sql query :table table :index index)
    (dbi:do-sql connection query)))

(defmethod fetch-columns-and-types-impl ((database-type <database-type-sqlite3>) connection table)
  (declare (ignore database-type))
  (let ((sql (format NIL "pragma table_info('~A')" table)))
    (log.sql sql :table table)
    (let* ((query (dbi:prepare connection sql))
          (result (dbi:execute query '())))
      (loop for row = (dbi:fetch result)
            while row
            collect (let* ((name (intern (snake->kebab (string-upcase (getf row :|name|))) :KEYWORD))
                          (access (intern (string-downcase (getf row :|name|)) :KEYWORD))
                          ; ex: varcahr(255) -> varchar
                          (column-type (car (str:split "(" (string-downcase (getf row :|type|)))))
                          (inv (cdr (assoc column-type *sqlite3-type-convert-functions* :test #'string=)))
                          (type (if inv (getf inv :type)
                                        *sqlite3-type-convert-unknown-type*))
                          (db-cl-fn (if inv (getf inv :db-cl-fn)
                                            *sqlite3-type-convert-unknown-function*))
                          (cl-db-fn (if inv (getf inv :cl-db-fn)
                                            *sqlite3-type-convert-unknown-function*)))
                      (list :name name
                            :access access
                            :type type
                            :db-cl-fn db-cl-fn
                            :cl-db-fn cl-db-fn))))))

(defmethod fetch-columns-and-types-plist-impl ((database-type <database-type-sqlite3>) connection table)
  (declare (ignore database-type))
  (let ((sql (format NIL "pragma table_info('~A')" table)))
    (log.sql sql :table table)
    (let* ((query (dbi:prepare connection sql))
          (result (dbi:execute query '())))
      (loop for row = (dbi:fetch result)
            while row
            append (let* ((name (intern (snake->kebab (string-upcase (getf row :|name|))) :KEYWORD))
                          (access (intern (string-downcase (getf row :|name|)) :KEYWORD))
                          ; ex: varcahr(255) -> varchar
                          (column-type (car (str:split "(" (string-downcase (getf row :|type|)))))
                          (inv (cdr (assoc column-type *sqlite3-type-convert-functions* :test #'string=)))
                          (type (if inv (getf inv :type)
                                    *sqlite3-type-convert-unknown-type*))
                          (db-cl-fn (if inv (getf inv :db-cl-fn)
                                        *sqlite3-type-convert-unknown-function*))
                          (cl-db-fn (if inv (getf inv :cl-db-fn)
                                        *sqlite3-type-convert-unknown-function*)))
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
  "Parse a column definition for SQLite3 CREATE/ALTER TABLE statement.

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
         (precision (when (or (eq type :decimal)
                              (eq type :float))
                      (getf attr :precision)))
         (scale (when (or (eq type :decimal)
                           (eq type :float))
                  (getf attr :scale))))
    (check-type-valid type)
    (create-column column-name type precision scale not-null-p primary-key-p auto-increment-p default-value)))


(defun create-column (column-name type precision scale not-null-p primary-key-p auto-increment-p default-value)
  (format NIL " ~A ~A~:[~2*~;(~A,~A)~]~@[ NOT NULL~*~]~@[ PRIMARY KEY~*~]~@[ AUTOINCREMENT~*~]~@[ DEFAULT ~A~]"
              column-name
              (type-convert type)
              (and precision scale)
              precision
              scale
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
                  created_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
              "))

(defmethod ensure-database-impl ((database-type <database-type-sqlite3>) connection)
  (declare (ignore database-type connection))
  'NOP)

(defmethod ensure-migration-table-impl ((database-type <database-type-sqlite3>) connection)
  (declare (ignore database-type))
  (dbi:do-sql connection CREATE-MIGRATION-TABLE))


(defmethod get-last-id-impl ((database-type <database-type-sqlite3>) connection)
  (declare (ignore database-type))

  (let ((result (dbi-cp:execute
                 (dbi-cp:prepare connection "select last_insert_rowid()")
                 '())))
    (getf (dbi-cp:fetch result) :|last_insert_rowid()|)))
