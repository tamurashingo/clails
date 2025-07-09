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
                #:fetch-columns-and-types-impl)
  (:import-from #:clails/model/connection
                #:get-connection-direct-impl
                #:create-connection-pool-impl)
  (:import-from #:clails/model/query
                #:get-last-id-impl)
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
                   :cl-db-fn ,#'identity))
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
                  :db-cl-fn ,#'identity
                  :cl-db-fn ,#'identity))))

(defparameter *sqlite3-type-convert-unknown-type* :string)
(defparameter *sqlite3-type-convert-unknown-function* #'identity)


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

(defmethod fetch-columns-and-types-impl ((database-type <database-type-sqlite3>) connection table)
  (declare (ignore database-type))
  (let* ((query (dbi:prepare connection (format NIL "pragma table_info('~A')" table)))
         (result (dbi:execute query '())))
    (loop for row = (dbi:fetch result)
          while row
;;          collect (intern (snake->kebab (string-upcase (getf row :|name|))) :KEYWORD))))
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
                          :cl-db-fn cl-db-fn)))))

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
         (default-value (getf attr :default-value))
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
                  created_at dateteime NOT NULL DEFAULT CURRENT_TIMESTAMP)~
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
