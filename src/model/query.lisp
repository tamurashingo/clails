(in-package #:cl-user)
(defpackage #:clails/model/query
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:ensure-list
                #:flatten)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:quote-meta-chars)
  (:import-from #:clails/condition
                #:optimistic-lock-error)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:validate
                #:ref
                #:has-error-p
                #:has-dirty-p
                #:frozen-p
                #:clear-error
                #:clear-dirty-flag)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab
                #:plist-exists)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:import-from #:cl-batis
                #:<batis-sql>
                #:gen-sql-and-params)
  (:import-from #:dbi-cp
                #:fetch-all
                #:execute
                #:prepare)
  (:export #:<query>
           #:query
           #:execute-query
           #:save
           #:get-last-id-impl
           #:make-record
           #:destroy))
(in-package #:clails/model/query)

;;;; ----------------------------------------
;;;; class

(defclass <query> ()
  ((model :initarg :model
          :documentation "Model class symbol")
   (alias :initarg :alias
          :initform nil
          :documentation "Alias name for the model in query")
   (columns :initarg :columns
            :initform nil
            :documentation "List of columns to select")
   (joins :initarg :joins
          :initform nil
          :documentation "List of join specifications")
   (where :initarg :where
          :initform nil
          :documentation "WHERE clause specification")
   (order-by :initarg :order-by
             :initform nil
             :documentation "ORDER BY clause specification")
   (limit :initarg :limit
          :initform nil
          :documentation "LIMIT value")
   (offset :initarg :offset
           :initform nil
           :documentation "OFFSET value")
   (inst :type 'clails/model/base-model::<base-model>
         :documentation "Model instance")
   (alias->model :initform (make-hash-table)
                 :documentation "Hash table mapping alias names to model symbols"))
  (:documentation "Query builder class for constructing and executing database queries."))

(defclass <join-query> ()
  ((join-type :initarg :join-type
              :reader join-type
              :documentation "Type of join (:inner-join, :left-join, etc.)")
   (relation :initarg :relation
             :reader relation
             :documentation "Relation alias to join")
   (through :initarg :through
            :initform nil
            :reader through
            :documentation "Intermediate relation for nested joins")
   (previous :initform nil
             :accessor previous-join
             :documentation "Previous join in the chain"))
  (:documentation "Join specification for query builder."))


;;;; ========================================
;;;; export method

(defmethod execute-query ((query <query>) named-values &key connection)
  "Execute the query and return model instances.
   
   Generates SQL from the query specification, executes it against the database,
   and builds model instances from the results, including nested relations.
   
   @param query [<query>] Query specification
   @param named-values [plist] Named parameter values for the query
   @param connection [dbi:<dbi-connection>] Optional database connection to use
   @return [list] List of model instances with populated relations
   "
  (multiple-value-bind (sql params)
      (generate-query query named-values)
    (when (log-level-enabled-p :sql :debug)
      (log.sql (format nil "sql: ~S" query))
      (log.sql (format nil "params: ~S" params)))
    (let* ((connection (get-connection))
           (result (dbi-cp:fetch-all
                    (dbi-cp:execute
                     (dbi-cp:prepare connection sql)
                     params))))
      (build-model-instances query result))))


(defmethod save ((inst <base-model>) &key connection)
  "Save model instance to the database.
   
   Validates the instance, then either updates (if ID exists and dirty)
   or inserts (if no ID). Clears dirty flags on success.
   
   @param inst [<base-model>] Model instance to save
   @param connection [dbi:<dbi-connection>] Optional database connection to use
   @return [<base-model>] The saved model instance
   @return [nil] NIL if validation failed or instance is frozen
   @condition optimistic-lock-error Signaled when update affects 0 rows (version conflict)
   "
  (unless (frozen-p inst)
    (clear-error inst)
    (validate inst)
    (unless (has-error-p inst)
      (prog1
          (if (ref inst :id)
              (if (has-dirty-p inst)
                  (let ((rows-updated (update1 inst :connection connection)))
                    (when (= rows-updated 0)
                      (error 'optimistic-lock-error))
                    inst)
                  inst)
              (insert1 inst :connection connection))
        (clear-dirty-flag inst)))))


(defgeneric get-last-id-impl (database-type connection)
  (:documentation "Get the last inserted ID for the specified database type.
   
   Implementation must be provided for each database type.
   
   @param database-type [<database-type>] Database type instance
   @param connection [dbi:<dbi-connection>] Database connection
   @return [integer] Last inserted ID
   "))


(defun make-record (model-name &rest values)
  "Create a new model instance with the given attribute values.
   
   @param model-name [symbol] Model class name (e.g., <todo>)
   @param values [plist] Property list of attribute key-value pairs
   @return [<base-model>] New model instance
   
   Example:
   (let ((inst (make-record '<todo> :title \"create new project\" :done nil)))
     (save inst))
   "
  (let ((inst (make-instance model-name)))
    (loop for (key value) on values by #'cddr
          do (setf (ref inst key) value))
    inst))


(defgeneric destroy (instance &key cascade)
  (:documentation "Delete record from the database.
   
   @param instance [<base-model>] Model instance to delete
   @param cascade [boolean] Whether to cascade delete to related records
   @return [integer] Number of rows deleted
   "))

(defmethod destroy ((inst <base-model>) &key cascade)
  "Delete a single model instance from the database.
   
   If cascade is true, also deletes related :has-many records.
   Sets the instance to frozen after deletion.
   
   @param inst [<base-model>] Model instance to delete
   @param cascade [boolean] If T, cascade delete to :has-many relations
   @return [integer] Number of rows deleted (0 if frozen, 1 otherwise)
   "
  (if (frozen-p inst)
      0
      (progn
        (when cascade
          (let ((relations (getf (gethash (class-name (class-of inst)) clails/model/base-model::*table-information*)
                                 :relations)))
            (maphash #'(lambda (k v)
                         ;; only :has-many relation is supported for cascade delete
                         (when (eq (getf v :type) :has-many)
                           (destroy (ref inst k) :cascade T)))
                     relations)))
        (let* ((table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
               (sql (format NIL "DELETE FROM ~A WHERE id = ?" table-name))
               (params (list (ref inst :id))))
          (when (log-level-enabled-p :sql :debug)
            (log.sql (format nil "sql: ~S" sql))
            (log.sql (format nil "params: ~S" params)))
          (let ((connection (get-connection)))
            (prog1
              (progn
                (dbi-cp:execute
                 (dbi-cp:prepare connection sql)
                 params)
                (dbi-cp:row-count connection))
              (clear-dirty-flag inst)
              (setf (slot-value inst 'clails/model/base-model::frozen-p) T)))))))

(defmethod destroy ((insts list) &key cascade)
  "Delete multiple model instances from the database.
   
   If cascade is true, also deletes related :has-many records for each instance.
   Sets all instances to frozen after deletion.
   
   @param insts [list] List of <base-model> instances to delete
   @param cascade [boolean] If T, cascade delete to :has-many relations
   @return [integer] Number of rows deleted
   "
  (if (null insts)
      0
      (progn
        ;; accept only <base-model> instances
        (dolist (i insts)
          (check-type i <base-model>))

        (when cascade
          (dolist (i insts)
            (let ((relations (getf (gethash (class-name (class-of i)) clails/model/base-model::*table-information*)
                                   :relations)))
              (maphash #'(lambda (k v)
                           ;; only :has-many relation is supported for cascade delete
                           (when (eq (getf v :type) :has-many)
                             (destroy (ref i k) :cascade T)))
                       relations))))


        (let* ((table-name (kebab->snake (slot-value (first insts) 'clails/model/base-model::table-name)))
               (ids (loop for i in insts
                      when (not (frozen-p i))
                        collect (ref i :id)))
               (sql (format NIL "DELETE FROM ~A WHERE id IN (~{?~*~^, ~})" table-name ids)))
          (when (log-level-enabled-p :sql :debug)
            (log.sql (format nil "sql: ~S" sql))
            (log.sql (format nil "ids: ~S" ids)))
          (prog1
              (let ((connection (get-connection)))
                (dbi-cp:execute
                  (dbi-cp:prepare connection sql)
                  ids)
                (dbi-cp:row-count connection))
            (dolist (i insts)
              (unless (frozen-p i)
                (clear-dirty-flag i)
                (setf (slot-value i 'clails/model/base-model::frozen-p) T))))))))

;;;; ========================================
;;;; export macro

(defmacro query (model &key as columns joins where order-by limit offset)
  "Create a query builder instance for the specified model.
   
   Provides a DSL for constructing SQL queries with support for joins,
   where clauses, ordering, and pagination.
   
   @param model [symbol] Model class name (e.g., <blog>)
   @param as [keyword] Required alias for the model (e.g., :blog)
   @param columns [list] List of column specifications (e.g., ((blog :id :title) (user :name)))
   @param joins [list] List of join specifications (e.g., ((:inner-join :user) (:left-join :comments)))
   @param where [form] WHERE clause expression
   @param order-by [list] ORDER BY specifications
   @param limit [integer] LIMIT value
   @param offset [integer] OFFSET value
   @return [<query>] Query builder instance
   @condition error Signaled when :as is missing or not a keyword
   
   Example:
   (query <blog>
     :as :blog
     :columns ((blog :id :title))
     :joins ((:inner-join :account))
     :where (:> (:blog :star) 0))
   "
  (unless as
    (error ":as keyword is required for query macro."))
  (unless (keywordp as)
    (error "The value for :as must be a keyword (e.g., :blog)."))

  (labels ((expand-columns (grouped-columns)
             ;; ((blog :id :title :content)
             ;;  (use :id))
             ;; => ((blog :id) (blog :title) (blog :content) (user :name))
             (mapcan #'(lambda (group)
                         (let ((alias (car group))
                               (column-names (cdr group)))
                           (mapcar #'(lambda (column-name)
                                       (list alias column-name))
                                   column-names)))
                     grouped-columns))

           (expand-joins (joins-list)
             (mapcar #'(lambda (join-spec)
                         (destructuring-bind (join-type relation &key through) join-spec
                           `(make-instance '<join-query>
                                           :join-type ',join-type
                                           :relation ',relation
                                           :through ',through)))
                     joins-list)))
    `(make-instance '<query>
                    :model ',model
                    :alias ',as
                    :columns ',(expand-columns columns)
                    :joins (list ,@(expand-joins joins))
                    :where ',where
                    :order-by ',order-by
                    :limit ',limit
                    :offset ',offset)))


;;;; ========================================
;;;; internals

;;; ----------------------------------------
;;; instance

(defmethod initialize-instance :after ((q <query>) &rest initargs)
  "Initialize query instance after creation.
   
   Sets up base model instance, assigns default alias if not provided,
   and builds the alias-to-model mapping.
   
   @param q [<query>] Query instance being initialized
   @param initargs [list] Initialization arguments (ignored)
   "
  (declare (ignore initargs))
  ;; Set up base instance and alias
  (let ((inst (make-instance (slot-value q 'model))))
    (setf (slot-value q 'inst) inst)
    (unless (slot-value q 'alias)
      (setf (slot-value q 'alias)
            (intern (symbol-name (slot-value q 'model)) :KEYWORD))))

  ;; Link up :through joins
  (let ((joins (slot-value q 'joins))
        (join-objects (make-hash-table)))
    (loop for j in joins do (setf (gethash (relation j) join-objects) j)))

  ;; build alias->model maphash
  (build-alias-map q))


(defmethod build-alias-map ((query <query>))
  "Build mapping from aliases to model classes.
   
   Resolves model classes for all aliases (base and joined) by looking up
   relation information in the model metadata.
   
   @param query [<query>] Query instance
   @condition error Signaled when source alias or relation cannot be resolved
   "
  (let* ((alias->model (slot-value query 'alias->model))
         (base-alias (slot-value query 'alias))
         (base-model (slot-value query 'model)))

    (setf (gethash base-alias alias->model) base-model)

    (loop for join-obj in (slot-value query 'joins)
          do (let* ((target-alias (relation join-obj))
                    (source-alias (or (through join-obj) base-alias))
                    (source-model (gethash source-alias alias->model)))
               (unless source-model
                 (error "Could not resolve join source alias: ~A. Ensure joins are ordered correctly." source-alias))
               (let* ((relations (getf (gethash source-model clails/model/base-model::*table-information*) :relations))
                      (rel-info (gethash target-alias relations)))
                 (unless rel-info
                   (error "Relation `~A' not found for model `~A'" target-alias source-model))
                 (let ((target-model (getf rel-info :model)))
                   (setf (gethash target-alias alias->model) target-model)))))))

;;; ----------------------------------------
;;; query

(defmethod generate-query ((query <query>) &optional named-values)
  "Generate SQL query and parameters from query specification.
   
   Constructs SELECT statement with joins, where clause, order by, limit, and offset.
   Handles dynamic IN clause expansion for parameterized queries.
   
   @param query [<query>] Query specification
   @param named-values [plist] Named parameter values
   @return [string] SQL query string
   @return [list] List of parameter values
   "
  (let* ((alias->model (slot-value query 'alias->model))
         (base-alias (slot-value query 'alias))
         (base-model (slot-value query 'model))
         (base-table-name (getf (gethash base-model clails/model/base-model::*table-information*) :table-name)))

    (let* ((joins (resolve-joins query))
           (columns (mapcar #'(lambda (c) (column-pair-to-name c T))
                            (generate-query-columns query alias->model)))
           (where-parts (multiple-value-list (parse-where-claude (slot-value query 'where))))
           (order-by (generate-order-by (slot-value query 'order-by)))
           (offset (generate-offset (slot-value query 'offset)))
           (limit (generate-limit (slot-value query 'limit)))
           (sql-template (format nil "SELECT ~{~A~^, ~} FROM ~A as ~A~@[ ~A~]~@[ WHERE ~A~]~@[ ORDER BY ~{~A~^, ~}~]~@[ LIMIT ~A~]~@[ OFFSET ~A~]"
                                 columns
                                 base-table-name
                                 (kebab->snake base-alias)
                                 (format nil "~{~A~^ ~}" joins)
                                 (first where-parts)
                                 order-by
                                 (getf limit :limit)
                                 (getf offset :offset)))
           (named-params (append (second where-parts)
                                          (ensure-list (getf limit :keyword))
                                          (ensure-list (getf offset :keyword)))))

      (let ((final-sql sql-template)
            (final-params '())
            (regular-named-params '()))

        (loop for param in named-params
              do (if (and (listp param) (eq (car param) :in-expansion))
                     (destructuring-bind (op column-sql keyword) (cdr param)
                       (let* ((values (getf named-values keyword))
                              (placeholder (format nil "__IN_CLAUSE_~A_~A__"
                                                   (cl-ppcre:regex-replace-all "[.:]" column-sql "_")
                                                   keyword)))

                         (if (null values)
                             (let ((replacement (if (string= op "IN") "1=0" "1=1")))
                               (setf final-sql (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars placeholder) final-sql replacement)))
                             (let* ((question-marks (format nil "(~{?~*~^, ~})" values))
                                    (replacement (format nil "~A ~A ~A" column-sql op question-marks)))
                               (setf final-sql (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars placeholder) final-sql replacement))
                               (appendf final-params values)))))
                     (push param regular-named-params)))

        (appendf final-params (generate-values (nreverse regular-named-params) named-values))

        ;; for debug
        (when (log-level-enabled-p :sql :debug)
          (log.sql (format nil "sql: ~S" sql))
          (log.sql (format nil "params: ~S" params)))

        (values final-sql final-params)))))


(defmethod resolve-joins ((query <query>))
  "Generate JOIN clauses for the query.
   
   @param query [<query>] Query instance
   @return [list] List of JOIN SQL strings
   "
  (let ((base-model-alias (slot-value query 'alias))
        (alias->model (slot-value query 'alias->model)))
    (loop for join-obj in (slot-value query 'joins)
          with joins-sql and keywords
          do (let ((sql (generate-join-sql join-obj base-model-alias alias->model)))
               (push sql joins-sql))
          finally (return (nreverse joins-sql)))))


(defmethod generate-query-columns ((query <query>) alias->model)
  "Generate list of columns to select.
   
   If columns are explicitly specified, returns them. Otherwise, returns
   all columns from all models in the query.
   
   @param query [<query>] Query instance
   @param alias->model [hash-table] Mapping from aliases to model classes
   @return [list] List of column pairs (alias column-name)
   "
  (if (slot-value query 'columns)
      (slot-value query 'columns)
      (loop for alias being the hash-key of (slot-value query 'alias->model)
            using (hash-value model)
            append (let ((inst (make-instance model)))
                     (loop for column in (slot-value inst 'clails/model/base-model::columns)
                           collect (list alias (getf column :name)))))))


(defmethod generate-join-sql ((join-obj <join-query>) base-model-alias alias->model)
  "Generate SQL JOIN clause for a single join specification.
   
   Constructs INNER JOIN or LEFT JOIN with ON clause based on relation metadata.
   
   @param join-obj [<join-query>] Join specification
   @param base-model-alias [keyword] Base model alias
   @param alias->model [hash-table] Mapping from aliases to model classes
   @return [string] JOIN SQL string
   @condition error Signaled when source model or relation cannot be resolved
   "
  (let* ((through-relation (through join-obj))
         (source-alias (if through-relation
                           through-relation
                           base-model-alias))
         (source-model (gethash source-alias alias->model)))
    (unless source-model
      (error "Could not determine source model for join: ~A" (relation join-obj)))
    (let* ((relations (getf (gethash source-model clails/model/base-model::*table-information*) :relations)))
      (unless relations
        (error "Table information not found for model: ~A" source-model))
      (let* ((rel-info (gethash (relation join-obj) relations)))
        (unless rel-info
          (error "Relation `~A` not found for model `~A`" (relation join-obj) source-model))
        (let* ((target-model (getf rel-info :model))
               (target-alias (relation join-obj))
               (target-table-name (getf (gethash target-model clails/model/base-model::*table-information*) :table-name))
               (on-clause (case (getf rel-info :type)
                            (:belongs-to
                             (format nil "~A.~A = ~A.id"
                                     (kebab->snake source-alias)
                                     (kebab->snake (getf rel-info :key))
                                     (kebab->snake target-alias)))
                            (:has-many
                             (format nil "~A.id = ~A.~A"
                                     (kebab->snake source-alias)
                                     (kebab->snake target-alias)
                                     (kebab->snake (getf rel-info :foreign-key)))))))
          (format nil "~A ~A as ~A ON ~A"
                   (case (join-type join-obj)
                     (:inner-join "INNER JOIN")
                     (:left-join "LEFT JOIN"))
                   target-table-name
                   (kebab->snake target-alias)
                   on-clause))))))


(defun fetch-columns (inst &key insert update)
  "Fetch column names to be included in SQL statement.

   For INSERT operations, only returns columns that have been explicitly set
   (marked with dirty-flag). For UPDATE operations, returns columns that have
   been modified plus :updated-at.

   @param inst [<base-model>] Model instance
   @param insert [boolean] If true, fetches columns for INSERT statement
   @param update [boolean] If true, fetches columns for UPDATE statement
   @return [list] List of column name strings
   "
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        as dirty-flag-hash = (slot-value inst 'clails/model/base-model::dirty-flag)
        when (or (and insert
                      (not (eq (getf column :name) :id))
                      (gethash (getf column :name) dirty-flag-hash))
                 ;; update column if dirty
                 (and update
                      (not (eq (getf column :name) :id))
                      (not (eq (getf column :name) :created-at))
                      (or (eq (getf column :name) :updated-at)
                          (gethash (getf column :name) dirty-flag-hash)))
                 (and (not insert)
                      (not update)))
        collect (string (getf column :name))))


(defun parse-where-claude (where)
  "Parse WHERE clause expression tree into SQL.
   
   Supports operators: :=, :<, :<=, :>, :>=, :<>, :!=, :like, :not-like,
   :between, :not-between, :in, :not-in, :null, :not-null, :and, :or.
   
   @param where [list] WHERE clause expression (nil for no where clause)
   @return [string] SQL WHERE clause string
   @return [list] List of parameter keywords
   @condition error Signaled for unsupported operators
   "
  (if (not where)
      nil
      (let ((elm (car where)))
        (cond ((find elm '(:= :< :<= :> :>= :<> :!=))
               (parse-exp2 elm (cdr where)))
              ((eq elm :like)
               (parse-exp2 "LIKE" (cdr where)))
              ((eq elm :not-like)
               (parse-exp2 "NOT LIKE" (cdr where)))
              ((find elm '(:between :not-between))
               (parse-between-clause (if (eq elm :between) "BETWEEN" "NOT BETWEEN") (cdr where)))
              ((find elm '(:in :not-in))
               (let ((op (if (eq elm :in) "IN" "NOT IN"))
                     (args (cdr where)))
                 (if (keywordp (second args))
                     (parse-in-dynamic op args)
                     (parse-in-clause op args))))
              ((eq elm :null)
               (parse-null2 :null (cdr where)))
              ((eq elm :not-null)
               (parse-null2 :not-null (cdr where)))
              ((eq elm :and)
               (loop for expression in (cdr where)
                     with exp and keywords
                     do (multiple-value-setq (exp keywords) (parse-where-claude expression))
                     collect exp into exp-list
                     append keywords into all-keywords
                     finally (return (values (format nil "(~{~A~^ AND ~})" exp-list)
                                             all-keywords))))
              ((eq elm :or)
               (loop for expression in (cdr where)
                     with exp and keywords
                     do (multiple-value-setq (exp keywords) (parse-where-claude expression))
                     collect exp into exp-list
                     append keywords into all-keywords
                     finally (return (values (format nil "(~{~A~^ OR ~})" exp-list)
                                             all-keywords))))
              (t
               (error "where claude: parse error `~A`" elm))))))

(defun parse-in-clause (op exp)
  "Parse IN clause with static list of values.
   
   @param op [string] Operator (\"IN\" or \"NOT IN\")
   @param exp [list] Expression (column-spec values-list)
   @return [string] SQL clause string
   @return [list] List of parameters
   @condition error Signaled when values are not a list
   "
  (let (column-sql)
    (multiple-value-bind (sql param)
        (lexer2 (car exp))
      (setf column-sql sql)
      (when param
        (error "Unexpected parameter in column part of IN clause.")))
    (let ((in-values (cadr exp)))
      (when (and (listp in-values) (eq (car in-values) 'quote))
        (setf in-values (cadr in-values)))
      (unless (listp in-values)
        (error "IN clause expects a list of values (e.g., '(1 2 3) or (:id1 :id2)). Got: ~S" in-values))
      (let (placeholders params)
        (dolist (val in-values)
          (multiple-value-bind (ph p) (lexer2 val)
            (push ph placeholders)
            (when p (appendf params (ensure-list p)))))
        (if (null placeholders)
            (if (string= op "IN")
                (values "1=0" nil)
                (values "1=1" nil))
            (values (format nil "~A ~A (~{~A~^, ~})" column-sql op (nreverse placeholders))
                    (flatten params)))))))

(defun parse-in-dynamic (op exp)
  "Parse IN clause with dynamic parameterized values.
   
   Creates placeholder for later expansion with actual parameter values.
   
   @param op [string] Operator (\"IN\" or \"NOT IN\")
   @param exp [list] Expression (column-spec param-keyword)
   @return [string] Placeholder string for later replacement
   @return [list] List containing :in-expansion spec
   "
  (let* ((column-spec (first exp))
         (param-keyword (second exp))
         (column-sql (column-pair-to-name column-spec))
         (placeholder (format nil "__IN_CLAUSE_~A_~A__"
                              (cl-ppcre:regex-replace-all "[.:]" column-sql "_")
                              param-keyword)))
    (values placeholder
            (list (list :in-expansion op column-sql param-keyword)))))

(defun parse-between-clause (op exp)
  "Parse BETWEEN clause.
   
   @param op [string] Operator (\"BETWEEN\" or \"NOT BETWEEN\")
   @param exp [list] Expression (column-spec value1 value2)
   @return [string] SQL BETWEEN clause
   @return [list] List of parameter keywords
   @condition error Signaled when column part contains parameters
   "
  (let (column-sql val1-sql val2-sql keywords)
    ;; Column
    (multiple-value-bind (sql param) (lexer2 (first exp))
      (setf column-sql sql)
      (when param (error "Unexpected parameter in column part of BETWEEN clause.")))
    ;; Value 1
    (multiple-value-bind (sql param) (lexer2 (second exp))
      (setf val1-sql sql)
      (when param (appendf keywords (ensure-list param))))
    ;; Value 2
    (multiple-value-bind (sql param) (lexer2 (third exp))
      (setf val2-sql sql)
      (when param (appendf keywords (ensure-list param))))
    (values (format nil "~A ~A ~A AND ~A" column-sql op val1-sql val2-sql)
            keywords)))

;; TODO: rename
(defun parse-exp2 (op exp)
  "Parse binary comparison expression.
   
   @param op [keyword|string] Operator (:=, :<, :>, etc. or \"LIKE\", \"NOT LIKE\")
   @param exp [list] Expression (left-operand right-operand)
   @return [string] SQL comparison expression
   @return [list] List of parameter keywords
   "
  (let (x y keywords)
    (multiple-value-bind (col1 param1)
        (lexer2 (car exp))
      (setf x col1)
      (when param1
        (appendf keywords (ensure-list param1))))
    (multiple-value-bind (col2 param2)
        (lexer2 (cadr exp))
      (setf y col2)
      (when param2
        (appendf keywords (ensure-list param2))))
    (values(format nil "~A ~A ~A" x op y)
           keywords)))


;; TODO: rename
(defun parse-null2 (op exp)
  "Parse NULL check expression.
   
   @param op [keyword] Operator (:null or :not-null)
   @param exp [list] Expression (column-spec)
   @return [string] SQL NULL check (\"IS NULL\" or \"IS NOT NULL\")
   @return [list] List of parameter keywords
   "
  (let (x keywords)
    (multiple-value-bind (col1 param1)
        (lexer2 (car exp))
      (setf x col1)
      (when param1
        (appendf keywords (ensure-list param1))))
    (values (format nil "~A ~A" x (if (eq op :null)
                                      "IS NULL"
                                      "IS NOT NULL"))
            keywords)))


;; TODO: rename
(defun lexer2 (param)
  "Convert parameter to SQL representation.
   
   @param param [list] Column pair (alias column-name)
   @param param [keyword] Parameter keyword (becomes \"?\")
   @param param [string] String literal (quoted)
   @param param [t] Other literal value
   @return [string] SQL representation
   @return [keyword|nil] Parameter keyword if applicable
   "
  (cond (;; (table :id)
         (and (listp param)
              (= (length param) 2)
              (symbolp (car param))
              (keywordp (cadr param)))

         (values (column-pair-to-name param)
                 nil))
        (;; :keyword parameter
         (keywordp param)
         (values "?" param))
        (;; string literal
         (stringp param)
         (values (format nil "'~A'" param) nil))
        (;; other literal value
         t
         (values param nil))))


(defun column-pair-to-name (pair &optional as)
  "Convert column pair to SQL column name.
   
   @param pair [list] Column pair (alias column-name)
   @param as [boolean] If true, includes AS clause for aliasing
   @return [string] SQL column reference (e.g., \"BLOG.TITLE\" or \"BLOG.TITLE as \\\"BLOG.TITLE\\\"\")
   "
  (if as (format nil "~A.~A as \"~A.~A\"" (kebab->snake (car pair)) (kebab->snake (cadr pair)) (kebab->snake (car pair)) (kebab->snake (cadr pair)))
         (format nil "~A.~A" (kebab->snake (car pair)) (kebab->snake (cadr pair)))))


(defun generate-order-by (params &optional order)
  "Generate ORDER BY clause from parameters.
   
   @param params [list] List of order specs ((:alias :column [:ASC|:DESC]) ...)
   @param order [list] Accumulator for recursion (internal use)
   @return [list] List of ORDER BY clause strings
   @condition error Signaled for invalid order spec format or sort direction
   
   Example input: ((:blog :star :desc) (:blog :id))
   Example output: (\"BLOG.STAR DESC\" \"BLOG.ID\")
   "
  (flet ((generate (p)
           (when (not (and (listp p) (>= (length p) 2) (keywordp (first p)) (keywordp (second p))))
             (error "parse error: order-by: expect (:keyword :keyword) but got ~S" p))
           (let ((sort-order (third p)))
             (when (not (or (null sort-order)
                            (eq sort-order :ASC)
                            (eq sort-order :DESC)))
               (error "parse error: order-by: expect :ASC or :DESC but got ~S" sort-order))
             (format nil "~A~@[ ~A~]" (column-pair-to-name p)
                     sort-order))))
    (if (null params)
        (nreverse order)
        (let ((p (car params)))
          (generate-order-by (cdr params)
                             (push (generate p)
                                   order))))))


;; TODO: In the future, refactor this to use OFFSET/FETCH depending on the database implementation.
(defun generate-offset (offset)
  "Generate OFFSET clause specification.
   
   @param offset [nil] No offset
   @param offset [keyword] Parameter keyword for offset value
   @param offset [integer] Literal offset value
   @return [plist] Property list with :offset and :keyword
   "
  (cond ((null offset)
         (list :offset nil
               :keyword nil))
        ((keywordp offset)
         (list :offset "?"
               :keyword offset))
        (t
         (list :offset offset
               :keyword nil))))

(defun generate-limit (limit)
  "Generate LIMIT clause specification.
   
   @param limit [nil] No limit
   @param limit [keyword] Parameter keyword for limit value
   @param limit [integer] Literal limit value
   @return [plist] Property list with :limit and :keyword
   "
  (cond ((null limit)
         (list :limit nil
               :keyword nil))
        ((keywordp limit)
         (list :limit "?"
               :keyword limit))
        (t
         (list :limit limit
               :keyword nil))))


(defun generate-values (named-params named-values)
  "Extract parameter values in order from named values plist.
   
   @param named-params [list] List of parameter keywords in order (e.g., (:start-date :end-date :start-date))
   @param named-values [plist] Property list of parameter values (e.g., (:start-date \"2025-01-01\" :end-date \"2025-12-31\"))
   @return [list] List of values in parameter order (e.g., (\"2025-01-01\" \"2025-12-31\" \"2025-01-01\"))
   "
  (loop for key in named-params
        collect (getf named-values key)))


;;; ----------------------------------------
;;; save

(defun insert1 (inst &key connection)
  "Insert a model instance into the database.

   Only inserts columns that have been explicitly set (marked with dirty-flag),
   allowing database default values to be applied to unset columns.
   Automatically sets created-at and updated-at timestamps.

   @param inst [<base-model>] Model instance to insert
   @param connection [connection] Optional database connection (uses connection pool if not provided)
   @return [<base-model>] The inserted instance with id, created-at, updated-at, and version set
   "
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name clails/model/base-model::*table-information*))
         (version-column (getf table-info :version-column))
         (current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (fetch-columns inst :insert T))
         (params nil))

    ;; Build params from columns that have dirty flag set
    (loop for colstr in columns
          as colkey = (intern colstr :KEYWORD)
          do (push (ref inst colkey) params))
    (setf params (nreverse params))

    ;; Add created-at and updated-at to columns and params
    (setf columns (append columns (list "CREATED-AT" "UPDATED-AT")))
    (setf params (append params (list current-datetime current-datetime)))

    ;; Add version column if specified
    (when version-column
      (setf columns (append columns (list (string version-column))))
      (setf params (append params (list 1))))

    ;; Build SQL with all columns including created-at and updated-at
    (let ((sql (format NIL "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{?~*~^, ~})"
                       table-name
                       (mapcar #'kebab->snake columns)
                       columns)))

      ;; convert parameter values using cl-db-fn
      (setf params (loop for colstr in columns
                         as colkey = (intern colstr :KEYWORD)
                         as column-info = (loop for col in (slot-value inst 'clails/model/base-model::columns)
                                                when (eq (getf col :name) colkey)
                                                return col)
                         for i from 0
                         as value = (nth i params)
                         collect (if column-info
                                     (funcall (getf column-info :cl-db-fn) value)
                                     value)))

      (when (log-level-enabled-p :sql :debug)
        (log.sql (format nil "sql: ~S" sql))
        (log.sql (format nil "params: ~S" params)))

      (let ((body #'(lambda (connection)
                      (dbi-cp:execute
                       (dbi-cp:prepare connection sql)
                       params)
                      (let ((last-id (get-last-id connection)))
                        (setf (ref inst :id) last-id)
                        (setf (ref inst :created-at) current-datetime)
                        (setf (ref inst :updated-at) current-datetime)
                        (when version-column
                          (setf (ref inst version-column) 1)))
                      inst)))

        (if connection
            (funcall body connection)
            (let ((connection (get-connection)))
              (funcall body connection)))))))


(defun get-last-id (connection)
  "Get the last inserted ID from the database.
   
   @param connection [dbi:<dbi-connection>] Database connection
   @return [integer] Last inserted ID
   "
  (get-last-id-impl *database-type* connection))


(defun update1 (inst &key connection)
  "Update a model instance in the database.
   
   Only updates columns marked as dirty. Automatically updates :updated-at
   and version column (if configured). Uses optimistic locking when version
   column is specified.
   
   @param inst [<base-model>] Model instance to update
   @param connection [connection] Optional database connection
   @return [integer] Number of rows updated (0 if version mismatch, 1 otherwise)
   "
  (let* ((class-name (class-name (class-of inst)))
         (table-info (gethash class-name clails/model/base-model::*table-information*))
         (version-column (getf table-info :version-column))
         (current-version (when version-column (ref inst version-column)))
         (current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns nil)
         (where-clause "id = ?")
         (where-params (list (ref inst :id)))
         (params nil)
         (sql nil))

    (when version-column
      ;; the version-column will be automatically added to the `update` columns when flag sets to dirty
      (setf (gethash version-column (slot-value inst 'clails/model/base-model::dirty-flag))
            T)
      (setf where-clause (format nil "~A AND ~A = ?" where-clause (kebab->snake version-column)))
      (appendf where-params (list current-version)))
    (setf columns (fetch-columns inst :update T))

    (setf params (alexandria:alist-plist (loop for colstr in columns
                                               as colkey = (intern colstr :KEYWORD)
                                               collect (cons colkey (ref inst colkey)))))

    ;; set updated-at and version
    (setf (getf params :updated-at) current-datetime)
    (when version-column
      (setf (getf params version-column) (1+ current-version)))

    (setf sql (format NIL "UPDATE ~A SET ~{~A = ?~^, ~} WHERE ~A"
                      table-name
                      (mapcar #'kebab->snake columns)
                      where-clause))

    ;; convert parameter plist -> values
    (setf params (convert-cl-db-values params inst))

    ;; append where-params
    (setf params (append params where-params))

    (when (log-level-enabled-p :sql :debug)
      (log.sql (format nil "sql: ~S" sql))
      (log.sql (format nil "params: ~S" params)))

    (let ((body #'(lambda (connection)
                    (dbi-cp:execute (dbi-cp:prepare connection sql) params)
                    (let ((rows (dbi-cp:row-count connection)))
                      (when (> rows 0)
                        (setf (ref inst :updated-at) current-datetime)
                        (when version-column
                          (setf (ref inst version-column) (1+ current-version))))
                      rows))))
      (if connection
          (funcall body connection)
          (let ((connection (get-connection)))
            (funcall body connection))))))


(defun convert-cl-db-values (params inst)
  "Convert Common Lisp values to database values.
   
   Applies cl-db-fn conversion function for each column.
   
   @param params [plist] Parameter values by column name
   @param inst [<base-model>] Model instance
   @return [list] List of converted values
   "
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        when (plist-exists params (getf column :name))
        collect (let ((name (getf column :name))
                      (fn (getf column :cl-db-fn)))
                  (funcall fn (getf params name)))))


;;; ----------------------------------------
;;; query -> model

(defun make-record-from (model-name &rest db-values)
  "Create model instance from database row values.
   
   Applies db-cl-fn conversion for each column and clears dirty flags.
   
   @param model-name [symbol] Model class name
   @param db-values [plist] Database values by column keyword
   @return [<base-model>] Model instance with values set and dirty flags cleared
   "
  (let ((inst (make-instance model-name))
        (columns-plist (clails/model/base-model::get-columns-plist model-name)))
    (loop for (key db-value) on db-values by #'cddr
          do (let ((fn (getf (getf columns-plist key) :DB-CL-FN)))
               (setf (ref inst key)
                     (funcall fn db-value))))
    (clear-dirty-flag inst)
    inst))


(defun split-db-column-name (keyword-name)
  "Split database column keyword into alias and column name.
   
   Splits a keyword like :|TABLE.COLUMN| into two keywords, :TABLE and :COLUMN.
   The keyword comes from the database driver.
   
   @param keyword-name [keyword] Database column keyword in format :|ALIAS.COLUMN|
   @return [keyword] Table alias keyword
   @return [keyword] Column name keyword
   @condition error Signaled when keyword is not in expected format
   "
  (let* ((str (string keyword-name))
         (dot-pos (position #\. str)))
    (if dot-pos
        (values (intern (snake->kebab (string-upcase (subseq str 0 dot-pos))) :keyword)
                (intern (snake->kebab (string-upcase (subseq str (1+ dot-pos)))) :keyword))
        (error "Invalid column name from DB: ~A. Expected 'ALIAS.COLUMN' format." keyword-name))))

(defun group-row-data-by-alias (row-plist)
  "Group flat database result row by table alias.
   
   Groups a flat plist of results from the DB into a hash table where keys are
   table aliases and values are plists of column data for that alias.
   
   @param row-plist [plist] Flat result row from database
   @return [hash-table] Hash table with aliases as keys and column data plists as values
   "
  (let ((grouped (make-hash-table)))
    ;; Group data into alists first to handle multiple columns for the same alias
    (loop for (key val) on row-plist by #'cddr
          do (multiple-value-bind (alias col) (split-db-column-name key)
               (push (cons col val) (gethash alias grouped))))
    ;; Convert the alists to plists for easier use with getf
    (maphash #'(lambda (alias alist)
                 (setf (gethash alias grouped) (alexandria:alist-plist (nreverse alist))))
             grouped)
    grouped))

(defun hydrate-instance (model-class data-plist record-cache)
  "Create or retrieve cached model instance from data.
   
   Creates a model instance from a plist of data, or retrieves it from cache if it
   has already been created for the same ID.
   
   @param model-class [symbol] Model class name
   @param data-plist [plist] Column data for the instance
   @param record-cache [hash-table] Cache of instances by model class and ID
   @return [<base-model>] Model instance
   @return [nil] NIL if data-plist has no :ID
   "
  (let ((id (getf data-plist :ID)))
    (when id
      (let* ((model-cache (or (gethash model-class record-cache)
                              (setf (gethash model-class record-cache) (make-hash-table :test #'eql))))
             (instance (gethash id model-cache)))
        (unless instance
          (setf instance (apply #'make-record-from model-class data-plist))
          (setf (gethash id model-cache) instance))
        instance))))

(defun hydrate-instances-for-row (grouped-data alias->model record-cache)
  "Create or retrieve model instances for all aliases in a result row.
   
   For a single result row (grouped by alias), creates or retrieves all
   corresponding model instances.
   
   @param grouped-data [hash-table] Row data grouped by alias
   @param alias->model [hash-table] Mapping from aliases to model classes
   @param record-cache [hash-table] Cache of instances
   @return [hash-table] Hash table mapping aliases to model instances
   "
  (let ((hydrated-row-instances (make-hash-table :test #'eq)))
    (maphash
     #'(lambda (alias data-plist)
         (let ((model-class (gethash alias alias->model)))
           (when model-class
             (let ((instance (hydrate-instance model-class data-plist record-cache)))
               (when instance
                 (setf (gethash alias hydrated-row-instances) instance))))))
     grouped-data)
    hydrated-row-instances))

(defmethod link-row-instances ((query <query>) hydrated-row-instances)
  "Links the model instances for a single row together based on the query's
   join definitions and the model's relation metadata.
   ex: #{ :blog =>    <blog {:id 1, :account-id: 1001, :account => (unbound)>
          :account => <account {:id 1001, :username \"user1\", :blogs => (unbound)>
       =>
       #{ :blog =>    <blog {:id 1, :account-id: 1001, :account => <instnact of account>
          :account => <account {:id 1001, :username \"user1\", :blogs => (<instance of blog>)> }
"
  (let ((alias->model (slot-value query 'alias->model))
        (base-alias (slot-value query 'alias)))
    (loop for join-obj in (slot-value query 'joins)
          do (let* ((target-alias (relation join-obj))
                    (source-alias (or (through join-obj) base-alias))
                    (source-inst (gethash source-alias hydrated-row-instances))
                    (target-inst (gethash target-alias hydrated-row-instances))
                    (source-model (gethash source-alias alias->model)))
               (when (and source-inst target-inst source-model)
                 (let* ((relations (getf (gethash source-model clails/model/base-model::*table-information*) :relations))
                        (rel-info (gethash target-alias relations)))
                   (when rel-info
                     (case (getf rel-info :type)
                       (:belongs-to
                        (setf (ref source-inst (getf rel-info :column)) target-inst))
                       (:has-many
                        ;; pushnew avoids duplicates if the same child is joined multiple times
                        (pushnew target-inst (ref source-inst (getf rel-info :as)) :test #'eq))))))))))

(defun finalize-has-many-relations (instances)
  "Initialize unbound :has-many relation slots to NIL.
   
   Ensures that for a list of instances, any :has-many relation slots that were not
   populated during result processing are initialized to NIL instead of being unbound.
   
   @param instances [list] List of model instances
   @return [list] The same list of instances
   "
  (loop for inst in instances
        do (let* ((model (class-name (class-of inst)))
                  (relations (getf (gethash model clails/model/base-model::*table-information*) :relations)))
             (when relations
               (maphash #'(lambda (alias rel-info)
                            (when (eq (getf rel-info :type) :has-many)
                              ;; Check if the slot is bound. If not, set it to nil.
                              (multiple-value-bind (val foundp) (gethash alias (slot-value inst 'clails/model/base-model::data))
                                (declare (ignore val))
                                (unless foundp
                                  (setf (ref inst alias) nil)))))
                        relations))))
  instances)

(defun build-model-instances (query result)
  "Process database result set into graph of nested model instances.
   
   Processes a raw database result set for a given <query> object and constructs
   a graph of nested model instances with relations properly linked.
   
   @param query [<query>] Query specification
   @param result [list] Raw database result set (list of plists)
   @return [list] List of unique main model instances with relations populated
   "
  (let* ((record-cache (make-hash-table :test #'eq)) ; Cache for all instances across all rows {model-class -> {id -> instance}}
         (main-instances (make-hash-table :test #'eql))) ; Cache for top-level instances {id -> instance}

    ;; Process each row from the database result
    (loop for row-plist in result
          do (let* ((grouped-data (group-row-data-by-alias row-plist))
                    (hydrated-row-instances (hydrate-instances-for-row grouped-data (slot-value query 'alias->model) record-cache)))

               ;; Link the instances created/retrieved for this specific row
               (link-row-instances query hydrated-row-instances)

               ;; Identify the main instance for this row and add it to our final set
               (let ((main-inst (gethash (slot-value query 'alias) hydrated-row-instances)))
                 (when main-inst
                   (setf (gethash (ref main-inst :ID) main-instances) main-inst)))))

    ;; Collect the unique main instances into a list
    (let ((final-results (loop for inst being the hash-value of main-instances collect inst)))
      ;; Post-process to ensure has-many slots are initialized
      (finalize-has-many-relations final-results))))


;;;; ----------------------------------------
;;;; Native Query Support (using cl-batis)

(defun execute-select-query (connection sql-string param-values)
  "Execute SELECT query and return results.

   @param connection [<connection>] Database connection
   @param sql-string [string] SQL query string
   @param param-values [list] Parameter values
   @return [list of plist] Query results (list of plists)
   "
  (when (log-level-enabled-p :sql :debug)
    (log.sql (format nil "sql: ~A" sql-string))
    (log.sql (format nil "params: ~S" param-values)))

  (dbi-cp:fetch-all
   (dbi-cp:execute
    (dbi-cp:prepare connection sql-string)
    param-values)))

(defun execute-update-query (connection sql-string param-values)
  "Execute UPDATE/INSERT/DELETE query and return affected row count.

   @param connection [<connection>] Database connection
   @param sql-string [string] SQL query string
   @param param-values [list] Parameter values
   @return [integer] Number of affected rows
   "
  (when (log-level-enabled-p :sql :debug)
    (log.sql (format nil "sql: ~A" sql-string))
    (log.sql (format nil "params: ~S" param-values)))

  (dbi-cp:execute
   (dbi-cp:prepare connection sql-string)
   param-values)
  (dbi-cp:row-count connection))

(defmethod execute-query ((sql <batis-sql>) named-values &key connection)
  "Execute SQL defined by cl-batis and return results.

   Uses gen-sql-and-params to convert the SQL definition to
   prepared statement SQL and parameter list, then executes it.
   SELECT queries return result rows, UPDATE queries return affected row count.

   @param sql [<batis-sql>] SQL definition created by select/defsql or update/defsql
   @param named-values [plist] Parameter values as property list
   @param connection [<connection>] Database connection (optional)
   @return [list of plist] Query results for SELECT queries
   @return [integer] Number of affected rows for UPDATE queries
   @condition database-error SQL execution error
   "
  (let ((conn (or connection (get-connection))))
    (multiple-value-bind (sql-string param-values)
        (gen-sql-and-params sql named-values)

      (let ((sql-type (slot-value sql 'batis.macro::sql-type)))
        (ecase sql-type
          (:select (execute-select-query conn sql-string param-values))
          (:update (execute-update-query conn sql-string param-values)))))))

