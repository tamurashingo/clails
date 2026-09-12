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
  (:import-from #:clails/environment
                #:*database-type*
                #:*%table-information-initialized*
                #:*%query-initialization-callbacks*)
  (:import-from #:clails/model/base-model
                #:<base-model>)
  (:import-from #:clails/model/query/type-conversion
                #:to-string
                #:to-text
                #:to-integer
                #:to-float
                #:to-decimal
                #:to-datetime
                #:to-date
                #:to-time
                #:to-boolean)
  (:import-from #:clails/util
                #:kebab->snake)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:export #:<query>
           #:<query-placeholder>
           #:ensure-initialized
           #:relation
           #:through
           #:query
           #:query-builder
           #:set-columns
           #:set-joins
           #:set-where
           #:set-order-by
           #:set-limit
           #:set-offset
           #:set-includes
           #:generate-query
           #:generate-values))
(in-package #:clails/model/query)

;;;; ----------------------------------------
;;;; Query builder: this file owns the <query> class, the `query` DSL macro,
;;;; the `query-builder` runtime constructor, all set-* mutators, and every
;;;; step of turning a <query> specification into a SQL template and its
;;;; parameters (WHERE-clause parsing, JOIN resolution, ORDER BY / LIMIT /
;;;; OFFSET generation). It does not execute SQL against a connection -- see
;;;; clails/model/query/crud for that -- and it does not implement the
;;;; to-string/to-integer/etc conversion functions themselves -- see
;;;; clails/model/query/type-conversion for those.
;;;;
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
    (lock-clause :initarg :lock-clause
                 :initform nil
                 :documentation "Lock clause for pessimistic locking")
   (includes :initarg :includes
             :initform nil
             :documentation "List of relation alias keywords to eagerly load in a single
                             batched query per relation (N+1 prevention), instead of one
                             query per record. Unlike :joins, an included relation is not
                             part of the primary SELECT -- it is loaded via additional
                             queries after the primary query executes. See
                             clails/model/query/crud:execute-query.")
   (inst :type <base-model>
         :documentation "Model instance")
   (alias->model :initform (make-hash-table)
                 :documentation "Hash table mapping alias names to model symbols")
   (query-cache :initform nil
                :documentation "Cached result of query parsing.
                                Format: (:sql-template <string>
                                        :where-params <list>
                                        :where-columns <list>
                                        :column-types <list>
                                        :limit-param <keyword|nil>
                                        :offset-param <keyword|nil>)
                                NIL when cache is invalid.

                                Example:
                                (:sql-template \"SELECT BLOG.ID as \\\"BLOG.ID\\\", BLOG.TITLE as \\\"BLOG.TITLE\\\" FROM BLOGS as BLOG WHERE BLOG.STATUS = ? AND BLOG.STAR > ? LIMIT ? OFFSET ?\"
                                 :where-params (:status :min-star)
                                 :where-columns ((:blog :status) (:blog :star))
                                 :column-types (:string :integer)
                                 :limit-param :limit
                                 :offset-param :offset)

                                With dynamic IN clause:
                                (:sql-template \"SELECT BLOG.ID as \\\"BLOG.ID\\\" FROM BLOGS as BLOG WHERE __IN_CLAUSE_BLOG_ID_:BLOG-IDS__\"
                                 :where-params ((:in-expansion \"IN\" \"BLOG.ID\" :blog-ids))
                                 :where-columns nil
                                 :column-types nil
                                 :limit-param nil
                                 :offset-param nil)")
   (query-cache-valid-p :initform nil
                        :documentation "Flag indicating if query-cache is valid.")
   (query-source-info :initform nil
                      :documentation "Original query definition for error messages"))
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

(defclass <query-placeholder> ()
  ((model :initarg :model
          :documentation "Model class symbol")
   (alias :initarg :alias
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
   (includes :initarg :includes
             :initform nil
             :documentation "List of relation alias keywords to eagerly load. See <query>.")
   (actual-query :initform nil
                 :accessor actual-query
                 :documentation "Actual <query> instance after initialization")
   (query-source-info :initform nil
                      :documentation "Original query definition for error messages"))
  (:documentation "Placeholder for query instances created before table information initialization."))


;;;; ----------------------------------------
;;;; placeholder methods

(defun ensure-initialized (placeholder)
  "Ensure the query placeholder has been initialized.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @return [<query>] Actual query instance
   @condition error Signaled when placeholder has not been initialized
   "
  (let ((actual (actual-query placeholder)))
    (if actual
        actual
        (error "Query not initialized. Call initialize-table-information first."))))


;;;; ========================================
;;;; export macro

(defmacro query (model &key as columns joins where order-by limit offset includes)
  "Create a query builder instance for the specified model.

   Provides a DSL for constructing SQL queries with support for joins,
   where clauses, ordering, pagination, and eager-loading of relations.

   @param model [symbol] Model class name (e.g., <blog>)
   @param as [keyword] Required alias for the model (e.g., :blog)
   @param columns [list] List of column specifications (e.g., ((blog :id :title) (user :name)))
   @param joins [list] List of join specifications (e.g., ((:inner-join :user) (:left-join :comments)))
   @param where [form] WHERE clause expression
   @param order-by [list] ORDER BY specifications
   @param limit [integer] LIMIT value
   @param offset [integer] OFFSET value
   @param includes [list] List of relation alias keywords (:has-many/:belongs-to, as declared
                          in the model's :relations) to eagerly load. After the primary query
                          executes, ONE additional batched query is issued per named relation
                          (e.g. WHERE company_id IN (...)) instead of one query per record --
                          see clails/model/query/crud:execute-query. Only relations declared
                          directly on `model` are supported (one level, not nested).
   @return [<query>] Query builder instance
   @return [<query-placeholder>] Query placeholder instance (if table information not initialized)
   @condition error Signaled when :as is missing or not a keyword

   Example:
   (query <blog>
     :as :blog
     :columns ((blog :id :title))
     :joins ((:inner-join :account))
     :where (:> (:blog :star) 0))

   Example (eager-loading to avoid N+1):
   (query <company>
     :as :company
     :includes (:departments))
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
                     joins-list))

           (make-source-info ()
             (list :model model
                   :as as
                   :columns columns
                   :joins joins
                   :where where
                   :order-by order-by
                   :limit limit
                   :offset offset
                   :includes includes)))
    `(if clails/environment:*%table-information-initialized*
         ;; Table information initialized: create actual query instance
         (let ((q (make-instance '<query>
                                 :model ',model
                                 :alias ',as
                                 :columns ',(expand-columns columns)
                                 :joins (list ,@(expand-joins joins))
                                 :where ',where
                                 :order-by ',order-by
                                 :limit ',limit
                                 :offset ',offset
                                 :includes ',includes)))
           (setf (slot-value q 'query-source-info) ',(make-source-info))
           q)
         ;; Not initialized: create placeholder and register callback
         (let ((placeholder (make-instance '<query-placeholder>
                                           :model ',model
                                           :alias ',as
                                           :columns ',(expand-columns columns)
                                           :joins (list ,@(expand-joins joins))
                                           :where ',where
                                           :order-by ',order-by
                                           :limit ',limit
                                           :offset ',offset
                                           :includes ',includes)))
           (setf (slot-value placeholder 'query-source-info) ',(make-source-info))
           (push (lambda ()
                   (let ((q (make-instance '<query>
                                           :model ',model
                                           :alias ',as
                                           :columns ',(expand-columns columns)
                                           :joins (list ,@(expand-joins joins))
                                           :where ',where
                                           :order-by ',order-by
                                           :limit ',limit
                                           :offset ',offset
                                           :includes ',includes)))
                     (setf (slot-value q 'query-source-info) ',(make-source-info))
                     (setf (actual-query placeholder) q)))
                 clails/environment:*%query-initialization-callbacks*)
           placeholder))))


(defun query-builder (model &key as)
  "Create a query builder instance for dynamic query construction.

   Unlike the query macro, this function allows runtime specification of
   query parameters, enabling dynamic query construction.

   @param model [symbol] Model class name (e.g., <blog>)
   @param as [keyword] Alias for the model (e.g., :blog)
   @return [<query>] Query builder instance
   @condition error Signaled when :as is not provided or not a keyword

   Example:
   (let ((q (query-builder '<blog> :as :blog)))
     (set-columns q '((blog :id :title)))
     (set-where q '(:= (:blog :status) :status))
     (execute-query q '(:status \"published\")))
   "
  (unless as
    (error ":as keyword is required for query-builder."))
  (unless (keywordp as)
    (error "The value for :as must be a keyword (e.g., :blog)."))

  (make-instance '<query>
                 :model model
                 :alias as))


(defgeneric set-columns (query columns)
  (:documentation "Set the columns to select in the query."))

(defmethod set-columns ((query <query>) columns)
  "Set the columns to select in the query.

   Replaces any previously set columns.

   @param query [<query>] Query builder instance
   @param columns [list] List of column specifications in grouped format
                         (e.g., ((blog :id :title) (account :name)))
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-columns q '((blog :id :title :content)))
   "
  (labels ((expand-columns (grouped-columns)
             (mapcan #'(lambda (group)
                         (let ((alias (car group))
                               (column-names (cdr group)))
                           (mapcar #'(lambda (column-name)
                                       (list alias column-name))
                                   column-names)))
                     grouped-columns)))
    (setf (slot-value query 'columns) (expand-columns columns))
    (invalidate-query-cache query)
    query))

(defmethod set-columns ((placeholder <query-placeholder>) columns)
  "Set the columns via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param columns [list] List of column specifications in grouped format
   @return [<query>] The actual query instance (for method chaining)
   @condition error Signaled when placeholder has not been initialized
   "
  (set-columns (ensure-initialized placeholder) columns))


(defgeneric set-joins (query joins)
  (:documentation "Set the JOIN clauses in the query."))

(defmethod set-joins ((query <query>) joins)
  "Set the JOIN clauses in the query.

   Replaces any previously set joins.

   @param query [<query>] Query builder instance
   @param joins [list] List of join specifications
                       (e.g., ((:inner-join :account) (:left-join :comments :through :account)))
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-joins q '((:inner-join :account)))
   "
  (labels ((expand-joins (joins-list)
             (mapcar #'(lambda (join-spec)
                         (destructuring-bind (join-type relation &key through) join-spec
                           (make-instance '<join-query>
                                          :join-type join-type
                                          :relation relation
                                          :through through)))
                     joins-list)))
    (setf (slot-value query 'joins) (expand-joins joins))
    ;; Rebuild alias map after changing joins
    (build-alias-map query)
    (invalidate-query-cache query)
    query))


(defmethod set-where ((query <query>) where-clause)
  "Set the WHERE clause in the query.

   Replaces any previously set WHERE clause.

   @param query [<query>] Query builder instance
   @param where-clause [list] WHERE clause expression (nil to remove WHERE clause)
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-where q '(:and (:= (:blog :status) :status)
                       (:> (:blog :star) :min-star)))
   "
  (setf (slot-value query 'where) where-clause)
  (invalidate-query-cache query)
  query)

(defmethod set-where ((placeholder <query-placeholder>) where-clause)
  "Set the WHERE clause via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param where-clause [list] WHERE clause expression
   @return [<query>] The actual query instance (for method chaining)
   @condition error Signaled when placeholder has not been initialized
   "
  (set-where (ensure-initialized placeholder) where-clause))


(defgeneric set-order-by (query order-by)
  (:documentation "Set the ORDER BY clause in the query."))

(defmethod set-order-by ((query <query>) order-by)
  "Set the ORDER BY clause in the query.

   Replaces any previously set ORDER BY clause.

   @param query [<query>] Query builder instance
   @param order-by [list] Order specifications
                          (e.g., (((:blog :created-at :desc) (:blog :id :asc))))
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-order-by q '(((:blog :star :desc) (:blog :id :asc))))
   "
  (setf (slot-value query 'order-by) order-by)
  (invalidate-query-cache query)
  query)


(defmethod set-limit ((query <query>) limit)
  "Set the LIMIT clause in the query.

   Replaces any previously set LIMIT.

   @param query [<query>] Query builder instance
   @param limit [integer|keyword|nil] LIMIT value, parameter keyword, or nil to remove
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-limit q 10)
   (set-limit q :limit-param)
   "
  (setf (slot-value query 'limit) limit)
  (invalidate-query-cache query)
  query)

(defmethod set-limit ((placeholder <query-placeholder>) limit)
  "Set the LIMIT clause via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param limit [integer|keyword|nil] LIMIT value, parameter keyword, or nil to remove
   @return [<query>] The actual query instance (for method chaining)
   @condition error Signaled when placeholder has not been initialized
   "
  (set-limit (ensure-initialized placeholder) limit))


(defgeneric set-offset (query offset)
  (:documentation "Set the OFFSET clause in the query."))

(defmethod set-offset ((query <query>) offset)
  "Set the OFFSET clause in the query.

   Replaces any previously set OFFSET.

   @param query [<query>] Query builder instance
   @param offset [integer|keyword|nil] OFFSET value, parameter keyword, or nil to remove
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-offset q 20)
   (set-offset q :offset-param)
   "
  (setf (slot-value query 'offset) offset)
  (invalidate-query-cache query)
  query)

(defmethod set-offset ((placeholder <query-placeholder>) offset)
  "Set the OFFSET clause via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param offset [integer|keyword|nil] OFFSET value, parameter keyword, or nil to remove
   @return [<query>] The actual query instance (for method chaining)
   @condition error Signaled when placeholder has not been initialized
   "
  (set-offset (ensure-initialized placeholder) offset))


(defgeneric set-includes (query includes)
  (:documentation "Set the list of relations to eagerly load in the query."))

(defmethod set-includes ((query <query>) includes)
  "Set the relations to eagerly load (N+1 prevention) in the query.

   Replaces any previously set includes. Each entry must be a relation alias
   keyword declared on the query's model via :has-many/:belongs-to. After the
   primary query executes, ONE additional batched query is issued per named
   relation instead of one query per record -- see
   clails/model/query/crud:execute-query.

   @param query [<query>] Query builder instance
   @param includes [list] List of relation alias keywords (e.g., '(:departments))
   @return [<query>] The query instance (for method chaining)

   Example:
   (set-includes q '(:departments))
   "
  (setf (slot-value query 'includes) includes)
  query)

(defmethod set-includes ((placeholder <query-placeholder>) includes)
  "Set the relations to eagerly load via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param includes [list] List of relation alias keywords
   @return [<query>] The actual query instance (for method chaining)
   @condition error Signaled when placeholder has not been initialized
   "
  (set-includes (ensure-initialized placeholder) includes))


;;;; ========================================
;;;; internals

;;; ----------------------------------------
;;; cache management

(defun invalidate-query-cache (query)
  "Invalidate the query cache.

   Called when any part of the query definition is modified.

   @param query [<query>] Query instance
   "
  (setf (slot-value query 'query-cache) nil)
  (setf (slot-value query 'query-cache-valid-p) nil))

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
                   (error "Relation `~A' not found for model `~A'~A" target-alias source-model (format-query-source-info query)))
                 (let ((target-model (getf rel-info :model)))
                   (setf (gethash target-alias alias->model) target-model)))))))

(defun format-query-source-info (query)
  "Format query source information for error messages.

   @param query [<query>] Query instance
   @return [string] Formatted source information, or empty string if not available
   "
  (let ((source-info (slot-value query 'query-source-info)))
    (if source-info
        (with-output-to-string (s)
          (format s "~%~%Query definition:")
          (format s "~%  (query ~A" (getf source-info :model))
          (format s "~%    :as ~A" (getf source-info :as))
          (when (getf source-info :columns)
            (format s "~%    :columns ~S" (getf source-info :columns)))
          (when (getf source-info :joins)
            (format s "~%    :joins ~S" (getf source-info :joins)))
          (when (getf source-info :where)
            (format s "~%    :where ~S" (getf source-info :where)))
          (when (getf source-info :order-by)
            (format s "~%    :order-by ~S" (getf source-info :order-by)))
          (when (getf source-info :limit)
            (format s "~%    :limit ~S" (getf source-info :limit)))
          (when (getf source-info :offset)
            (format s "~%    :offset ~S" (getf source-info :offset)))
          (when (getf source-info :includes)
            (format s "~%    :includes ~S" (getf source-info :includes)))
          (format s ")"))
        "")))

;;; ----------------------------------------
;;; query

(defun get-or-parse-query (query)
  "Get cached query parse result or parse and cache it.

   @param query [<query>] Query instance
   @return [string] SQL template string
   @return [list] WHERE clause parameter keywords
   @return [list] WHERE clause column specs
   @return [list] Column types for parameters
   @return [keyword|nil] LIMIT parameter keyword
   @return [keyword|nil] OFFSET parameter keyword
   "
  (if (slot-value query 'query-cache-valid-p)
      ;; Return from cache
      (let ((cache (slot-value query 'query-cache)))
        (values (getf cache :sql-template)
                (getf cache :where-params)
                (getf cache :where-columns)
                (getf cache :column-types)
                (getf cache :limit-param)
                (getf cache :offset-param)))
      ;; Parse and cache
      (let* ((alias->model (slot-value query 'alias->model))
             (base-alias (slot-value query 'alias))
             (base-model (slot-value query 'model))
             (base-table-name (getf (gethash base-model clails/model/base-model::*table-information*)
                                    :table-name))
             (joins (resolve-joins query))
             (columns (mapcar #'(lambda (c) (column-pair-to-name c T))
                              (generate-query-columns query alias->model)))
             (where-parts (multiple-value-list (parse-where-claude (slot-value query 'where))))
             (order-by (generate-order-by (slot-value query 'order-by)))
             (offset-spec (generate-offset (slot-value query 'offset)))
             (limit-spec (generate-limit (slot-value query 'limit)))
             (lock-clause (slot-value query 'lock-clause))
             (sql-template (format nil "SELECT ~{~A~^, ~} FROM ~A as ~A~@[ ~A~]~@[ WHERE ~A~]~@[ ORDER BY ~{~A~^, ~}~]~@[ LIMIT ~A~]~@[ OFFSET ~A~]~@[ ~A~]"
                                   columns
                                   base-table-name
                                   (kebab->snake base-alias)
                                   (format nil "~{~A~^ ~}" joins)
                                   (first where-parts)
                                   order-by
                                   (getf limit-spec :limit)
                                   (getf offset-spec :offset)
                                   lock-clause))
             ;; Build column types list for WHERE clause parameters
             (column-types (loop for column-spec in (third where-parts)
                                 collect (when column-spec
                                          (let* ((model-symbol (gethash (first column-spec) alias->model))
                                                 (column-keyword (second column-spec)))
                                            (when model-symbol
                                              (get-column-type-from-model model-symbol column-keyword)))))))

        ;; Save to cache
        (setf (slot-value query 'query-cache)
              (list :sql-template sql-template
                    :where-params (second where-parts)
                    :where-columns (third where-parts)
                    :column-types column-types
                    :limit-param (getf limit-spec :keyword)
                    :offset-param (getf offset-spec :keyword)))
        (setf (slot-value query 'query-cache-valid-p) t)

        ;; Return results
        (values sql-template
                (second where-parts)
                (third where-parts)
                column-types
                (getf limit-spec :keyword)
                (getf offset-spec :keyword)))))

(defmethod generate-query ((query <query>) &optional named-values &key (convert-types t))
  "Generate SQL query and parameters from query specification.

   Constructs SELECT statement with joins, where clause, order by, limit, and offset.
   Handles dynamic IN clause expansion for parameterized queries.
   Uses cached parsing results if available.

   @param query [<query>] Query specification
   @param named-values [plist] Named parameter values
   @param convert-types [boolean] Whether to perform automatic type conversion (default: t)
   @return [string] SQL query string
   @return [list] List of parameter values
   "
  (multiple-value-bind (sql-template where-params where-columns column-types limit-param offset-param)
      (get-or-parse-query query)

    (let ((final-sql sql-template)
          (final-params '())
          (param-specs '())
          (alias->model (slot-value query 'alias->model)))

      ;; Process WHERE clause parameters - collect all parameter specs in order
      (loop for param in where-params
            for param-index from 0
            do (if (and (listp param) (eq (car param) :in-expansion))
                   ;; Dynamic IN clause expansion - store spec for later processing
                   (destructuring-bind (op column-sql keyword) (cdr param)
                     (let ((placeholder (format nil "__IN_CLAUSE_~A_~A__"
                                                (cl-ppcre:regex-replace-all "[.:]" column-sql "_")
                                                keyword)))
                       (push (list :in-expansion
                                   :op op
                                   :column-sql column-sql
                                   :keyword keyword
                                   :placeholder placeholder)
                             param-specs)))
                   ;; Regular parameter - store spec for later processing
                   (push (list :regular
                               :keyword param
                               :column-type (nth param-index column-types))
                         param-specs)))

      ;; Reverse to maintain original order
      (setf param-specs (nreverse param-specs))

      ;; Process all parameters in order
      (loop for spec in param-specs
            do (case (first spec)
                 (:in-expansion
                  (let* ((op (getf (rest spec) :op))
                         (column-sql (getf (rest spec) :column-sql))
                         (keyword (getf (rest spec) :keyword))
                         (placeholder (getf (rest spec) :placeholder))
                         (values (getf named-values keyword)))
                    (if (null values)
                        (let ((replacement (if (string= op "IN") "1=0" "1=1")))
                          (setf final-sql (cl-ppcre:regex-replace-all
                                           (cl-ppcre:quote-meta-chars placeholder)
                                           final-sql
                                           replacement)))
                        (let* ((question-marks (format nil "(~{?~*~^, ~})" values))
                               (replacement (format nil "~A ~A ~A" column-sql op question-marks)))
                          (setf final-sql (cl-ppcre:regex-replace-all
                                           (cl-ppcre:quote-meta-chars placeholder)
                                           final-sql
                                           replacement))
                          (appendf final-params values)))))
                 (:regular
                  (let* ((keyword (getf (rest spec) :keyword))
                         (column-type (getf (rest spec) :column-type))
                         (value (getf named-values keyword))
                         (converted-value (if (and convert-types column-type)
                                              (convert-value-by-type value column-type)
                                              value)))
                    (appendf final-params (list converted-value))))))

      ;; Add LIMIT/OFFSET parameters
      (when limit-param
        (appendf final-params (list (getf named-values limit-param))))
      (when offset-param
        (appendf final-params (list (getf named-values offset-param))))

      ;; For debug
      (when (log-level-enabled-p :debug :sql)
        (log.sql (format nil "sql: ~S" final-sql))
        (log.sql (format nil "params: ~S" final-params)))

      (values final-sql final-params))))


(defmethod generate-query ((placeholder <query-placeholder>) &optional named-values &key (convert-types t))
  "Generate SQL query via placeholder delegation.

   @param placeholder [<query-placeholder>] Query placeholder instance
   @param named-values [plist] Named parameter values
   @param convert-types [boolean] Whether to perform automatic type conversion (default: t)
   @return [string] SQL query string
   @return [list] List of parameter values
   @condition error Signaled when placeholder has not been initialized
   "
  (generate-query (ensure-initialized placeholder) named-values :convert-types convert-types))


(defmethod resolve-joins ((query <query>))
  "Generate JOIN clauses for the query.

   @param query [<query>] Query instance
   @return [list] List of JOIN SQL strings
   "
  (let ((base-model-alias (slot-value query 'alias))
        (alias->model (slot-value query 'alias->model)))
    (loop for join-obj in (slot-value query 'joins)
          with joins-sql and keywords
          do (let ((sql (generate-join-sql join-obj base-model-alias alias->model query)))
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


(defmethod generate-join-sql ((join-obj <join-query>) base-model-alias alias->model query)
  "Generate SQL JOIN clause for a single join specification.

   Constructs INNER JOIN or LEFT JOIN with ON clause based on relation metadata.

   @param join-obj [<join-query>] Join specification
   @param base-model-alias [keyword] Base model alias
   @param alias->model [hash-table] Mapping from aliases to model classes
   @param query [<query>] Query instance for error messages
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
          (error "Relation `~A` not found for model `~A`~A" (relation join-obj) source-model (format-query-source-info query)))
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
                   (kebab->snake target-table-name)
                   (kebab->snake target-alias)
                   on-clause))))))


(defun parse-where-claude (where)
  "Parse WHERE clause expression tree into SQL.

   Supports operators: :=, :<, :<=, :>, :>=, :<>, :!=, :like, :not-like,
   :between, :not-between, :in, :not-in, :null, :not-null, :and, :or.

   @param where [list] WHERE clause expression (nil for no where clause)
   @return [string] SQL WHERE clause string
   @return [list] List of parameter keywords
   @return [list] List of column specs corresponding to parameters
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
                     with exp and keywords and columns
                     do (multiple-value-setq (exp keywords columns) (parse-where-claude expression))
                     collect exp into exp-list
                     append keywords into all-keywords
                     append columns into all-columns
                     finally (return (values (format nil "(~{~A~^ AND ~})" exp-list)
                                             all-keywords
                                             all-columns))))
              ((eq elm :or)
               (loop for expression in (cdr where)
                     with exp and keywords and columns
                     do (multiple-value-setq (exp keywords columns) (parse-where-claude expression))
                     collect exp into exp-list
                     append keywords into all-keywords
                     append columns into all-columns
                     finally (return (values (format nil "(~{~A~^ OR ~})" exp-list)
                                             all-keywords
                                             all-columns))))
              (t
               (error "where claude: parse error `~A`" elm))))))

(defun parse-in-clause (op exp)
  "Parse IN clause with static list of values.

   @param op [string] Operator (\"IN\" or \"NOT IN\")
   @param exp [list] Expression (column-spec values-list)
   @return [string] SQL clause string
   @return [list] List of parameters
   @return [list] List of column specs corresponding to parameters
   @condition error Signaled when values are not a list
   "
  (let (column-sql column-spec)
    (multiple-value-bind (sql param)
        (lexer2 (car exp))
      (setf column-sql sql)
      (setf column-spec (car exp))
      (when param
        (error "Unexpected parameter in column part of IN clause.")))
    (let ((in-values (cadr exp)))
      (when (and (listp in-values) (eq (car in-values) 'quote))
        (setf in-values (cadr in-values)))
      (unless (listp in-values)
        (error "IN clause expects a list of values (e.g., '(1 2 3) or (:id1 :id2)). Got: ~S" in-values))
      (let (placeholders params columns)
        (dolist (val in-values)
          (multiple-value-bind (ph p) (lexer2 val)
            (push ph placeholders)
            (when p
              (appendf params (ensure-list p))
              (when (and (listp column-spec) (= (length column-spec) 2))
                (push column-spec columns)))))
        (if (null placeholders)
            (if (string= op "IN")
                (values "1=0" nil nil)
                (values "1=1" nil nil))
            (values (format nil "~A ~A (~{~A~^, ~})" column-sql op (nreverse placeholders))
                    (flatten params)
                    (nreverse columns)))))))

(defun parse-in-dynamic (op exp)
  "Parse IN clause with dynamic parameterized values.

   Creates placeholder for later expansion with actual parameter values.

   @param op [string] Operator (\"IN\" or \"NOT IN\")
   @param exp [list] Expression (column-spec param-keyword)
   @return [string] Placeholder string for later replacement
   @return [list] List containing :in-expansion spec
   @return [list] Empty list (no column mapping for dynamic IN)
   "
  (let* ((column-spec (first exp))
         (param-keyword (second exp))
         (column-sql (column-pair-to-name column-spec))
         (placeholder (format nil "__IN_CLAUSE_~A_~A__"
                              (cl-ppcre:regex-replace-all "[.:]" column-sql "_")
                              param-keyword)))
    (values placeholder
            (list (list :in-expansion op column-sql param-keyword))
            nil)))

(defun parse-between-clause (op exp)
  "Parse BETWEEN clause.

   @param op [string] Operator (\"BETWEEN\" or \"NOT BETWEEN\")
   @param exp [list] Expression (column-spec value1 value2)
   @return [string] SQL BETWEEN clause
   @return [list] List of parameter keywords
   @return [list] List of column specs corresponding to parameters
   @condition error Signaled when column part contains parameters
   "
  (let (column-sql column-spec val1-sql val2-sql keywords columns)
    ;; Column
    (multiple-value-bind (sql param) (lexer2 (first exp))
      (setf column-sql sql)
      (setf column-spec (first exp))
      (when param (error "Unexpected parameter in column part of BETWEEN clause.")))
    ;; Value 1
    (multiple-value-bind (sql param) (lexer2 (second exp))
      (setf val1-sql sql)
      (when param
        (appendf keywords (ensure-list param))
        (when (and (listp column-spec) (= (length column-spec) 2))
          (push column-spec columns))))
    ;; Value 2
    (multiple-value-bind (sql param) (lexer2 (third exp))
      (setf val2-sql sql)
      (when param
        (appendf keywords (ensure-list param))
        (when (and (listp column-spec) (= (length column-spec) 2))
          (push column-spec columns))))
    (values (format nil "~A ~A ~A AND ~A" column-sql op val1-sql val2-sql)
            keywords
            (nreverse columns))))

;; TODO: rename
(defun parse-exp2 (op exp)
  "Parse binary comparison expression.

   @param op [keyword|string] Operator (:=, :<, :>, etc. or \"LIKE\", \"NOT LIKE\")
   @param exp [list] Expression (left-operand right-operand)
   @return [string] SQL comparison expression
   @return [list] List of parameter keywords
   @return [list] List of column specs corresponding to parameters
   "
  (let (x y keywords columns)
    (multiple-value-bind (col1 param1)
        (lexer2 (car exp))
      (setf x col1)
      (when param1
        (appendf keywords (ensure-list param1))
        (when (and (listp (cadr exp)) (= (length (cadr exp)) 2))
          (push (cadr exp) columns))))
    (multiple-value-bind (col2 param2)
        (lexer2 (cadr exp))
      (setf y col2)
      (when param2
        (appendf keywords (ensure-list param2))
        (when (and (listp (car exp)) (= (length (car exp)) 2))
          (push (car exp) columns))))
    (values (format nil "~A ~A ~A" x op y)
            keywords
            (nreverse columns))))


;; TODO: rename
(defun parse-null2 (op exp)
  "Parse NULL check expression.

   @param op [keyword] Operator (:null or :not-null)
   @param exp [list] Expression (column-spec)
   @return [string] SQL NULL check (\"IS NULL\" or \"IS NOT NULL\")
   @return [list] List of parameter keywords
   @return [list] Empty list (NULL checks don't have parameters)
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
            keywords
            nil)))


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


;;; ----------------------------------------
;;; column type lookup for WHERE-clause parameter conversion
;;;
;;; These bridge query building to clails/model/query/type-conversion: they
;;; resolve a WHERE-clause parameter's column type from model metadata and
;;; dispatch to the matching to-string/to-integer/etc conversion function.

(defun get-column-type-from-model (model-symbol column-keyword)
  "Get column type from model's column information.

   @param model-symbol [symbol] Model class symbol
   @param column-keyword [keyword] Column name keyword
   @return [keyword] Column type keyword (e.g., :string, :integer, :boolean)
   @return [nil] If column not found
   "
  (let ((model-inst (make-instance model-symbol)))
    (loop for col in (slot-value model-inst 'clails/model/base-model::columns)
          when (eq (getf col :name) column-keyword)
          return (getf col :type))))

(defun convert-value-by-type (value type-keyword)
  "Convert value based on type keyword.

   @param value [t] Value to convert
   @param type-keyword [keyword] Type keyword (e.g., :string, :integer, :boolean)
   @return [t] Converted value
   "
  (case type-keyword
    (:string (to-string value))
    (:text (to-text value))
    (:integer (to-integer value))
    (:float (to-float value))
    (:decimal (to-decimal value))
    (:datetime (to-datetime value))
    (:date (to-date value))
    (:time (to-time value))
    (:boolean (to-boolean value))
    (t value)))


;;; ----------------------------------------
;;; generate values

(defun generate-values (named-params named-values)
  "Extract parameter values in order from named values plist.

   @param named-params [list] List of parameter keywords in order (e.g., (:start-date :end-date :start-date))
   @param named-values [plist] Property list of parameter values (e.g., (:start-date \"2025-01-01\" :end-date \"2025-12-31\"))
   @return [list] List of values in parameter order (e.g., (\"2025-01-01\" \"2025-12-31\" \"2025-01-01\"))
   "
  (loop for key in named-params
        collect (getf named-values key)))
