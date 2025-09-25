(in-package #:cl-user)
(defpackage #:clails/model/query
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:ensure-list
                #:flatten)
  (:import-from #:clails/environment
                #:*database-type*)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:validate
                #:ref
                #:has-error-p
                #:has-dirty-p
                #:clear-error
                #:clear-dirty-flag)
  (:import-from #:clails/util
                #:kebab->snake
                #:snake->kebab
                #:plist-exists)
  (:export #:select
           #:make-record
           #:save
           #:get-last-id-impl
           #:query
           #:execute-query
           #:build-model-instances))
(in-package #:clails/model/query)


(defmethod save ((inst <base-model>) &key connection)
  (clear-error inst)
  (validate inst)
  (unless (has-error-p inst)
    (prog1
        (if (ref inst :id)
            (if (has-dirty-p inst)
                (update1 inst :connection connection)
                inst)
            (insert1 inst :connection connection))
      (clear-dirty-flag inst))))


(defun generate-select-query (inst where order-by)
  (let* ((params (make-array (length where)
                             :fill-pointer 0))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (mapcar #'kebab->snake
                          (fetch-columns inst)))
         (conditions (if where
                         (parse-where where params)
                         nil))
         (sort (if order-by
                  (parse-order-by order-by)
                   nil)))
    (list :query (format NIL "SELECT ~{~A~^, ~} FROM ~A ~@[ WHERE ~A ~] ~@[ ORDER BY ~{~{~A~^ ~}~^, ~}~]" columns table-name conditions sort)
          :params (coerce params 'list))))


(defun fetch-columns (inst &key insert update)
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        as dirty-flag-hash = (slot-value inst 'clails/model/base-model::dirty-flag)
        when (or (and insert
                      (not (eq (getf column :name) :id)))
                 ;; update column if dirty
                 (and update
                      (not (eq (getf column :name) :id))
                      (not (eq (getf column :name) :created-at))
                      (or (eq (getf column :name) :updated-at)
                          (gethash (getf column :name) dirty-flag-hash)))
                 (and (not insert)
                      (not update)))
          collect (string (getf column :name))))


(defun parse-where (where-cond params)
  (assert (not (null where-cond)))
  (let ((elm (car where-cond)))
    (cond ((eq elm '=)
           (parse-exp '= (cdr where-cond) params))
          ((eq elm '<)
           (parse-exp '< (cdr where-cond) params))
          ((eq elm '<=)
           (parse-exp '<= (cdr where-cond) params))
          ((eq elm '>)
           (parse-exp '> (cdr where-cond) params))
          ((eq elm '>=)
           (parse-exp '>= (cdr where-cond) params))
          ((eq elm '<>)
           (parse-exp '<> (cdr where-cond) params))
          ((eq elm '!=)
           (parse-exp '!= (cdr where-cond) params))
          ((string-equal elm "null")
           (parse-null 'null (cdr where-cond) params))
          ((string-equal elm "not-null")
           (parse-null 'not-null (cdr where-cond) params))
          ((eq elm 'and)
           (format NIL "(~{~A~^ AND ~})"
                   (loop for exp in (cdr where-cond)
                         collect (parse-where exp params))))
          ((eq elm 'or)
           (format NIL "(~{~A~^ OR ~})"
                   (loop for exp in (cdr where-cond)
                         collect (parse-where exp params))))
          (t (error "parse error: ~A" elm)))))

(defun parse-exp (op exp params)
  (assert (= 2 (length exp)))
  (let (x y))
    (multiple-value-bind (col1 param1)
        (lexer (car exp))
      (setf x col1)
      (when param1
        (vector-push param1 params)))
    (multiple-value-bind (col2 param2)
        (lexer (cadr exp))
      (setf y col2)
      (when param2
        (vector-push param2 params)))
    (format NIL "~A ~A ~A" x op y))

(defun parse-null (op exp params)
  (let (x)
    (multiple-value-bind (col1 param1)
        (lexer (car exp))
      (setf x col1)
      (when param1
        (vector-push param1 params)))
    (format nil "~A ~A" x (if (eq op 'null)
                              "IS NULL"
                              "IS NOT NULL"))))


(defun lexer (param)
  (cond ((or (stringp param)
             (numberp param))
          (values "?" param))
        ((and (symbolp param)
              (not (keywordp param)))
          (values (kebab->snake param) NIL))
        (t (values param NIL))))

(defun parse-order-by (params &optional order)
  (flet ((convert (p)
         (when (and (not (symbolp p))
                    (not (stringp p)))
           (error "parse error: expect symbol or string but got ~A" p))
         (kebab->snake (string p))))
    (if (null params)
        order
        (let ((p (car params)))
          (cond ((or (symbolp p) (stringp p))
                 (parse-order-by (cdr params)
                                 (append order (list (list (convert p) "ASC")))))
                ((listp p)
                 (parse-order-by (cdr params)
                                 (append order
                                         (list
                                           (list (convert (car p))
                                                 (cond ((eq (cadr p) :asc)
                                                        "ASC")
                                                       ((eq (cadr p) :desc)
                                                        "DESC")
                                                       (t (error "parse error: order by expected keyword :ASC or :DESC but ~A" p))))))))
                (t (error "parse error: expect symbol, string or list but got ~A" p)))))))


(defun make-record (model-name &rest values)
  "(let ((inst (make-record '<todo> :title \"create new project\" :done nil)))
     (save inst))"
  (let ((inst (make-instance model-name)))
    (loop for (key value) on values by #'cddr
          do (setf (ref inst key) value))
    inst))

(defun make-record-from (model-name &rest db-values)
  (let ((inst (make-instance model-name))
        (columns-plist (clails/model/base-model::get-columns-plist model-name)))
    (loop for (key db-value) on db-values by #'cddr
          do (let* ((key (intern (snake->kebab key) :KEYWORD))
                    (fn (getf (getf columns-plist key) :DB-CL-FN)))
              (setf (ref inst key)
                    (funcall fn db-value))))
    (clear-dirty-flag inst)
    inst))


;(defun save (inst)
;  (if (ref inst :id)
;      (update1 inst)
;      (insert1 inst)))


(defun insert1 (inst &key connection)
  (let* ((current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (fetch-columns inst :insert T))
         (sql (format NIL "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{?~*~^, ~})" table-name (mapcar #'kebab->snake columns) columns))
         (params (alexandria:alist-plist (loop for colstr in columns
                                               as  colkey = (intern colstr :KEYWORD)
                                               collect (cons colkey (ref inst colkey))))))

    ;; set created-at and updated-at
    (setf (getf params :created-at) current-datetime)
    (setf (getf params :updated-at) current-datetime)

    ;; convert parameter
    ;; plist -> values
    (setf params (convert-cl-db-values params inst))

    ;; TODO: debug
    (format t "debug: query: ~S~%" sql)
    (format t "debug: params: ~S~%" params)

    (let ((body #'(lambda (connection)
                    (dbi-cp:execute
                     (dbi-cp:prepare connection sql)
                     params)
                    (let ((last-id (get-last-id connection)))
                      (setf (ref inst :id) last-id)
                      (setf (ref inst :created-at) current-datetime)
                      (setf (ref inst :updated-at) current-datetime))
                    inst)))

      (if connection
          (funcall body connection)
          (clails/model/connection:with-db-connection (connection)
            (funcall body connection))))))

(defun get-last-id (connection)
  (get-last-id-impl *database-type* connection))

(defgeneric get-last-id-impl (database-type connection)
  (:documentation "get last id"))

(defun update1 (inst &key connection)
  (let* ((current-datetime (get-universal-time))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (fetch-columns inst :update T))
         (sql (format NIL "UPDATE ~A SET ~{~A = ?~^, ~} WHERE id = ?" table-name (mapcar #'kebab->snake columns)))
         (params (alexandria:alist-plist (loop for colstr in columns
                                               as  colkey = (intern colstr :KEYWORD)
                                               collect (cons colkey (ref inst colkey))))))

    ;; set updated-at
    (setf (getf params :updated-at) current-datetime)

    ;; convert parameter
    ;; plist -> values
    (setf params (convert-cl-db-values params inst))

    ;; append id
    (setf params (append params
                         (list (ref inst :id))))

    ;; TODO: debug
    (format t "debug: query: ~A~%" sql)
    (format t "debug: params: ~A~%" params)

    (let ((body #'(lambda (connection)
                    (dbi-cp:execute
                     (dbi-cp:prepare connection sql)
                     params)
                    (setf (ref inst :updated-at) current-datetime)
                    inst)))

      (if connection
          (funcall body connection)
          (clails/model/connection:with-db-connection (connection)
            (funcall body connection))))))


(defun convert-cl-db-values (params inst)
  (loop for column in (slot-value inst 'clails/model/base-model::columns)
        when (plist-exists params (getf column :name))
          collect (let ((name (getf column :name))
                        (fn (getf column :cl-db-fn)))
                    (funcall fn (getf params name)))))






#|

query example:

(query <blog>
:where (= (blog :id) 1))


(query <writer>
:as writer
:columns ((writer :id :name))
:where (:like (writer :name) :search-name)
:order-by ((writer :id)))


(query <blog>
:as blog
:joins ((:inner-join <writer> :as writer
:on (= (writer :id)
(blog :writer-id)))
(:left-join <comment> :as comment
:on (= (blog :id)
(comment :blog-id))))
:where (> (blog :star) 0)
:order-by ((blog :star :desc)
(blog :id)))


|#


(defclass <query> ()
  ((model :initarg :model)
   (alias :initarg :alias
          :initform nil)
   (columns :initarg :columns
            :initform nil)
   (joins :initarg :joins
          :initform nil)
   (where :initarg :where
          :initform nil)
   (order-by :initarg :order-by
             :initform nil)
   (limit :initarg :limit
          :initform nil)
   (offset :initarg :offset
           :initform nil)
   (inst :type 'clails/model/base-model::<base-model>)
   (alias->model :initform (make-hash-table)
                 :documentation "alias name to model symbol")))

(defclass <join-query> ()
  ((join-type :initarg :join-type :reader join-type)
   (relation :initarg :relation :reader relation)
   (through :initarg :through :initform nil :reader through)
   (previous :initform nil :accessor previous-join)))



(defmethod initialize-instance :after ((q <query>) &rest initargs)
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



(defmacro query (model &key as columns joins where order-by limit offset)
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


(defmethod execute-query ((query <query>) named-values &key connection)
  (let* ((q (generate-query query))
         (sql (getf q :query))
         (named-params (getf q :keywords)))
          ;; TODO: debug
    (format t "debug: query: ~A~%" sql)
    (format t "debug: params: ~A~%" named-params)
    (let ((result (clails/model/connection:with-db-connection (connection)
                    (dbi-cp:fetch-all
                     (dbi-cp:execute
                      (dbi-cp:prepare connection sql)
                      (generate-values named-params named-values))))))
      (build-model-instances query result))))


(defmethod generate-join-sql ((join-obj <join-query>) base-model-alias alias->model)
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

(defmethod resolve-joins ((query <query>))
  (let ((base-model-alias (slot-value query 'alias))
        (alias->model (slot-value query 'alias->model)))
    (loop for join-obj in (slot-value query 'joins)
          with joins-sql and keywords
          do (let ((sql (generate-join-sql join-obj base-model-alias alias->model)))
               (push sql joins-sql))
          finally (return (nreverse joins-sql)))))

(defmethod generate-query-columns ((query <query>) alias->model)
  (if (slot-value query 'columns)
      (slot-value query 'columns)
      (loop for alias being the hash-key of (slot-value query 'alias->model)
              using (hash-value model)
            append (let ((inst (make-instance model)))
                     (loop for column in (slot-value inst 'clails/model/base-model::columns)
                           collect (list alias (getf column :name)))))))

(defmethod generate-query ((query <query>))
  (let* ((alias->model (slot-value query 'alias->model))
         (base-alias (slot-value query 'alias))
         (base-model (slot-value query 'model))
         (base-table-name (getf (gethash base-model clails/model/base-model::*table-information*) :table-name)))

    (let* ((joins (resolve-joins query))
           (columns (mapcar #'(lambda (c) (column-pair-to-name c T))
                            (generate-query-columns query alias->model)))
           (where-keywords (multiple-value-bind (w kw)
                               (parse-where-claude (slot-value query 'where))
                             (list :where w :keywords kw)))
           (order-by (generate-order-by (slot-value query 'order-by)))
           (offset (generate-offset (slot-value query 'offset)))
           (limit (generate-limit (slot-value query 'limit))))

      (list :query (format nil "SELECT ~{~A~^, ~} FROM ~A as ~A~@[ ~A~]~@[ WHERE ~A~]~@[ ORDER BY ~{~A~^, ~}~]~@[ LIMIT ~A~]~@[ OFFSET ~A~]"
                           columns
                           base-table-name
                           (kebab->snake base-alias)
                           (format nil "~{~A~^ ~}" joins)
                           (getf where-keywords :where)
                           order-by
                           (getf limit :limit)
                           (getf offset :offset))
            :keywords (flatten (append (ensure-list (getf where-keywords :keywords))
                                       (ensure-list (getf limit :keyword))
                                       (ensure-list (getf offset :keyword))))))))


(defun generate-values (named-params named-values)
  "named-params: '(:start-date :end-date :start-date)
   named-values: '(:start-date \"2025-01-01\" :end-date \"2025-12-31\")

   returns -> '(\"2025-01-01\" \"2025-12-31\" \"2025-01-01\")"
  (loop for key in named-params
        collect (getf named-values key)))


(defun parse-where-claude (where)
  (if (not where)
      nil
      (let ((elm (car where)))
        (cond ((find elm '(:= :< :<= :> :>= :<> :!=))
               (parse-exp2 elm (cdr where)))
              ((eq elm :like)
               (parse-exp2 "LIKE" (cdr where)))
              ((eq elm :not-like)
               (parse-exp2 "NOT LIKE" (cdr where)))
              ((eq elm :null)
               (parse-null2 :null (cdr where)))
              ((eq elm :not-null)
               (parse-null2 :not-null (cdr where)))
              ((eq elm :and)
               (loop for expression in (cdr where)
                     with exp and keywords
                     do (multiple-value-setq (exp keywords) (parse-where-claude expression))
                     collect exp into exp-list
                     when keywords
                       collect keywords into keywords-list
                     finally (return (values (format nil "(~{~A~^ AND ~})" exp-list)
                                             (flatten keywords-list)))))
              ((eq elm :or)
               (loop for expression in (cdr where)
                     with exp and keywords
                     do (multiple-value-setq (exp keywords) (parse-where-claude expression))
                     collect exp into exp-list
                     when keywords
                       collect keywords into keywords-list
                     finally (return (values (format nil "(~{~A~^ OR ~})" exp-list)
                                             (flatten keywords-list)))))
              (t
               (error "where claude: parse error `~A`" elm))))))

;; TODO: rename
(defun parse-exp2 (op exp)
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
  (if as (format nil "~A.~A as \"~A.~A\"" (kebab->snake (car pair)) (kebab->snake (cadr pair)) (kebab->snake (car pair)) (kebab->snake (cadr pair)))
         (format nil "~A.~A" (kebab->snake (car pair)) (kebab->snake (cadr pair)))))


(defun generate-order-by (params &optional order)
  "((:blog :star :desc)
    (:blog :id))
   => (\"BLOG.STAR DESC\" \"BLOG.ID\")"
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
  (cond ((null limit)
         (list :limit nil
               :keyword nil))
        ((keywordp limit)
         (list :limit "?"
               :keyword limit))
        (t
         (list :limit limit
               :keyword nil))))

;;;
;;; New functions for processing query results
;;;

(defun split-db-column-name (keyword-name)
  "Splits a keyword like :|TABLE.COLUMN| into two keywords, :TABLE and :COLUMN.
   The keyword comes from the database driver."
  (let* ((str (string keyword-name))
         (dot-pos (position #\. str)))
    (if dot-pos
        (values (intern (snake->kebab (string-upcase (subseq str 0 dot-pos))) :keyword)
                (intern (snake->kebab (string-upcase (subseq str (1+ dot-pos)))) :keyword))
        (error "Invalid column name from DB: ~A. Expected 'ALIAS.COLUMN' format." keyword-name))))

(defun group-row-data-by-alias (row-plist)
  "Groups a flat plist of results from the DB into a hash table where keys are
   table aliases and values are plists of column data for that alias."
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
  "Creates a model instance from a plist of data, or retrieves it from cache if it
   has already been created for the same ID."
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
  "For a single result row (grouped by alias), creates or retrieves all
   corresponding model instances."
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
  "Ensures that for a list of instances, any :has-many relation slots that were not
   populated during result processing are initialized to NIL instead of being unbound."
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
  "Processes a raw database result set for a given <query> object and constructs
   a graph of nested model instances."
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


