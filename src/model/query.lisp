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
                #:ref)
  (:import-from #:clails/util
                #:kebab->snake
                #:plist-exists)
  (:export #:select
           #:make-record
           #:save
           #:get-last-id-impl))
(in-package #:clails/model/query)



(defun select (model-name &key where order-by connection)
  "Given a model name, returns instances that match the conditions
(select '<todo>) => (#<TODO> #<TODO> #<TODO>)
(select '<todo> :where '(= id 1)) => (#<TODO>)
(select '<todo> :where '(or (= id 1)
                            (= done false))) => (#<TODO> #<TODO>)
(select '<todo> :order-by '(id))
(select '<todo> :order-by '((id :desc) (created-at :asc)))
"
  (let* ((inst (make-instance model-name))
         (select (generate-select-query inst where order-by))
         (body #'(lambda (connection)
                   (let ((result (dbi-cp:fetch-all
                                  (dbi-cp:execute
                                   (dbi-cp:prepare connection (getf select :query))
                                   (getf select :params)))))
                     (loop for row in result
                           collect (let ((ret (make-instance model-name)))
                                     (loop for column in (slot-value ret 'clails/model/base-model::columns)
                                           do (let ((name (getf column :name))
                                                    (access (getf column :access))
                                                    (fn (getf column :db-cl-fn)))
                                                (setf (ref ret name)
                                                      (funcall fn (getf row access)))))
                                     ret))))))
    ;; TODO: debug
    (format t "debug: query: ~A~%" (getf select :query))
    (format t "debug: params: ~A~%" (getf select :params))
    ;; TODO: get current thread connection
    (if connection
        (funcall body connection)
        (clails/model/connection:with-db-connection (connection)
          (funcall body connection)))))

(defmethod save ((inst <base-model>) &key connection)
  (if (ref inst :id)
      (update1 inst :connection connection)
      (insert1 inst :connection connection)))

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
        when (or (and insert
                      (not (eq (getf column :name) :id)))
                 (and update
                      (or (not (eq (getf column :name) :id)))
                          (not (eq (getf column :name) :created-at)))
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
  (let (x y)
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
    (format NIL "~A ~A ~A" x op y)))

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
   ;; TODO: use *table-information* instead of make-instance
   (inst :type 'clails/model/base-model::<base-model>)))

(defclass <join-query> (<query>)
  ((join-type :initarg :join-type
              :type keyword)))


(defmethod initialize-instance :after ((q <query>) &rest initargs)
  (declare (ignore initargs))
  (let ((inst (make-instance (slot-value q 'model))))
    (setf (slot-value q 'inst) inst)

    (unless (slot-value q 'alias)
      (setf (slot-value q 'alias)
            (slot-value inst 'clails/model/base-model::table-name)))))


(defmacro query (model &key as columns joins where order-by limit offset)
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
                         ;; join-spec (:inner-join <user> :as user :on ....)
                         (let ((join-type (first join-spec))
                               (details (rest join-spec)))
                           ;; details -> (<user> :as user :on ...)
                           (destructuring-bind (join-model &key as on) details
                             ;; make `(list :inner-join (make-instance...))`
                             `(list ',join-type
                                    (make-instance '<join-query>
                                                   :join-type ',join-type
                                                   :model ',join-model
                                                   :alias ',as
                                                   :where ',on)))))
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



(defmethod generate-query ((query <query>))
  (let ((table-name (slot-value (slot-value query 'inst) 'clails/model/base-model::table-name))
        (alias (slot-value query 'alias))
        (columns (mapcar #'column-pair-to-name
                         (generate-query-columns query)))
        (joins-keywords (loop for join in (slot-value query 'joins)
                              with joins and keywords
                              do (multiple-value-bind (j kw)
                                     (generate-join (cadr join))
                                   (push j joins)
                                   (push kw keywords))
                              finally (return (list :joins (nreverse joins)
                                                    :keywords  (flatten (nreverse keywords))))))
        (where-keywords (multiple-value-bind (w kw)
                            (parse-where-claude (slot-value query 'where))
                          (list :where w
                                :keywords kw)))
        (order-by (generate-order-by (slot-value query 'order-by)))
        (offset (generate-offset (slot-value query 'offset)))
        (limit (generate-limit (slot-value query 'limit))))

    (list :query (format nil "SELECT ~{~A~^, ~} FROM ~A as ~A~@[ ~{~A~^ ~}~]~@[ WHERE ~A~]~@[ ORDER BY ~{~A~^, ~}~]~@[ LIMIT ~A~]~@[ OFFSET ~A~]"
                         columns
                         table-name
                         alias
                         (getf joins-keywords :joins)
                         (getf where-keywords :where)
                         order-by
                         (getf limit :limit)
                         (getf offset :offset))
          :keywords (append (ensure-list (getf joins-keywords :keywords))
                            (ensure-list (getf where-keywords :keywords))
                            (ensure-list (getf limit :keyword))
                            (ensure-list (getf offset :keyword))))))


(defun generate-values (named-params named-values)
  "named-params: '(:start-date :end-date :start-date)
   named-values: '(:start-date \"2025-01-01\" :end-date \"2025-12-31\")

   returns -> '(\"2025-01-01\" \"2025-12-31\" \"2025-01-01\")"
  (loop for key in named-params
        collect (getf named-values key)))


(defmethod generate-query-columns ((query <query>))
  (if (slot-value query 'columns)
      (slot-value query 'columns)
      (mapcan #'(lambda (q)
                  (fetch-columns2 q))
              (append
               (ensure-list query)
               (mapcar #'(lambda (j)
                           ;; j -> (:inner-join (make-instance '<subquery> ...))
                           (cadr j))
                       (slot-value query 'joins))))))


;; TODO: rename
(defmethod fetch-columns2 ((query <query>))
  (let ((inst (slot-value query 'inst))
        (alias (slot-value query 'alias)))
    (loop for column in (slot-value inst 'clails/model/base-model::columns)
          collect (list alias (getf column :name)))))


(defmethod generate-join ((join-query <join-query>))
  (let ((join-type (slot-value join-query 'join-type))
        (table-name (slot-value (slot-value join-query 'inst) 'clails/model/base-model::table-name))
        (alias (kebab->snake (slot-value join-query 'alias))))
    (multiple-value-bind (where-claude keywords)
        (parse-where-claude (slot-value join-query 'where))
      (values
       (format nil "~A ~A as ~A on ~A"
               (cond ((eq join-type :inner-join) "INNER JOIN")
                     ((eq join-type :left-join) "LEFT JOIN")
                     (t (error "not implement:~A" join-type)))
               table-name
               alias
               where-claude)
       keywords))))


(defun parse-where-claude (where)
  (let ((elm (car where)))
    (cond ((find elm '(= < <= > >= <> !=))
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
                                         keywords-list))))
          ((eq elm :or)
           (loop for expression in (cdr where)
                 with exp and keywords
                 do (multiple-value-setq (exp keywords) (parse-where-claude expression))
                 collect exp into exp-list
                 when keywords
                   collect keywords into keywords-list
                 finally (return (values (format nil "(~{~A~^ OR ~})" exp-list)
                                         keywords-list))))
          (t
           (error "where claude: parse error `~A`" elm)))))

;; TODO: rename
(defun parse-exp2 (op exp)
  (let (x y keywords)
    (multiple-value-bind (col1 param1)
        (lexer (car exp))
      (setf x col1)
      (when param1
        (appendf keywords (ensure-list param1))))
    (multiple-value-bind (col2 param2)
        (lexer (cadr exp))
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

(defun column-pair-to-name (pair)
  (format nil "~A.~A" (kebab->snake (car pair)) (kebab->snake (cadr pair))))


(defun generate-order-by (params &optional order)
  "((blog :star :desc)
    (blog :id))
   => (\"BLOG.STAR DESC\" \"BLOG.ID\")"
  (flet ((generate (p)
           (when (not (and (symbolp (first p))
                           (keywordp (second p))))
             (error "parse error: order-by: expect (symbol :keyword) but got ~S" p))
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
