(in-package #:cl-user)
(defpackage #:clails/model/query
  (:use #:cl)
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


