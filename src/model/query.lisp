(in-package #:cl-user)
(defpackage #:clails/model/query
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:ref)
  (:import-from #:clails/util
                #:kebab->snake)
  (:export #:select))
(in-package #:clails/model/query)



(defun select (model-name &key where order-by)
  "Given a model name, returns instances that match the conditions
(select '<todo>) => (#<TODO> #<TODO> #<TODO>)
(select '<todo> :where '(= id 1)) => (#<TODO>)
(select '<todo> :where '(or (= id 1)
                            (= done false))) => (#<TODO> #<TODO>)
(select '<todo> :order-by '(id))
(select '<todo> :order-by '((id :desc) (created-at :asc)))
"
  (let* ((inst (make-instance model-name))
         (select (generate-select-query inst where order-by)))
    ;; TODO: debug
    (format t "debug: query: ~A~%" (getf select :query))
    (format t "debug: params: ~A~%" (getf select :params))
    ;; TODO: get current thread connection
    (clails/model/connection:with-db-connection (connection)
      (let ((result (dbi-cp:fetch-all
                      (dbi-cp:execute
                        (dbi-cp:prepare connection (getf select :query))
                        (getf select :params)))))
        (loop for row in result
              collect (let ((ret (make-instance model-name)))
                         (loop for col in (slot-value ret 'clails/model/base-model::columns)
                               do (setf (ref ret col)
                                        (getf row (intern (kebab->snake (string col)) :KEYWORD))))
                          ret))))))


(defun generate-select-query (inst where order-by)
  (let* ((params (make-array (length where)
                             :fill-pointer 0))
         (table-name (kebab->snake (slot-value inst 'clails/model/base-model::table-name)))
         (columns (fetch-columns inst))
         (conditions (if where
                         (parse-where where params)
                         nil))
         (sort (if order-by
                   (parse-order-by order-by)
                   nil)))
    (list :query (format NIL "SELECT ~{~A~^, ~} FROM ~A ~@[ WHERE ~A ~] ~@[ ORDER BY ~{~{~A~^ ~}~^, ~}~]" columns table-name conditions sort)
          :params (coerce params 'list))))


(defun fetch-columns (inst)
  (loop for col in (slot-value inst 'clails/model/base-model::columns)
        collect (kebab->snake (string col))))


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

