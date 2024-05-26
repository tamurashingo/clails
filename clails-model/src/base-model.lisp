(in-package #:cl-user)
(defpackage #:clails-model/base-model
  (:use #:cl))
(in-package #:clails-model/base-model)


(defclass <base-model> ()
  ((id
    :initarg :id
    :accessor id)
   (created-at
    :initarg :created-at
    :accessor created-at)
   (updated-at
    :initarg :updated-at
    :accessor updated-at)))


(defun select (model-name &key where order-by)
  (multiple-value-bind (query params)
      (generate-select model-name where order-by)
    ;; prepare query
    ;; execute params
    (values query params)))


(defun generate-select (model-name where order-by)
  (let* ((params (make-array (length where)
                             :fill-pointer 0))
         (table-name (kebab->snake model-name))
         (columns (fetch-slots model-name))
         (conditions (if where
                         (parse-where where params)
                         nil)))
    (values (format NIL "SELECT ~{~A~^, ~} FROM ~A ~@[ WHERE ~A ~]" columns table-name conditions)
            params)))


(defun fetch-slots (model-name)
  (mapcar #'kebab->snake
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of (make-instance model-name))))))

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

(defun kebab->snake (sym)
  "convert KEBAB-CASE to SNAKE_CASE"
  (assert (and (symbolp sym)
               (not (keywordp sym))))
  (read-from-string (ppcre:regex-replace-all "-" (string sym) "_")))

(defun lexer (param)
  (cond ((or (stringp param)
             (numberp param))
         (values "?" param))
        ((and (symbolp param)
              (not (keywordp param)))
         (values (kebab->snake param) NIL))
        (t (values param NIL))))


