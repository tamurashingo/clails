(in-package #:cl-user)
(defpackage #:clails/controller/base-controller
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:clails/condition
                #:404/not-found)
  (:export #:<base-controller>
           #:get-all
           #:get-one
           #:post-one
           #:put-one
           #:delete-one
           #:create-scanner-from-uri-path))


(in-package #:clails/controller/base-controller)

(defclass <base-controller> ()
  ((request :reader request)
   (env :reader env)
   (view :accessor view)))

(defgeneric get-all (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric get-one (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric post-one (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric put-one (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric delete-one (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))



(defclass <default-controller> (<base-controller>)
  ())

(defmethod get-all ((controller <default-controller>))
  (setf (view controller) "index.html"))

#|
(defparameter clails/environment:*routing-tables*
  '((:path "/"
     :controller "todoapp/controller/todo-controller:<todo-controller>"
     :type :all)
    (:path "/todo/:id"
     :controller "todoapp/controller/todo-controller:<todo-controller>"
     :type :one)))

(defparameter *router* nil)
|#


(defparameter *router* nil)

(defun initialize-routing-tables ()
  (setf *router*
        (loop for tbl in *tables*
              collect(append tbl
                             (create-scanner-from-uri-path (getf tbl :path))))))


(defun create-scanner-from-uri-path (path)
  "return regex string, parameter names"
  (let ((scanner (list #\^))
        (params))
    (%create-scanner-normal path 0 (length path) scanner params)))


(defun %create-scanner-normal (path pos len scanner params)
  (if (= pos len)
      (progn
        (push #\$ scanner)
        `(:scanner ,(coerce (nreverse scanner) 'string)
          :params ,params))
      (let ((c (aref path pos)))
        (cond ((eq c #\:)
               (push #\( scanner)
               (setf scanner (append (nreverse (coerce "[0-9A-Za-z\\-._~%]+" 'list))
                                     scanner))
               (%create-scanner-read-id path (1+ pos) len scanner params))
              (t
               (when (find c ".")
                 (push #\\ scanner))
               (push c scanner)
               (%create-scanner-normal path (1+ pos) len scanner params))))))


(defun %create-scanner-read-id (path pos len scanner params &optional reading-params)
  (if (= pos len)
      (progn
        (push (intern (string-upcase (coerce (nreverse reading-params)
                                             'string))
                      :KEYWORD)
              params)
        (push #\) scanner)
        (push #\$ scanner)
        `(:scanner ,(coerce (nreverse scanner) 'string)
          :params ,(nreverse params)))
      (let ((c (aref path pos)))
        (cond ((find c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-._~%")
               (%create-scanner-read-id path (1+ pos) len scanner params (push c reading-params)))
              (t
               (eq c #\/)
               (push #\) scanner)
               (push c scanner)
               (push (intern (string-upcase (coerce (nreverse reading-params)
                                                    'string))
                             :KEYWORD)
                     params)
               (%create-scanner-normal path (1+ pos) len scanner params))))))

