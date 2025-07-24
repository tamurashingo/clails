(in-package #:cl-user)
(defpackage #:clails/controller/base-controller
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:clails/condition
                #:404/not-found)
  (:import-from #:clails/environment
                #:*routing-tables*)
  (:export #:<base-controller>
           #:<web-controller>
           #:<rest-controller>
           #:request
           #:env
           #:params
           #:view
           #:do-get
           #:do-post
           #:do-put
           #:do-delete
           #:initialize-routing-tables
           #:create-scanner-from-uri-path
           #:path-controller
           #:<default-controller>))


(in-package #:clails/controller/base-controller)

(defclass <base-controller> ()
  ((request :reader request)
   (env :reader env)
   (params :initform (make-hash-table :test 'equal)
           :reader params)))

(defclass <web-controller> (<base-controller>)
  ((view :accessor view)))

(defclass <rest-cotnroller> (<base-controller>)
  ((resopnse :accessor response)))


(defmethod param ((controller <base-controller>) key)
  (gethash key (slot-value controller 'params)))

(defgeneric do-get (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-post (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-put (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-delete (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))



(defclass <default-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <default-controller>))
  (format t "default-controller:do-get~%")
  (setf (view controller) "index.html"))

#|
(defparameter clails/environment:*routing-tables*
  '((:path "/"
     :controller "todoapp/controller/todo-controller:<todo-controller>"
     :type :all)
    (:path "/todo/:id"
     :controller "todoapp/controller/todo-controller:<todo-controller>"
     :type :one)))

|#

(defparameter *router* nil)


(defun path-controller (path)
  (loop for r in *router*
        do (multiple-value-bind (match regs)
               (ppcre:scan-to-strings (getf r :scanner) path)
             (when match
               (return (append r
                               `(:parameters ,regs)))))))


(defun initialize-routing-tables ()
  (setf *router*
        (loop for tbl in *routing-tables*
              collect(append tbl
                             (create-scanner-from-uri-path (getf tbl :path))))))


(defun create-scanner-from-uri-path (path)
  "return regex string, parameter names"
  (let ((scanner (list #\^))
        (keys))
    (%create-scanner-normal path 0 (length path) scanner keys)))


(defun %create-scanner-normal (path pos len scanner keys)
  (if (= pos len)
      (progn
        (push #\$ scanner)
        `(:scanner ,(coerce (nreverse scanner) 'string)
          :keys ,keys))
      (let ((c (aref path pos)))
        (cond ((eq c #\:)
               (push #\( scanner)
               (setf scanner (append (nreverse (coerce "[0-9A-Za-z\\-._~%]+" 'list))
                                     scanner))
               (%create-scanner-read-id path (1+ pos) len scanner keys))
              (t
               (when (find c ".")
                 (push #\\ scanner))
               (push c scanner)
               (%create-scanner-normal path (1+ pos) len scanner keys))))))


(defun %create-scanner-read-id (path pos len scanner keys &optional reading-key)
  (if (= pos len)
      (progn
        (push (string-downcase (coerce (nreverse reading-key)
                                       'string))
              keys)
        (push #\) scanner)
        (push #\$ scanner)
        `(:scanner ,(coerce (nreverse scanner) 'string)
          :keys ,(nreverse keys)))
      (let ((c (aref path pos)))
        (cond ((find c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-._~%")
               (%create-scanner-read-id path (1+ pos) len scanner keys (push c reading-key)))
              (t
               (eq c #\/)
               (push #\) scanner)
               (push c scanner)
               (push (string-downcase (coerce (nreverse reading-key)
                                                    'string))
                     keys)
               (%create-scanner-normal path (1+ pos) len scanner keys))))))

