(in-package #:cl-user)
(defpackage #:clails/controller/base-controller
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:clails/condition
                #:404/not-found)
  (:import-from #:clails/environment
                #:*routing-tables*
                #:*project-dir*)
  (:import-from #:clails/logger
                #:log.web-access
                #:log-package.trace
                #:log-package.debug
                #:log-level-enabled-p)
  (:export #:<base-controller>
           #:<web-controller>
           #:<rest-controller>
           #:request
           #:env
           #:code
           #:header
           #:params
           #:view
           #:view-data
           #:set-view
           #:set-redirect
           #:set-response
           #:do-get
           #:do-post
           #:do-put
           #:do-delete
           #:initialize-routing-tables
           #:create-scanner-from-uri-path
           #:path-controller
           #:<default-controller>
           #:lisp-type
           #:lisp-version))


(in-package #:clails/controller/base-controller)

(defclass <base-controller> ()
  ((request :reader request)
   (env :reader env)
   (code :initform 200
         :reader code)
   (header :initform '()
           :reader header)
   (params :initform (make-hash-table :test 'equal)
           :reader params)))

(defmethod initialize-instance :after ((c <base-controller>) &rest initargs)
  "set default values"
  (declare (ignore initargs))
  (setf (slot-value c 'code) 200))


(defclass <web-controller> (<base-controller>)
  ((view :accessor view)
   (view-data :initform nil
              :accessor view-data)))

(defmethod initialize-instance :after ((c <web-controller>) &rest initargs)
  (declare (ignore initargs))
  (let ((header (header c)))
    (setf (slot-value c 'header) (append header '(:content-type "text/html")))))


(defclass <rest-controller> (<base-controller>)
  ((resopnse :accessor response)))


(defmethod param ((controller <base-controller>) key)
  (gethash key (slot-value controller 'params)))

(defgeneric do-get (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-get called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-post (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-post called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-put (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-put called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-delete (controller)
  (:documentation "")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-delete called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))


(defmethod set-view ((controller <web-controller>) viewname &optional data)
  "Set view template and optional data for rendering.
   viewname: template file name (e.g., \"users/show.html\")
   data: plist of data to pass to template (e.g., '(:user user :todos todos))"
  (setf (view controller) (merge-pathnames (format nil "app/views/~A" viewname)
                                           *project-dir*))
  (setf (view-data controller) data))


(defmethod set-redirect ((controller <web-controller>) path)
  (let* ((redirect-url (if (ppcre:scan "^https?://.*" path)
                           path
                           (let* ((env (env controller))
                                  (uri (quri:make-uri :scheme (getf env :url-scheme)
                                                      :host (getf env :server-name)
                                                      :port (getf env :server-port)
                                                      :path path)))
                             (quri:render-uri uri))))
         (header (header controller)))
    (setf (slot-value controller 'header)
          `(:content-type "text/html"
            :content-length 0
            :location ,redirect-url))
    (setf (slot-value controller 'code)
          302))
  (setf (view controller) nil))

(defmethod set-response ((controller <rest-controller>) alist)
  (setf (response controller) alist))


(defclass <default-controller> (<web-controller>)
  ((lisp-type :accessor lisp-type)
   (lisp-version :accessor lisp-version)))

(defmethod do-get ((controller <default-controller>))
  (format t "default-controller:do-get~%")
  (setf (lisp-type controller) (lisp-implementation-type))
  (setf (lisp-version controller) (lisp-implementation-version))
  (setf (view controller) (asdf:system-relative-pathname :clails "template/index.html")))


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

