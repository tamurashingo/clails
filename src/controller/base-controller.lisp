(in-package #:cl-user)
(defpackage #:clails/controller/base-controller
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:clails/condition
                #:404/not-found)
  (:import-from #:clails/environment
                #:*routing-tables*
                #:*project-dir*
                #:*project-name*)
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
           #:view-package
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
  ((request :reader request
            :documentation "HTTP request object")
   (env :reader env
        :documentation "Environment variables from request")
   (code :initform 200
         :reader code
         :documentation "HTTP response status code")
   (header :initform '()
           :reader header
           :documentation "HTTP response headers as plist")
   (params :initform (make-hash-table :test 'equal)
           :reader params
           :documentation "Request parameters hash table"))
  (:documentation "Base controller class for handling HTTP requests."))

(defmethod initialize-instance :after ((c <base-controller>) &rest initargs)
  "Set default HTTP status code to 200 after initialization.

   @param c [<base-controller>] Controller instance being initialized
   @param initargs [list] Initialization arguments (ignored)
   "
  (declare (ignore initargs))
  (setf (slot-value c 'code) 200))


(defclass <web-controller> (<base-controller>)
  ((view :accessor view
         :documentation "View template pathname")
   (view-data :initform nil
              :accessor view-data
              :documentation "Data to be passed to view template")
   (view-package :accessor view-package
                 :initform nil
                 :documentation "Package name for view rendering"))
  (:documentation "Controller class for web applications that render HTML views."))

(defmethod initialize-instance :after ((c <web-controller>) &rest initargs)
  "Set default content-type header to text/html after initialization.

   @param c [<web-controller>] Controller instance being initialized
   @param initargs [list] Initialization arguments (ignored)
   "
  (declare (ignore initargs))
  (let ((header (header c)))
    (setf (slot-value c 'header) (append header '(:content-type "text/html")))))


(defclass <rest-controller> (<base-controller>)
  ((resopnse :accessor response
             :documentation "Response data for REST API"))
  (:documentation "Controller class for REST API endpoints that return structured data."))


(defmethod param ((controller <base-controller>) key)
  "Get a parameter value from the controller's params hash table.

   @param controller [<base-controller>] Controller instance
   @param key [string] Parameter key
   @return [t] Parameter value, or NIL if not found
   "
  (gethash key (slot-value controller 'params)))

(defgeneric do-get (controller)
  (:documentation "Handle HTTP GET request.

   Default implementation signals a 404/not-found error.
   Override this method to implement GET request handling.

   @param controller [<base-controller>] Controller instance
   @condition 404/not-found Signaled when not overridden
   ")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-get called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-post (controller)
  (:documentation "Handle HTTP POST request.

   Default implementation signals a 404/not-found error.
   Override this method to implement POST request handling.

   @param controller [<base-controller>] Controller instance
   @condition 404/not-found Signaled when not overridden
   ")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-post called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-put (controller)
  (:documentation "Handle HTTP PUT request.

   Default implementation signals a 404/not-found error.
   Override this method to implement PUT request handling.

   @param controller [<base-controller>] Controller instance
   @condition 404/not-found Signaled when not overridden
   ")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-put called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))

(defgeneric do-delete (controller)
  (:documentation "Handle HTTP DELETE request.

   Default implementation signals a 404/not-found error.
   Override this method to implement DELETE request handling.

   @param controller [<base-controller>] Controller instance
   @condition 404/not-found Signaled when not overridden
   ")
  (:method ((controller <base-controller>))
    (when (log-level-enabled-p :trace)
      (log-package.trace "do-delete called"))
    (error '404/not-found
           :path (getf (getf controller :request)
                       :path-info))))


(defmethod set-view ((controller <web-controller>) viewname &optional data)
  "Set view template and optional data for rendering.
   
   Sets the view template file path and data to be passed to the template.
   The view package is automatically resolved based on the view path.
   
   @param controller [<web-controller>] Controller instance
   @param viewname [string] Template file name relative to app/views/ (e.g., \"users/show.html\")
   @param data [plist] Data to pass to template (e.g., '(:user user :todos todos))
   @param data [nil] No data to pass (template can still access controller)
   "
  (setf (view controller) (merge-pathnames (format nil "app/views/~A" viewname)
                                           *project-dir*))
  (setf (view-data controller) data)
  (setf (view-package controller) (resolve-view-package viewname)))


(defun resolve-view-package (viewname)
  "Resolve package name from view file path using central *project-name*.

   Converts view path to package name following the convention:
   - \"index.html\" -> :{project}/views/package
   - \"todo/show.html\" -> :{project}/views/todo/package
   - \"admin/user/list.html\" -> :{project}/views/admin/user/package

   This implementation uses clails/environment:*project-name* instead of inferring
   the project name from *project-dir* to avoid failures when the directory path changes.

   @param viewname [string] View file path relative to app/views/
   @return [keyword] Package name as keyword
   "
  (let* ((project-name *project-name*)
         (path-parts (split-view-path viewname)))
    (make-keyword
      (if path-parts
          (format nil "~A/views/~{~A~^/~}/package" project-name path-parts)
          (format nil "~A/views/package" project-name)))))

(defun split-view-path (viewname)
  "Split view path and extract directory parts (excluding filename).

   @param viewname [string] View file path (e.g., \"todo/show.html\")
   @return [list] List of directory names (e.g., (\"todo\"))
   "
  (let* ((pathname (pathname viewname))
         (directory (pathname-directory pathname)))
    (when (and directory (eq (car directory) :relative))
      (cdr directory))))

(defun make-keyword (name)
  "Convert string to keyword.

   @param name [string] String to convert to keyword
   @return [keyword] Keyword
   "
  (intern (string-upcase name) :keyword))


(defmethod set-redirect ((controller <web-controller>) path)
  "Set HTTP redirect response.

   If path is an absolute URL (starting with http:// or https://), uses it directly.
   Otherwise, constructs a full URL using the request's scheme, host, and port.
   Sets HTTP status code to 302 and Location header.

   @param controller [<web-controller>] Controller instance
   @param path [string] Redirect path or absolute URL
   "
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
  "Set response data for REST API.

   @param controller [<rest-controller>] Controller instance
   @param alist [list] Association list of response data
   "
  (setf (response controller) alist))


(defparameter *router* nil
  "Global routing table holding compiled route scanners.")


(defun path-controller (path)
  "Find matching controller for the given request path.

   Iterates through *router* and returns the first route that matches the path,
   including captured URL parameters.

   @param path [string] Request path to match
   @return [plist] Route information with :scanner, :controller, :parameters, etc.
   @return [nil] NIL if no matching route found
   "
  (loop for r in *router*
        do (multiple-value-bind (match regs)
               (ppcre:scan-to-strings (getf r :scanner) path)
             (when match
               (return (append r
                               `(:parameters ,regs)))))))


(defun initialize-routing-tables ()
  "Initialize the routing table by compiling path patterns.

   Converts *routing-tables* entries into *router* by adding regex scanners
   for each route path pattern.
   "
  (setf *router*
        (loop for tbl in *routing-tables*
              collect(append tbl
                             (create-scanner-from-uri-path (getf tbl :path))))))


(defun create-scanner-from-uri-path (path)
  "Create a regex scanner and extract parameter names from a URI path pattern.

   Converts path patterns like \"/users/:id/posts/:post_id\" into regex scanners
   that can match actual request paths and extract parameter values.

   @param path [string] URI path pattern with :param-name placeholders
   @return [plist] Property list with :scanner (regex string) and :keys (parameter names)
   "
  (let ((scanner (list #\^))
        (keys))
    (%create-scanner-normal path 0 (length path) scanner keys)))


(defun %create-scanner-normal (path pos len scanner keys)
  "Process normal characters in path pattern.

   Internal helper for create-scanner-from-uri-path.

   @param path [string] URI path pattern
   @param pos [integer] Current position in path
   @param len [integer] Length of path
   @param scanner [list] Accumulator for regex characters
   @param keys [list] Accumulator for parameter names
   @return [plist] Property list with :scanner and :keys
   "
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
  "Read parameter name from path pattern.

   Internal helper for create-scanner-from-uri-path.

   @param path [string] URI path pattern
   @param pos [integer] Current position in path
   @param len [integer] Length of path
   @param scanner [list] Accumulator for regex characters
   @param keys [list] Accumulator for parameter names
   @param reading-key [list] Current parameter name being read
   @return [plist] Property list with :scanner and :keys
   "
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

