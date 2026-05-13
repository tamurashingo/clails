(in-package #:cl-user)
(defpackage #:clails/middleware/clails-middleware
  (:use #:cl)
  (:import-from #:lack/request
                #:request-method
                #:request-path-info)
  (:import-from #:clails/controller/base-controller
                #:path-controller
                #:do-get
                #:do-post
                #:do-put
                #:do-delete
                #:request
                #:params)
  (:import-from #:clails/view/view-resolver
                #:resolve-view)
  (:import-from #:clails/condition
                #:404/not-found)
  (:import-from #:clails/controller/error-handle-controller
                #:<error-handle-controller>)
  (:import-from #:clails/logger
                #:log.web-access
                #:log-package.trace
                #:log-level-enabled-p)
  (:export #:*lack-middleware-clails-controller*))

(in-package #:clails/middleware/clails-middleware)

(defparameter *lack-middleware-clails-controller*
  (lambda (app)
    "Clails controller middleware for routing and dispatching requests.
     
     Routes requests to appropriate controllers based on URL patterns,
     handles HTTP method dispatching (GET/POST/PUT/DELETE), and
     resolves views for rendering.
     "
    (lambda (env)
      (handler-case
          (let* ((controller (make-controller env))
                 (method (request-method (request controller)))
                 (action (slot-value controller 'clails/controller/base-controller::action)))
            (when (log-level-enabled-p :trace)
              (log-package.trace (format nil "Request path: ~A" (getf env :path-info))))
            
            ;; If action is specified, call the action method
            (if action
                (progn
                  (when (log-level-enabled-p :trace)
                    (log-package.trace (format nil "Dispatching to action: ~A" action)))
                  (call-action controller action))
                ;; Otherwise, dispatch to do-* methods (backward compatibility)
                (progn
                  (cond ((eq method :get)
                         (when (log-level-enabled-p :trace)
                           (log-package.trace "Dispatching to do-get"))
                         (do-get controller))
                        ((or (eq method :put)
                             (and (eq method :post)
                                  (string-equal "put"
                                           (gethash "_method" (params controller)))))
                         (when (log-level-enabled-p :trace)
                           (log-package.trace "Dispatching to do-put"))
                         (do-put controller))
                        ((or (eq method :delete)
                             (and (eq method :post)
                                  (string-equal "delete"
                                           (gethash "_method" (params controller)))))
                         (when (log-level-enabled-p :trace)
                           (log-package.trace "Dispatching to do-delete"))
                         (do-delete controller))
                        ((eq method :post)
                         (when (log-level-enabled-p :trace)
                           (log-package.trace "Dispatching to do-post"))
                         (do-post controller))
                        (t
                         (when (log-level-enabled-p :trace)
                           (log-package.trace (format nil "Unsupported method: ~A" method)))
                         nil))))
            (resolve-view controller))
        (404/not-found (c)
          (funcall app env)))))
  "Lack middleware function for Clails controller routing and dispatching.")


(defun call-action (controller action-name)
  "Call the specified action method on the controller.

   @param controller [<base-controller>] Controller instance
   @param action-name [string] Action name (e.g., \"index\", \"show\")
   @condition 404/not-found When action method does not exist
   "
  (let* ((action-symbol (intern (string-upcase action-name)
                                (symbol-package (class-name (class-of controller)))))
         (method (find-method (symbol-function action-symbol) 
                             nil 
                             (list (class-of controller)) 
                             nil)))
    (unless method
      (error '404/not-found
             :path (lack/request:request-path-info (request controller))))
    (funcall action-symbol controller)))


(defun make-controller (env)
  "Create and initialize controller instance for the request.
   
   Finds the appropriate controller based on the request path and HTTP method,
   initializes it with request parameters (both from URL and path),
   and sets up the request, environment, and action.
   
   @param env [plist] Lack environment containing request information
   @return [<base-controller>] Initialized controller instance
   @condition 404/not-found Signaled when no matching route found
   "
  (let* ((path-info (getf env :path-info))
         (req (lack/request:make-request env))
         (http-method (lack/request:request-method req))
         (controller-info (path-controller path-info http-method)))
    (when (null controller-info)
      (error '404/not-found
             :path path-info))
    (let* ((controller (make-instance (read-from-string (getf controller-info :controller))))
           (param-hash (slot-value controller 'clails/controller/base-controller::params)))

      ;; request parameter
      (loop for req-param in (lack/request:request-parameters req)
            do (setf (gethash (car req-param) param-hash) (cdr req-param)))

      ;; path parmeter
      (loop for path-param in (getf controller-info :keys)
            for idx from 0
            do (setf (gethash path-param param-hash) (aref (getf controller-info :parameters) idx)))

      ;; Set action if specified in route
      (let ((action (getf controller-info :action)))
        (when action
          (setf (slot-value controller 'clails/controller/base-controller::action) action)))

      (setf (slot-value controller 'clails/controller/base-controller::request) req)
      (setf (slot-value controller 'clails/controller/base-controller::env) env)

      controller)))
