(in-package #:cl-user)
(defpackage #:clails/middleware/clails-middleware
  (:use #:cl)
  (:import-from #:lack/request
                #:request-method)
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
    (lambda (env)
      (handler-case
          (let* ((controller (make-controller env))
                 (method (request-method (request controller))))
            (when (log-level-enabled-p :trace)
              (log-package.trace (format nil "Request path: ~A" (getf env :path-info))))
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
                   nil))
            (resolve-view controller))
        (404/not-found (c)
          (funcall app env))))))


(defun make-controller (env)
  (let* ((path-info (getf env :path-info))
         (req (lack/request:make-request env))
         (controller-info (path-controller path-info)))
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

      (setf (slot-value controller 'clails/controller/base-controller::request) req)
      (setf (slot-value controller 'clails/controller/base-controller::env) env)

      controller)))
