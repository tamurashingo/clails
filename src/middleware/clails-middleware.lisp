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
                #:request)
  (:import-from #:clails/view/view-resolver
                #:resolve-view)
  (:import-from #:clails/condition
                #:404/not-found)
  (:import-from #:clails/controller/error-handle-controller
                #:<error-handle-controller>)
  (:export #:*lack-middleware-clails-controller*))

(in-package #:clails/middleware/clails-middleware)

(defparameter *lack-middleware-clails-controller*
  (lambda (env)
    (handler-bind ((404/not-found
                     (lambda (c)
                       (let ((controller (make-instance '<error-handle-controller>
                                                        :exception c)))
                         (resolve-view controller)))))
      (let* ((controller (make-controller env))
             (method (request-method (request controller))))
        (cond ((eq method :get)
               (do-get controller))
              ((eq method :post)
               (do-post controller))
              ((eq method :put)
               (do-put controller))
              ((eq method :delete)
               (do-delete controller))
              (t
               nil))
        (resolve-view controller)))))



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
            do (format t "path-param:~S~%" path-param)
            do (setf (gethash path-param param-hash) (aref (getf controller-info :parameters) idx)))

      (setf (slot-value controller 'clails/controller/base-controller::request) req)
      (setf (slot-value controller 'clails/controller/base-controller::env) env)

      controller)))
