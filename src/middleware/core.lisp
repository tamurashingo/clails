(in-package #:cl-user)
(defpackage #:clails/middleware/core
  (:use #:cl)
  (:import-from #:lack.middleware.static
                #:*lack-middleware-static*)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:export #:*clails-middleware-stack*
           #:add-middleware-before
           #:add-middleware-after
           #:show-middleware-stack))
(in-package #:clails/middleware/core)

(defparameter *clails-middleware-stack* (list
                                          *lack-middleware-clails-controller*
                                          #'(lambda (app)
                                              (funcall *lack-middleware-static*
                                                       app
                                                       :path "/"
                                                       :root #P"./public/")))
  "A list of middleware functions to be applied to the application.
   Each middleware function should take a single argument,
   which is the next application in the stack,
   and return a new application that wraps the next one.
   see: https://github.com/fukamachi/lack ")


(defun add-middleware-before (middleware)
  "Add middleware at the beginning of the stack.
   
   The middleware will be executed before all existing middleware.
   
   @param middleware [function] Middleware function to add
   "
  (setf *clails-middleware-stack*
        (cons middleware *clails-middleware-stack*)))

(defun add-middleware-after (middleware)
  "Add middleware at the end of the stack.
   
   The middleware will be executed after all existing middleware.
   
   @param middleware [function] Middleware function to add
   "
  (setf *clails-middleware-stack*
        (append *clails-middleware-stack*
                (list middleware))))

(defun show-middleware-stack (&optional (stream T))
  "Display the current middleware stack.
   
   @param stream [stream] Output stream (default: *standard-output*)
   "
  (dolist (mw *clails-middleware-stack*)
    (format stream "~A~%" mw)))
