(in-package #:cl-user)
(defpackage #:clails/middleware/transaction-middleware
  (:use #:cl)
  (:import-from #:clails/model/connection
                #:get-connection
                #:release-connection)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log-package.trace)
  (:export #:*lack-middleware-transaction*
           #:*enable-transaction-middleware*))
(in-package #:clails/middleware/transaction-middleware)


(defparameter *enable-transaction-middleware* t
  "Flag to enable/disable transaction middleware.
   
   When T (default), all requests are wrapped in transactions.
   Set to NIL to disable automatic transaction management.")


(defparameter *lack-middleware-transaction*
  (lambda (app)
    "Transaction middleware for managing database transactions per request.
     
     Acquires a connection and wraps the request in a transaction using dbi-cp:with-transaction.
     Commits the transaction on successful completion or rolls back on error.
     Ensures the connection is always released back to the pool.
     "
    (lambda (env)
      (if (not *enable-transaction-middleware*)
          ;; Transaction middleware disabled, just call the app
          (funcall app env)
          ;; Transaction middleware enabled
          (let ((connection nil))
            (unwind-protect
                (progn
                  (when (log-level-enabled-p :trace)
                    (log-package.trace "Transaction middleware: acquiring connection"))
                  
                  ;; Acquire connection
                  (setf connection (get-connection))
                  
                  (when (log-level-enabled-p :trace)
                    (log-package.trace "Transaction middleware: starting transaction"))
                  
                  ;; Execute the application within a transaction
                  (dbi-cp:with-transaction connection
                    (when (log-level-enabled-p :trace)
                      (log-package.trace "Transaction middleware: executing application"))
                    
                    (funcall app env)))
              
              ;; Always release the connection
              (when connection
                (when (log-level-enabled-p :trace)
                  (log-package.trace "Transaction middleware: releasing connection"))
                (release-connection connection)))))))
  "Lack middleware function for automatic transaction management.")
