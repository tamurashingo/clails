(in-package #:cl-user)
(defpackage #:clails/model/connection
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*
                #:*connection-pool*)
  (:export #:startup-connection-pool
           #:shutdown-connection-pool
           #:create-connection-pool-impl
           #:with-db-connection))
(in-package #:clails/model/connection)

(defun startup-connection-pool ()
  (when (null *connection-pool*)
    (setf *connection-pool* (create-connection-pool-impl *database-type*))))

(defun shutdown-connection-pool ()
  (when *connection-pool*
    (dbi-cp:shutdown *connection-pool*)
    (setf *connection-pool* nil)))

(defgeneric create-connection-pool-impl (database-type)
  (:documentation "Create connection pool."))


(defun get-connection-direct (&key (no-database nil))
  (get-connection-direct-impl *database-type* :no-database no-database))

(defgeneric get-connection-direct-impl (database-type &key)
  (:documentation "Get connection to database."))

(defun disconnect-direct (connection)
  (dbi:disconnect connection))

(defmacro with-db-connection-direct ((connection &key (no-database nil)) &body body)
  `(let ((,connection (get-connection-direct :no-database ,no-database)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect-direct ,connection))))


(defun get-connection ()
  (dbi-cp:get-connection *connection-pool*))

(defun disconnect (connection)
  (dbi-cp:disconnect connection))

(defmacro with-db-connection ((connection) &body body)
  `(let ((,connection (get-connection)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect ,connection))))
