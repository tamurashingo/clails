(in-package #:cl-user)
(defpackage #:clails/model/connection
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*)
  (:export #:with-db-connection))
(in-package #:clails/model/connection)

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
  '())

(defun disconnect (connection)
  '())

(defmacro with-db-connection (connection &body body)
  `(let ((,connection (get-connection)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect ,connection))))
