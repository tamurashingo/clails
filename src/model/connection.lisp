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


(defclass <connection> ()
  ((thread-id :initarg :thread-id)
   (connection :initarg :connection
               :reader connection
               :type dbi-cp.proxy::<dbi-connection-proxy>))
  (:documentation "#### Description:

Wrapper class for easily refrencing <dbi-connection-proxy> by thread-id
"))


(defparameter *thread-connection-pool*
  nil
  ;(make-hash-table :test #'eq)
  "#### Description:

**key** -> thread id\
**value** -> <connection>

Hashtable to manage connections bound to threads.
")


(defun find-connection-by-thread (thread)
  "#### Syntax:

**find-connection-by-thread** thread => <connection> | nil

#### Arguments and values:

*thread* -> thread \
*<connection>* -> connection

#### Description:

find connection bt thread from connection-
"
  (let ((thread-id (sb-thread::thread-os-thread thread)))
    (gethash thread-id *thread-connection-pool*)))


(defun get-connection-by-thread (thread)
  "#### Syntax:

**get-connection-by-thread** thread => <connection>

### Arguments and values:

*thread* -> thread \
*<connection>* -> connection
"

  (anaphora:aif (find-connection-by-thread thread)
                (connection anaphora:it)
                (let* ((connection (dbi-cp:get-connection *connection-pool*))
                       (thread-id (sb-thread::thread-os-thread thread))
                       (inst (make-instance '<connection>
                                            :thread-id thread-id
                                            :connection connection)))
                  (setf (gethash thread-id *thread-connection-pool*) inst)
                  connection)))

(defun collect-destroyed-thread-connection ()
  "#### Syntax:

**collect-destroyed-thread-connection** => any

#### Description:

Releases the connection bound to the terminated thread.
Execute every minute like garbage collection.
"
  (let ((all-threads (mapcar #'(lambda (th) (sb-thread::thread-os-thread th))
                             (bt:all-threads))))
    (maphash #'(lambda (key connection)
                 (when (not (find key all-threads))
                   (dbi-cp:disconnect connection)))
             *thread-connection-pool*)))



(defun startup-connection-pool ()
  (when (null *connection-pool*)
    (setf *thread-connection-pool* (make-hash-table :test #'eq))
    (setf *connection-pool* (create-connection-pool-impl *database-type*))))

(defun shutdown-connection-pool ()
  (when *connection-pool*
    (dbi-cp:shutdown *connection-pool*)
    (setf *thread-connection-pool* nil)
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
  (let ((current-th (bt:current-thread)))
    (get-connection-by-thread current-th)))


(defun disconnect (connection)
  (let ((thread-id (sb-thread::thread-os-thread (bt:current-thread))))
    (remhash thread-id *thread-connection-pool*)
    (dbi-cp:disconnect connection)))

(defmacro with-db-connection ((connection) &body body)
  `(let ((,connection (get-connection)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect ,connection))))
