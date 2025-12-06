(in-package #:cl-user)
(defpackage #:clails/model/connection
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*
                #:*connection-pool*
                #:*sqlite3-lock-module-loaded*)
  (:export #:startup-connection-pool
           #:shutdown-connection-pool
           #:create-connection-pool-impl
           #:with-db-connection
           #:get-connection
           #:release-connection
           #:<connection>))
(in-package #:clails/model/connection)


(defclass <connection> ()
  ((thread-id :initarg :thread-id
              :documentation "Operating system thread ID")
   (connection :initarg :connection
               :reader connection
               :type dbi-cp.proxy::<dbi-connection-proxy>
               :documentation "Database connection proxy"))
  (:documentation "Wrapper class for managing database connections bound to threads."))


(defparameter *thread-connection-pool*
  nil
  "Hash table managing connections bound to threads.

   Key: thread OS ID
   Value: <connection> instance")


(defun find-connection-by-thread (thread)
  "Find connection associated with the given thread.
   
   @param thread [thread] Thread object
   @return [<connection>] Connection instance if found
   @return [nil] NIL if no connection found for the thread
   "
  (let ((thread-id (sb-thread::thread-os-thread thread)))
    (gethash thread-id *thread-connection-pool*)))


(defun get-connection-by-thread (thread)
  "Get or create connection for the given thread.
   
   If a connection already exists for the thread, returns it.
   Otherwise, acquires a new connection from the pool and associates it with the thread.
   
   @param thread [thread] Thread object
   @return [dbi-cp.proxy::<dbi-connection-proxy>] Database connection
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
  "Release connections bound to terminated threads.
   
   Performs garbage collection of connections associated with threads
   that no longer exist. Should be executed periodically (e.g., every minute).
   "
  (let ((all-threads (mapcar #'(lambda (th) (sb-thread::thread-os-thread th))
                             (bt:all-threads))))
    (maphash #'(lambda (key connection)
                 (when (not (find key all-threads))
                   (dbi-cp:disconnect connection)))
             *thread-connection-pool*)))



(defun startup-connection-pool ()
  "Initialize the database connection pool.
   
   Creates the thread-connection hash table and connection pool
   if they don't already exist.
   For SQLite3, loads the lock module to enable pessimistic locking support.
   "
  (when (null *connection-pool*)
    (setf *thread-connection-pool* (make-hash-table :test #'eq))
    (setf *connection-pool* (create-connection-pool-impl *database-type*))

    ;; Load SQLite3 lock module if using SQLite3
    (when (and (typep *database-type* 'clails/environment:<database-type-sqlite3>)
               (not clails/environment:*sqlite3-lock-module-loaded*))
      (load (merge-pathnames "src/model/impl/sqlite3-lock.lisp"
                             (asdf:system-source-directory :clails)))
      (setf clails/environment:*sqlite3-lock-module-loaded* t))))

(defun shutdown-connection-pool ()
  "Shutdown the database connection pool.
   
   Closes all connections and clears the connection pools.
   "
  (when *connection-pool*
    (dbi-cp:shutdown *connection-pool*)
    (setf *thread-connection-pool* nil)
    (setf *connection-pool* nil)))

(defgeneric create-connection-pool-impl (database-type)
  (:documentation "Create connection pool for the specified database type.
   
   Implementation must be provided for each database type.
   
   @param database-type [<database-type>] Database type instance
   @return [dbi-cp:<connection-pool>] Connection pool instance
   "))


(defun get-connection-direct (&key (no-database nil))
  "Get a direct database connection (not from pool).
   
   @param no-database [boolean] If T, connect without specifying a database
   @return [dbi:<dbi-connection>] Direct database connection
   "
  (get-connection-direct-impl *database-type* :no-database no-database))

(defgeneric get-connection-direct-impl (database-type &key)
  (:documentation "Get direct connection to database for the specified type.
   
   Implementation must be provided for each database type.
   
   @param database-type [<database-type>] Database type instance
   @param no-database [boolean] If T, connect without specifying a database
   @return [dbi:<dbi-connection>] Direct database connection
   "))

(defun disconnect-direct (connection)
  "Disconnect a direct database connection.
   
   @param connection [dbi:<dbi-connection>] Connection to disconnect
   "
  (dbi:disconnect connection))

(defmacro with-db-connection-direct ((connection &key (no-database nil)) &body body)
  "Execute body with a direct database connection.
   
   Acquires a direct connection, executes the body, and ensures
   the connection is closed even if an error occurs.
   
   @param connection [symbol] Variable name to bind the connection to
   @param no-database [boolean] If T, connect without specifying a database
   @param body [form] Forms to execute with the connection
   "
  `(let ((,connection (get-connection-direct :no-database ,no-database)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect-direct ,connection))))


(defun get-connection ()
  "Get database connection for the current thread.
   
   If a connection is already associated with the thread, returns it.
   Otherwise, acquires a new connection from the pool and associates it with the thread.
   
   @return [dbi-cp.proxy::<dbi-connection-proxy>] Database connection
   "
  (let ((current-th (bt:current-thread)))
    (get-connection-by-thread current-th)))


(defun release-connection (&optional connection)
  "Release the connection for the current thread back to the pool.
   
   If connection is provided, releases that specific connection.
   Otherwise, releases the connection associated with the current thread.
   
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Optional connection to release
   "
  (let ((thread-id (sb-thread::thread-os-thread (bt:current-thread))))
    (when connection
      (remhash thread-id *thread-connection-pool*)
      (dbi-cp:disconnect connection))))


(defun disconnect (connection)
  "Disconnect and release the connection for the current thread.
   
   @param connection [dbi-cp.proxy::<dbi-connection-proxy>] Connection to disconnect
   "
  (release-connection connection))

(defmacro with-db-connection ((connection) &body body)
  "Execute body with a pooled database connection.
   
   Acquires a connection from the pool for the current thread,
   executes the body, and ensures the connection is released
   even if an error occurs.
   
   @param connection [symbol] Variable name to bind the connection to
   @param body [form] Forms to execute with the connection
   "
  `(let ((,connection (get-connection)))
     (unwind-protect
         (progn
           ,@body)
       (disconnect ,connection))))
