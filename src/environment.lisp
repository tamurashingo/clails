(in-package #:cl-user)
(defpackage #:clails/environment
  (:use #:cl)
  (:export #:*project-name*
           #:*project-dir*
           #:*project-environment*
           #:*database-config*
           #:*database-type*
           #:*migration-base-dir*
           #:*connection-pool*
           #:*routing-tables*
           #:*startup-hooks*
           #:*shutdown-hooks*
           #:*default-lock-mode*
           #:*sqlite3-busy-timeout*
           #:*sqlite3-lock-retry-count*
           #:*sqlite3-transaction-mode*
           #:*sqlite3-lock-module-loaded*
           #:<database-type>
           #:<database-type-mysql>
           #:<database-type-postgresql>
           #:<database-type-sqlite3>
           #:<database-type-dummy>
           #:set-environment))
(in-package #:clails/environment)

(defclass <database-type> ()
  ((database-type :initform nil
                  :accessor database-type
                  :documentation "Database type identifier"))
  (:documentation "Base class for database type specifications."))

(defclass <database-type-mysql> (<database-type>)
  ((database-type :initform :mysql))
  (:documentation "MySQL database type specification."))

(defclass <database-type-postgresql> (<database-type>)
  ((database-type :initform :postgresql))
  (:documentation "PostgreSQL database type specification."))

(defclass <database-type-sqlite3> (<database-type>)
  ((database-type :initform :sqlite3))
  (:documentation "SQLite3 database type specification."))

(defclass <database-type-dummy> (<database-type>)
  ((database-type :initform :dummy))
  (:documentation "Dummy database type specification for testing."))


(defparameter *project-name* ""
  "Project name. Set in app/config/environment.lisp.")

(defparameter *project-dir* ""
  "Project directory. Set at startup.")

(defparameter *project-environment* :develop
  "Specify one of :develop, :test, or :production. Can be overridden in app/config/environment.lisp.")

(defparameter *database-config* nil
  "Holds database connection information, etc. Set in app/config/database.lisp.")

(defparameter *database-type* nil
  "Holds an instance of <database-type> to specify the database in use. Set in app/config/database.lisp.")

(defparameter *migration-base-dir* ""
  "The base path for directories where migration files are placed. Usually set to *project-dir*. (May be set to a different directory for testing, etc.)")

(defparameter *connection-pool* nil
  "Database connection pool. Created when the application server statts and destroyed when it shuts down.")

(defparameter *routing-tables*
  '((:path "/"
     :controller "clails/controller/base-controller:<default-controller>")))

(defparameter *startup-hooks*
  '("clails/model/connection:startup-connection-pool"))

(defparameter *shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool"))

(defparameter *default-lock-mode* :for-update
  "Default lock mode for with-locked-transaction macro.

   Possible values:
   - :for-update        - Exclusive lock (default) - PostgreSQL/MySQL
   - :for-share         - Shared lock - PostgreSQL/MySQL
   - :for-no-key-update - PostgreSQL only
   - :for-key-share     - PostgreSQL only
   - :immediate         - SQLite3 only (BEGIN IMMEDIATE)
   - :exclusive         - SQLite3 only (BEGIN EXCLUSIVE)

   This can be overridden in <project>/app/config/environment.lisp")

(defparameter *sqlite3-busy-timeout* 50
  "SQLite3 busy timeout in milliseconds.

   When SQLite3 encounters a locked database, it will wait up to this
   many milliseconds before returning SQLITE_BUSY error.
   Default is 50ms.

   This can be overridden in <project>/app/config/environment.lisp")

(defparameter *sqlite3-lock-retry-count* 3
  "Number of retry attempts for SQLite3 locked database errors.

   When BEGIN IMMEDIATE fails due to database lock, the transaction
   will be retried up to this many times with exponential backoff.
   Default is 3 retries.

   This can be overridden in <project>/app/config/environment.lisp")

(defparameter *sqlite3-transaction-mode* nil
  "SQLite3 transaction mode for the current dynamic context.

   This is a special variable used to pass the transaction mode
   to the begin-transaction method override in sqlite3-lock module.

   Possible values:
   - nil          - Normal transaction (BEGIN TRANSACTION)
   - :immediate   - Immediate lock (BEGIN IMMEDIATE)
   - :exclusive   - Exclusive lock (BEGIN EXCLUSIVE)

   This variable is set by with-locked-transaction macro and should not
   be set directly by user code.")

(defparameter *sqlite3-lock-module-loaded* nil
  "Flag indicating whether sqlite3-lock module has been loaded.

   Set to T after src/model/impl/sqlite3-lock.lisp is successfully loaded.
   Used to ensure the module is loaded only once.")

(defparameter +ENVIRONMENT-NAMES+ '("DEVELOP" "TEST" "PRODUCTION")
  "List of valid environment names.")

(defun check-environment-name (env-name)
  "Check if the given environment name is valid.

   @param env-name [string] Environment name to validate
   @return [boolean] T if valid, NIL otherwise
   "
  (not (null (member env-name +ENVIRONMENT-NAMES+ :test #'string-equal))))

(defun set-environment (env-name)
  "Set the project environment if the name is valid.

   Valid environment names are DEVELOP, TEST, and PRODUCTION (case-insensitive).
   Updates *project-environment* with the keyword version of the name.

   @param env-name [string] Environment name to set
   @return [keyword] The set environment keyword, or NIL if invalid
   "
  (let ((env (string-upcase env-name)))
    (when (check-environment-name env)
      (setf *project-environment* (intern env :KEYWORD)))))
