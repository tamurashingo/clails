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
