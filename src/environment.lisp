(in-package #:cl-user)
(defpackage #:clails/environment
  (:use #:cl)
  (:export #:*project-name*
           #:*project-dir*
           #:*project-environment*
           #:*database-config*
           #:*database-type*
           #:*connection-pool*
           #:*routing-tables*
           #:*startup-hooks*
           #:*shutdown-hooks*
           #:set-environment))
(in-package #:clails/environment)

(defclass <database-type> ()
  ((database-type :initform nil
                  :accessor database-type)))
(defclass <database-type-mysql> (<database-type>)
  ((database-type :initform :mysql)))
(defclass <database-type-postgresql> (<database-type>)
  ((database-type :initform :postgresql)))
(defclass <database-type-sqlite3> (<database-type>)
  ((database-type :initform :sqlite3)))
(defclass <database-type-dummy> (<database-type>)
  ((database-type :initform :dummy)))


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

(defparameter *connection-pool* nil
  "Database connection pool. Created when the application server statts and destroyed when it shuts down.")

(defparameter *routing-tables*
  '((:path "/"
     :controller "clails/controller/base-controller:<default-controller>")))

(defparameter *startup-hooks*
  '("clails/model/connection:startup-connection-pool"))

(defparameter *shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool"))


(defparameter +ENVIRONMENT-NAMES+ '("DEVELOP" "TEST" "PRODUCTION"))

(defun check-environment-name (env-name)
  (not (null (member env-name +ENVIRONMENT-NAMES+ :test #'string-equal))))

(defun set-environment (env-name)
  (let ((env (string-upcase env-name)))
    (when (check-environment-name env)
      (setf *project-environment* (intern env :KEYWORD)))))
