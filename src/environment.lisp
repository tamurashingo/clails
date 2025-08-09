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
           #:*shutdown-hooks*))
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
  "project name")

(defparameter *project-dir* ""
  "Project directory, Used whengenerating migration files, etc.")

(defparameter *project-environment* :develop
  ":develop, :test, :production")

(defparameter *database-config* nil
  "Database configuration. Set when the application server starts.")

(defparameter *database-type* nil
  "Type of database class")

(defparameter *connection-pool* nil
  "Database connection pool. Created when the application server statts and destroyed when it shuts down.")

(defparameter *routing-tables*
  '((:path "/"
     :controller "clails/controller/base-controller:<default-controller>")))

(defparameter *startup-hooks*
  '("clails/model/connection:startup-connection-pool"))

(defparameter *shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool"))
