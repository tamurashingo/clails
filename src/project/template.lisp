(in-package #:cl-user)
(defpackage #:clails/project/template
  (:use #:cl)
  (:export #:<template>
           #:path
           #:prefix
           #:postfix
           #:template))
(in-package #:clails/project/template)

(defclass <template> ()
  ((path
    :initarg :path
    :reader path
    :type string
    :documentation "Relative path from the project directory where this file is located")
   (prefix
    :initarg :prefix
    :initform ""
    :reader prefix
    :type string
    :documentation "Used to add 'create' or 'add-column' when generating migration files")
   (postfix
    :initarg :postfix
    :initform ""
    :reader postfix
    :type string
    :documentation "RESERVED")
   (template
    :initarg :template
    :reader template
    :type string)))


(defparameter asd-template
  (make-instance '<template>
                 :path "/"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-system
  (:use #:asdf #:cl))
(in-package #:<%= (@ project-name ) %>-system)

(defsystem <%= (@ project-name ) %>
  :description \"\"
  :version \"0.0.1\"
  :author \"\"
  :license \"\"
  :depends-on (\"clails\")
  :components ((:file \"package\")
               (:module \"app\"
                :components ((:module \"controllers\"
                              :components ((:file \"package\")))
                             (:module \"models\"
                              :components ((:file \"package\")))
                             (:module \"views\"
                              :components ((:file \"package\")))))
               (:module \"config\"
                :components ((:file \"package\")
                             (:file \"environment\")
                             (:file \"database\")))))
"))

(defparameter package-template
  (make-instance '<template>
                 :path "/"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>
  (:use #:cl))
(defpackage #:<%= (@ project-name ) %>/model/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))
(in-package #:<%= (@ project-name ) %>)

(setf clails/environment:*project-name* \"<%= (@ project-name ) %>\")
(setf clails/environment:*project-dir* \"<%= (@ project-dir ) %>\")
(setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-<%= string-downcase (string (@ database )) %>>))
"))

(defparameter app/controller/package-template
  (make-instance '<template>
                 :path "/app/controllers"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-controller
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>-controller)
"))

(defparameter app/model/package-template
  (make-instance '<template>
                 :path "/app/models"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-model
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:ref
                #:<base-model>))
(in-package #:<%= (@ project-name ) %>-model)
"))

(defparameter app/view/package-template
  (make-instance '<template>
                 :path "/app/views"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-view
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>-view)
"))

(defparameter config/package-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-config
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-environment*)
  (:import-from #:clails/util
                #:env-or-default))
(in-package #:<%= (@ project-name ) %>-config)

(defun env (env-name)
  (uiop:getenv env-name))

"))

(defparameter config/environment-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)


;; project name
(setf clails/environment:*project-name* \"<%= (@ project-name ) %>\")

;; project directory
(setf clails/environment:*project-dir* #P\"<%= (@ project-dir ) %>\")
"))


(defparameter config/database-sqlite-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(setf clails/environment:*database-config*
  `(:database :sqlite3
    :develop (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-develop.sqlite3\"))
    :test (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-test.sqlite3\"))
    :production (:database-name ,(env \"CLAILS_DB_NAME\"))))
"))

(defparameter config/database-mysql-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(setf clails/environment:*database-config*
  `(:database :mysql
    :develop (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
              :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
              :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
              :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
              :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :test (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_test\")
           :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
           :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
           :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
           :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :production (:database-name ,(env \"CLAILS_DB_NAME\")
                 :host ,(env \"CLAILS_DB_HOST\")
                 :port ,(env \"CLAILS_DB_PORT\")
                 :username ,(env \"CLAILS_DB_USERNAME\")
                 :password ,(env \"CLAILS_DB_PASSWORD\"))))
"))

(defparameter config/database-postgresql-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(setf clails/environment:*database-config*
  `(:database :postgresql
    :develop (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
              :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
              :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
              :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
              :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :test (:database-name ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_test\")
           :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
           :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
           :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
           :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :production (:database-name ,(env \"CLAILS_DB_NAME\")
                 :host ,(env \"CLAILS_DB_HOST\")
                 :port ,(env \"CLAILS_DB_PORT\")
                 :username ,(env \"CLAILS_DB_USERNAME\")
                 :password ,(env \"CLAILS_DB_PASSWORD\"))))
"))


(defparameter model-migration-template
  (make-instance '<template>
                 :path "/db/migrate"
                 :prefix "create"
                 :template "(in-package #:<%= (@ project-name ) %>/model/db)

(defmigration \"<%= (@ migration-name ) %>\"
  (:up #'(lambda (connection)
           ;;
           nil)
   :down #'(lambda (connection)
             ;;
             nil)))
"))

(defparameter model-template
  (make-instance '<template>
                 :path "/app/models"
                 :template "(in-package #:<%= (@ project-name ) %>-model)

(defmodel <<%= (@ model ) %>> (<base-model>)
  (:table \"<%= (@ model ) %>\"))
"))

