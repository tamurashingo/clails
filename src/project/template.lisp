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
  :depends-on (\"clails\"
               \"clails-<%= string-downcase (@ database )) %>\")
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
                             (:file \"database\")))))
"))

(defparameter package-template
  (make-instance '<template>
                 :path "/"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>
  (:use #:cl))
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
  (:import-from #:clails/util
                #:env-or-default))
(in-package #:<%= (@ project-name ) %>-config)

(defun env (env-name)
  (uiop:getenv env-name))

"))

(defparameter config/database-sqlite-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(defparameter *db*
  `(:database :sqlite3
    :develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-develop.sqlite3\"))
    :test (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-test.sqlite3\"))
    :production (:database ,(env \"CLAILS_DB_NAME\"))))
"))

(defparameter config/database-mysql-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(defparameter *db*
  `(:database :mysql
    :develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
              :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
              :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
              :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
              :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :test (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_test\")
           :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
           :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
           :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
           :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :production (:database ,(env \"CLAILS_DB_NAME\")
                 :host ,(env \"CLAILS_DB_HOST\")
                 :port ,(env \"CLAILS_DB_PORT\")
                 :username ,(env \"CLAILS_DB_USERNAME\")
                 :password ,(env \"CLAILS_DB_PASSWORD\"))))
"))

(defparameter config/database-postgresql-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(defparameter *db*
  `(:database :postgresql
    :develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
              :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
              :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
              :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
              :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :test (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_test\")
           :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
           :port ,(env-or-default \"CLAILS_DB_PORT\" \"3306\")
           :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"root\")
           :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :production (:database ,(env \"CLAILS_DB_NAME\")
                 :host ,(env \"CLAILS_DB_HOST\")
                 :port ,(env \"CLAILS_DB_PORT\")
                 :username ,(env \"CLAILS_DB_USERNAME\")
                 :password ,(env \"CLAILS_DB_PASSWORD\"))))
"))


(defparameter model-migration-create-template
  (make-instance '<template>
                 :path "/db/migrate"
                 :prefix "create"
                 :template "(create-table <%= (@ model-name ) %>
(
<% (loop for col in (@ body do %><%= (format NIL \"  ~S~%\" col) %><% ) %>))
"))

(defparameter model-migration-addcolumn-template
  (make-instance '<template>
                 :path "/db/migrate"
                 :prefix "add-column"
                 :template "(add-column <%= (@ model-name ) %>
(
<% (loop for col in (@ body do %><%= (format NIL \"  ~S~%\" col) %><% ) %>))
"))

(defparameter model-template
  (make-instance '<template>
                 :path "/app/models"
                 :template "(in-package #:<%= (@ project-name ) %>-model)

(defclass <%= (@ model ) %> (<base-model>) ())
"))
              

