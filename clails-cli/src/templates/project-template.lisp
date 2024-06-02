(in-package #:cl-user)
(defpackage #:clails-cli/templates/project-template
  (:use #:cl)
  (:import-from #:clails-cli/template
                #:<template>)
  (:export #:asd-template
           #:package-template
           #:controller-package-template
           #:model-package-template
           #:view-package-template))

(in-package #:clails-cli/templates/project-template)


(defparameter asd-template
  (make-instance '<template>
                 :path "/"
                 :template   "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-system
  (:use #:asdf #:cl))
(in-package #:<%= (@ project-name ) %>-system)

(defsystem <%= (@ project-name) %>
  :description \"\"
  :version \"0.0.1\"
  :author \"\"
  :license \"\"
  :depends-on (\"clails-cli\"
               \"clails-model\"
               \"clails-model-<%= string-downcase (string (@ database )) %>\")
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
                             (:file \"database\")))
               (:module \"cli\"
                :components ((:file \"<%= (@ project-name ) %>-cli\")))))
"))

(defparameter package-template
  (make-instance '<template>
                 :path "/"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>)

(defparameter *clails-project* \"<%= (@ project-name ) %>\")
(defparameter *clails-directory* \"<%= (@ project-dir ) %>\")
(defparameter *clails-database* \"<%= string-downcase (string (@ database )) %>\")
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
  (:use #:cl))
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
  (:export #:db/create))
(in-package #:<%= (@ project-name ) %>-config)

(defun env (env-name)
  (uiop:getenv env-name))

(defun env-or-default (env-name default-value)
  (or (uiop:getenv env-name)
      default-value))

"))

(defparameter config/database-sqlite-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)


(defun db/create (env)
  (let ((database-name (getf (getf *db* env) :database)))
    (dbi:connect :sqlite3
                 :database-name database-name)
    ; log
    (format T \"database ~A created.~%\" database-name)))



(defparameter *db*
  `(:develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-develop.sqlite3\"))
    :test (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-dir ) %>/tmp/<%= (@ project-name ) %>-test.sqlite3\"))
    :production (:database ,(env \"CLAILS_DB_NAME\"))))
"))

(defparameter config/database-mysql-template
  (make-instance '<template>
                 :path "/config"
                 :template "(in-package #:<%= (@ project-name ) %>-config)

(defun db/create (env)
  (let* ((db (getf *db* env))
         (database-name (getf db :database))
         (host (getf db :host))
         (port (parse-integer (getf db :port)))
         (username (getf db :username))
         (password (getf db :password)))
    (dbi:with-connection (conn :mysql
                               :host host
                               :port port
                               :username username
                               :password password)
      (dbi:do-sql conn (format NIL \"create database ~A\" database-name))
      ; log
      (format T \"database ~A created.~%\" database-name))))

(defparameter *db*
  `(:develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
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

(defun db/create (env)
  (let* ((db (getf *db* env))
         (database-name (getf db :database))
         (host (getf db :host))
         (port (parse-integer (getf db :port)))
         (username (getf db :username))
         (password (getf db :password)))
    (dbi:with-connection (conn :postgres
                               :database-name \"\"
                               :host host
                               :port port
                               :username username
                               :password password)
      (dbi:do-sql conn (format NIL \"create database ~A\" database-name))
      ; log
      (format T \"database ~A created.~%\" database-name))))


(defparameter *db*
  `(:develop (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_develop\")
              :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
              :port ,(env-or-default \"CLAILS_DB_PORT\" \"5432\")
              :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"clails\")
              :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :test (:database ,(env-or-default \"CLAILS_DB_NAME\" \"<%= (@ project-name ) %>_test\")
           :host ,(env-or-default \"CLAILS_DB_HOST\" \"localhost\")
           :port ,(env-or-default \"CLAILS_DB_PORT\" \"5432\")
           :username ,(env-or-default \"CLAILS_DB_USERNAME\" \"clails\")
           :password ,(env-or-default \"CLAILS_DB_PASSWORD\" \"password\"))
    :production (:database ,(env \"CLAILS_DB_NAME\")
                 :host ,(env \"CLAILS_DB_HOST\")
                 :port ,(env \"CLAILS_DB_PORT\")
                 :username ,(env \"CLAILS_DB_USERNAME\")
                 :password ,(env \"CLAILS_DB_PASSWORD\"))))
"))

