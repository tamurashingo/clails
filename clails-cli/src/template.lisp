(in-package #:cl-user)
(defpackage #:clails-cli/template
  (:use #:cl)
  (:export :<template>
           :path
           :prefix
           :postfix
           :template))
(in-package #:clails-cli/template)

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
    :type string)
   (postfix
    :initarg :postfix
    :initform ""
    :reader postfix
    :type string)
   (template
    :initarg :template
    :reader template
    :type string)))



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
               \"clails-model\")
  :components ((:file \"package\")
               (:module \"app\"
                :components ((:module \"controllers\"
                              :components ((:file \"package\")))
                             (:module \"models\"
                              :components ((:file \"package\")))
                             (:module \"views\"
                              :components ((:file \"package\")))))))
"))

(defparameter package-template
  (make-instance '<template>
                 :path "/"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>)

(setf clails-cli/main::*clails-project* \"<%= (@ project-name ) %>\")
(setf clails-cli/main::*clails-directory* \"<%= (@ project-dir ) %>\")
"))




(defparameter controller-package-template
  (make-instance '<template>
                 :path "/app/controllers"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-controller
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>-controller)
"))


(defparameter model-package-template
  (make-instance '<template>
                 :path "/app/models"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-model
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>-model)
"))

(defparameter view-package-template
  (make-instance '<template>
                 :path "/app/views"
                 :template "(in-package #:cl-user)
(defpackage #:<%= (@ project-name ) %>-view
  (:use #:cl))
(in-package #:<%= (@ project-name ) %>-view)
"))

(defparameter model-migration-create-template
  (make-instance '<template>
                 :path "/db/migrate"
                 :prefix "create"
                 :template "(create-table <%= (@ model-name ) %>
(
<% (loop for col in (@ body) do %><%= (format NIL \"  ~S~%\" col) %><% ) %>))
"))

(defparameter model-migration-addcolumn-template
  (make-instance '<template>
                 :path "/db/migrate"
                 :prefix "add-column"
                 :template "(add-column <%= (@ model-name ) %>
(
<% (loop for col in (@ body) do %><%= (format NIL \"  ~S~%\" col) %><% ) %>))
"))


(defparameter model-template
  (make-instance '<template>
                 :path "/app/models"
                 :template "(in-package #:<%= (@ project-name ) %>-model)

(defclass <%= (@ model ) %> (<base-model>) ())
"))



(defparameter controller-template
  "")

(defparameter view-template
  "")


