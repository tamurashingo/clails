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


