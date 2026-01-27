(in-package #:cl-user)
(defpackage #:todoapp/models/todo
  (:use #:cl)
  (:import-from #:clails/model
                #:defmodel
                #:<base-model>
                #:query
                #:execute-query
                #:make-record
                #:save
                #:ref)
  (:import-from #:local-time
                #:now)
  (:export #:<todo>
           #:find-all
           #:create-todo
           #:find-by-id
           #:mark-as-done))

(in-package #:todoapp/models/todo)

(defmodel <todo> (<base-model>) (:table "todo"))

(defparameter *find-all*
  (query <todo> :as :todo))

(defun find-all ()
  "Find all todo items.

   @return [list] List of todo records
   "
   (execute-query *find-all* nil))

(defun create-todo (title)
  "Create a new todo item with the given title.

   @param title [string] Todo title
   @return [<todo>] Created todo record
   "
  (let ((todo (make-record '<todo> :title title :done nil)))
    (save todo)
    todo))

(defparameter *find-by-id*
  (query <todo>
         :as :todo
         :where (:= (:todo :id) :id-param)))

(defun find-by-id (id)
  "Find a todo item by ID.

   @param id [integer] Todo ID
   @return [<todo>] Todo record
   @return [nil] NIL if not found
   "
  (let* ((results (execute-query *find-by-id* (list :id-param id))))
    (car results)))

(defun mark-as-done (todo)
  "Mark a todo item as done.

   @param todo [<todo>] Todo record to mark as done
   @return [<todo>] Updated todo record
   "
  (setf (ref todo :done) t)
  (setf (ref todo :done-at) (now))
  (save todo)
  todo)
