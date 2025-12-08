;;;; Task Core
;;;; DSL for defining tasks

(defpackage #:clails/task/core
  (:use #:cl)
  (:import-from #:clails/task/registry
                #:register-task)
  (:export #:deftask
           #:defnamespace))

(in-package #:clails/task/core)

(defmacro deftask (name &key description namespace depends-on args function)
  "Define a new task.

   Example:
     (deftask :migrate
       :namespace :db
       :description \"Run database migrations\"
       :depends-on (:create)
       :args (&key (verbose t))
       :function (lambda (&key (verbose t))
                   (format t \"Running migrations~%\")))

   @param name [keyword] Task name
   @param description [string or nil] Task description (optional)
   @param namespace [keyword or nil] Task namespace (optional)
   @param depends-on [list] List of task dependencies (optional)
   @param args [list] Lambda list for task arguments (optional)
   @param function [form] Task implementation (should be a lambda form)
   @return [string] Registry key
   "
  `(register-task ',name
                  :description ,description
                  :namespace ',namespace
                  :depends-on ',depends-on
                  :args ',args
                  :function ,function))

(defmacro defnamespace (namespace &body tasks)
  "Define multiple tasks within a namespace.

   Example:
     (defnamespace :db
       (deftask :migrate
         :description \"Run migrations\"
         :function (lambda () ...))
       (deftask :seed
         :description \"Load seed data\"
         :function (lambda () ...)))

   @param namespace [keyword] Namespace name
   @param tasks [list] List of deftask forms
   @return [list] List of registry keys
   "
  `(progn
     ,@(mapcar (lambda (task-def)
                 `(deftask ,(second task-def)
                    :namespace ,namespace
                    ,@(cddr task-def)))
               tasks)))
