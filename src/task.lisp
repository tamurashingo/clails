;;;; Task System
;;;; Main integration package for task system

(defpackage #:clails/task
  (:use #:cl)
  (:import-from #:clails/task/registry
                #:register-task
                #:find-task
                #:list-tasks
                #:list-namespaces
                #:task-exists-p
                #:remove-task
                #:clear-registry
                #:load-custom-tasks
                #:make-task-info
                #:task-info-name
                #:task-info-namespace
                #:task-info-description
                #:task-info-depends-on
                #:task-info-args
                #:task-info-function)
  (:import-from #:clails/task/runner
                #:run-task)
  (:import-from #:clails/task/core
                #:deftask
                #:defnamespace)
  (:export #:register-task
           #:find-task
           #:list-tasks
           #:list-namespaces
           #:task-exists-p
           #:remove-task
           #:clear-registry
           #:load-custom-tasks
           #:make-task-info
           #:task-info-name
           #:task-info-namespace
           #:task-info-description
           #:task-info-depends-on
           #:task-info-args
           #:task-info-function
           #:run-task
           #:deftask
           #:defnamespace
           #:initialize-task-system))

(in-package #:clails/task)

(defun initialize-task-system ()
  "Initialize task system (load custom tasks).

   This function should be called before executing tasks.
   It loads custom tasks from lib/tasks/.

   @return [t] Returns t after successful initialization
   "
  (load-custom-tasks)
  t)

