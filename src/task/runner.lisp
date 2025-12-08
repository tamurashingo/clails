;;;; Task Runner
;;;; Task execution engine with dependency resolution

(defpackage #:clails/task/runner
  (:use #:cl)
  (:import-from #:clails/task/registry
                #:find-task
                #:task-info-name
                #:task-info-namespace
                #:task-info-depends-on
                #:task-info-function)
  (:export #:run-task
           #:*executed-tasks*))

(in-package #:clails/task/runner)

(defvar *executed-tasks* '()
  "List of tasks already executed in current run")

(defun resolve-dependencies (task-info)
  "Resolve and execute task dependencies.

   @param task-info [<task-info>] Task information object
   @return [null] Returns nil
   "
  (dolist (dep (task-info-depends-on task-info))
    (let ((dep-task (if (consp dep)
                        (find-task (second dep) (first dep))
                        (find-task dep))))
      (unless dep-task
        (error "Dependency task not found: ~A" dep))
      (run-task-internal dep-task))))

(defun run-task-internal (task-info &rest args)
  "Internal task execution with idempotency guarantee.

   @param task-info [<task-info>] Task information object
   @param args [list] Arguments to pass to task function
   @return [t] Returns t after successful execution
   "
  (let ((task-key (format nil "~A:~A"
                         (or (task-info-namespace task-info) "")
                         (task-info-name task-info))))
    (unless (member task-key *executed-tasks* :test #'string=)
      (resolve-dependencies task-info)
      
      (format t "~&Running task: ~A~%" task-key)
      (apply (task-info-function task-info) args)
      
      (push task-key *executed-tasks*))))

(defun run-task (name-or-list &rest args-or-namespace)
  "Run a task by name and optional namespace.

   Dependencies are resolved and executed first.
   Tasks are executed only once per run (idempotent).

   Usage:
     (run-task :task-name)
     (run-task :task-name :namespace)
     (run-task :task-name :key1 val1 :key2 val2)
     (run-task :task-name :namespace :key1 val1)

   @param name-or-list [keyword] Task name
   @param args-or-namespace [list] Namespace (optional) and task arguments
   @return [t] Returns t after successful execution
   @condition error Task not found
   "
  (let ((name name-or-list)
        (namespace nil)
        (args '()))
    ;; Parse arguments
    (when args-or-namespace
      (let ((first-arg (first args-or-namespace))
            (rest-args (rest args-or-namespace)))
        ;; If first argument is a symbol AND
        ;; (rest is empty OR rest starts with keyword),
        ;; treat first as namespace
        (if (and (symbolp first-arg)
                 (or (null rest-args)
                     (keywordp (first rest-args))))
            (progn
              (setf namespace first-arg)
              (setf args rest-args))
            ;; Otherwise all args are task arguments
            (setf args args-or-namespace))))
    
    (let ((*executed-tasks* '()))
      (let ((task-info (find-task name namespace)))
        (unless task-info
          (error "Task not found: ~A~@[:~A~]" namespace name))
        (apply #'run-task-internal task-info args)))))
