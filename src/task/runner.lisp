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
  (:import-from #:clails/logger/core
                #:log.task)
  (:export #:run-task
           #:*executed-tasks*))

(in-package #:clails/task/runner)

(defvar *executed-tasks* '()
  "List of tasks already executed in current run")

(defun format-task-name (task-info)
  "Format task name for logging.

   @param task-info [<task-info>] Task information object
   @return [string] Formatted task name (e.g., \"db:migrate\" or \"cleanup\")
   "
  (if (task-info-namespace task-info)
      (format nil "~A:~A"
              (task-info-namespace task-info)
              (task-info-name task-info))
      (format nil "~A" (task-info-name task-info))))

(defun resolve-dependencies (task-info)
  "Resolve and execute task dependencies with logging.

   @param task-info [<task-info>] Task information object
   @return [null] Returns nil
   "
  (dolist (dep (task-info-depends-on task-info))
    (let* ((dep-task (if (consp dep)
                         (find-task (second dep) (first dep))
                         (find-task dep)))
           (dep-task-name (when dep-task (format-task-name dep-task))))
      (unless dep-task
        (log.task "Dependency not found"
                  :task-name (format-task-name task-info)
                  :dependency dep
                  :status :error)
        (error "Dependency task not found: ~A" dep))

      ;; Log dependency resolution
      (log.task "Resolving dependency"
                :task-name (format-task-name task-info)
                :dependency dep-task-name)

      (run-task-internal dep-task))))

(defun run-task-internal (task-info &rest args)
  "Internal task execution with idempotency guarantee and logging.

   @param task-info [<task-info>] Task information object
   @param args [list] Arguments to pass to task function
   @return [t] Returns t after successful execution
   "
  (let* ((task-name (format-task-name task-info))
         (task-key task-name)
         (start-time (get-internal-real-time)))

    (unless (member task-key *executed-tasks* :test #'string=)
      ;; Resolve dependencies first
      (resolve-dependencies task-info)

      ;; Log task start
      (log.task "Task started"
                :task-name task-name
                :namespace (task-info-namespace task-info)
                :args args)
      (format t "~&Running task: ~A~%" task-name)

      ;; Execute task with error handling
      (handler-case
          (progn
            (apply (task-info-function task-info) args)

            ;; Log task completion
            (let* ((end-time (get-internal-real-time))
                   (duration (/ (- end-time start-time)
                               internal-time-units-per-second)))
              (log.task "Task completed"
                        :task-name task-name
                        :namespace (task-info-namespace task-info)
                        :duration duration
                        :status :success)
              (format t "Task completed: ~A (~,3Fs)~%" task-name duration)))

        (error (e)
          ;; Log task failure
          (let* ((end-time (get-internal-real-time))
                 (duration (/ (- end-time start-time)
                             internal-time-units-per-second)))
            (log.task "Task failed"
                      :task-name task-name
                      :namespace (task-info-namespace task-info)
                      :duration duration
                      :status :error
                      :error (format nil "~A" e))
            (format *error-output* "Task failed: ~A - ~A~%" task-name e)
            (error e))))

      ;; Mark task as executed
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
          (log.task "Task not found"
                    :task-name (if namespace
                                   (format nil "~A:~A" namespace name)
                                   (format nil "~A" name))
                    :status :error)
          (error "Task not found: ~A~@[:~A~]" namespace name))
        (apply #'run-task-internal task-info args)))))
