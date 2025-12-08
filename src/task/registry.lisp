;;;; Task Registry
;;;; Central registry for task registration, searching, and management

(defpackage #:clails/task/registry
  (:use #:cl)
  (:import-from #:clails/environment
                #:*task-base-dir*)
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
           #:task-info-function))

(in-package #:clails/task/registry)

(defvar *task-registry* (make-hash-table :test 'equal)
  "Global task registry. Key is \"namespace:name\" or \"name\"")

(defclass <task-info> ()
  ((name
    :initarg :name
    :accessor task-info-name
    :documentation "Task name")
   (namespace
    :initarg :namespace
    :initform nil
    :accessor task-info-namespace
    :documentation "Task namespace (optional)")
   (description
    :initarg :description
    :initform nil
    :accessor task-info-description
    :documentation "Task description")
   (depends-on
    :initarg :depends-on
    :initform nil
    :accessor task-info-depends-on
    :documentation "List of task dependencies")
   (args
    :initarg :args
    :initform nil
    :accessor task-info-args
    :documentation "List of task arguments")
   (function
    :initarg :function
    :accessor task-info-function
    :documentation "Task function"))
  (:documentation "Information about a task"))

(defun make-task-info (&key name namespace description depends-on args function)
  "Create a new task-info instance.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @param description [string or nil] Task description (optional)
   @param depends-on [list] List of task dependencies (optional)
   @param args [list] List of task arguments (optional)
   @param function [function] Task function
   @return [<task-info>] Task information object
   "
  (make-instance '<task-info>
                 :name name
                 :namespace namespace
                 :description description
                 :depends-on depends-on
                 :args args
                 :function function))

(defun task-key (name &optional namespace)
  "Generate registry key from task name and optional namespace.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @return [string] Registry key
   "
  (if namespace
      (format nil "~A:~A" namespace name)
      (format nil "~A" name)))

(defun register-task (name &key namespace description depends-on args function)
  "Register a task in the global registry.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @param description [string or nil] Task description (optional)
   @param depends-on [list] List of task dependencies (optional)
   @param args [list] List of task arguments (optional)
   @param function [function] Task function
   @return [string] Registry key
   "
  (let ((key (task-key name namespace)))
    (setf (gethash key *task-registry*)
          (make-task-info :name name
                         :namespace namespace
                         :description description
                         :depends-on depends-on
                         :args args
                         :function function))
    key))

(defun find-task (name &optional namespace)
  "Find a task by name and optional namespace.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @return [<task-info> or nil] Task information if found, nil otherwise
   "
  (let ((key (task-key name namespace)))
    (gethash key *task-registry*)))

(defun task-exists-p (name &optional namespace)
  "Check if a task exists.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @return [boolean] T if task exists, nil otherwise
   "
  (not (null (find-task name namespace))))

(defun list-tasks (&optional namespace)
  "List all tasks, optionally filtered by namespace.

   @param namespace [keyword or nil] Filter by namespace (optional)
   @return [list] List of (key . task-info) pairs sorted by key
   "
  (let ((tasks '()))
    (maphash (lambda (key task-info)
               (when (or (null namespace)
                        (equal namespace (task-info-namespace task-info)))
                 (push (cons key task-info) tasks)))
             *task-registry*)
    (sort tasks #'string< :key #'car)))

(defun list-namespaces ()
  "List all namespaces.

   @return [list] Sorted list of namespaces
   "
  (let ((namespaces (make-hash-table :test 'equal)))
    (maphash (lambda (key task-info)
               (declare (ignore key))
               (when (task-info-namespace task-info)
                 (setf (gethash (task-info-namespace task-info) namespaces) t)))
             *task-registry*)
    (sort (loop for ns being the hash-keys of namespaces collect ns)
          #'string<)))

(defun remove-task (name &optional namespace)
  "Remove a task from registry.

   @param name [keyword] Task name
   @param namespace [keyword or nil] Task namespace (optional)
   @return [boolean] T if task was removed, nil if not found
   "
  (let ((key (task-key name namespace)))
    (remhash key *task-registry*)))

(defun clear-registry ()
  "Clear all tasks from registry.

   @return [null] Always returns nil
   "
  (clrhash *task-registry*))

(defun load-custom-tasks ()
  "Load all custom task files from lib/tasks/.

   This function loads all .lisp files from {*task-base-dir*}/lib/tasks/**/*.
   Task files should define tasks using deftask or defnamespace macros.

   Files are loaded in alphabetical order.

   @return [integer] Number of task files loaded
   "
  (let ((task-files (directory (format nil "~A/lib/tasks/**/*.lisp"
                                       *task-base-dir*)))
        (loaded-count 0))
    (dolist (file (sort task-files #'string< :key #'namestring))
      (format t "Loading task file: ~A" file)
      (handler-case
          (progn
            (load file)
            (incf loaded-count)
            (format t " ... done~%"))
        (error (e)
          (format t " ... failed: ~A~%" e))))
    (format t "Loaded ~A task file~:P~%" loaded-count)
    loaded-count))
