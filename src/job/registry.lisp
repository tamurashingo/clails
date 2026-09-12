;;;; Job Registry
;;;; Central registry for job registration and lookup.
;;;;
;;;; This mirrors clails/task/registry, but jobs are keyed by name only
;;;; (no namespaces) since jobs are executed asynchronously by the worker
;;;; rather than invoked ad-hoc from the CLI.

(defpackage #:clails/job/registry
  (:use #:cl)
  (:export #:register-job
           #:find-job
           #:list-jobs
           #:job-exists-p
           #:remove-job
           #:clear-registry
           #:make-job-info
           #:job-info-name
           #:job-info-description
           #:job-info-max-attempts
           #:job-info-function))

(in-package #:clails/job/registry)

(defvar *job-registry* (make-hash-table :test 'eq)
  "Global job registry. Key is the job name keyword.")

(defclass <job-info> ()
  ((name
    :initarg :name
    :accessor job-info-name
    :documentation "Job name (keyword)")
   (description
    :initarg :description
    :initform nil
    :accessor job-info-description
    :documentation "Job description")
   (max-attempts
    :initarg :max-attempts
    :initform 25
    :accessor job-info-max-attempts
    :documentation "Default maximum attempts before a job is marked failed")
   (function
    :initarg :function
    :accessor job-info-function
    :documentation "Job handler function"))
  (:documentation "Information about a registered job."))

(defun make-job-info (&key name description (max-attempts 25) function)
  "Create a new job-info instance.

   @param name [keyword] Job name
   @param description [string or nil] Job description (optional)
   @param max-attempts [integer] Default maximum attempts (default: 25)
   @param function [function] Job handler function
   @return [<job-info>] Job information object
   "
  (make-instance '<job-info>
                 :name name
                 :description description
                 :max-attempts max-attempts
                 :function function))

(defun register-job (name &key description (max-attempts 25) function)
  "Register a job handler in the global registry.

   @param name [keyword] Job name
   @param description [string or nil] Job description (optional)
   @param max-attempts [integer] Default maximum attempts (default: 25)
   @param function [function] Job handler function
   @return [keyword] The registered job name
   "
  (setf (gethash name *job-registry*)
        (make-job-info :name name
                       :description description
                       :max-attempts max-attempts
                       :function function))
  name)

(defun find-job (name)
  "Find a job by name.

   @param name [keyword] Job name
   @return [<job-info> or nil] Job information if found, nil otherwise
   "
  (gethash name *job-registry*))

(defun job-exists-p (name)
  "Check if a job is registered.

   @param name [keyword] Job name
   @return [boolean] T if the job exists, nil otherwise
   "
  (not (null (find-job name))))

(defun list-jobs ()
  "List all registered jobs.

   @return [list] List of (name . job-info) pairs sorted by name
   "
  (let ((jobs '()))
    (maphash (lambda (name job-info)
               (push (cons name job-info) jobs))
             *job-registry*)
    (sort jobs #'string< :key (lambda (pair) (string (car pair))))))

(defun remove-job (name)
  "Remove a job from the registry.

   @param name [keyword] Job name
   @return [boolean] T if the job was removed, nil if not found
   "
  (remhash name *job-registry*))

(defun clear-registry ()
  "Clear all jobs from the registry.

   @return [null] Always returns nil
   "
  (clrhash *job-registry*))
