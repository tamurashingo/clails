;;;; Job System
;;;; Main integration package for the background job queue.
;;;;
;;;; See document/job-queue.md for a full walkthrough. In short:
;;;;
;;;;   (defjob :send-welcome-email
;;;;     :max-attempts 5
;;;;     :function (lambda (&key user-id) ...))
;;;;
;;;;   (enqueue-job :send-welcome-email :arguments (list :user-id 42))
;;;;
;;;;   (run-worker-loop)                     ; or (start-worker), (stop-worker)

(defpackage #:clails/job
  (:use #:cl)
  (:import-from #:clails/job/registry
                #:register-job
                #:find-job
                #:list-jobs
                #:job-exists-p
                #:remove-job
                #:clear-registry
                #:make-job-info
                #:job-info-name
                #:job-info-description
                #:job-info-max-attempts
                #:job-info-function)
  (:import-from #:clails/job/core
                #:defjob)
  (:import-from #:clails/job/queue
                #:*jobs-table-name*
                #:*default-max-attempts*
                #:*default-backoff-base-seconds*
                #:*default-backoff-max-seconds*
                #:enqueue-job
                #:claim-due-job
                #:mark-job-succeeded
                #:mark-job-retry
                #:mark-job-failed
                #:backoff-seconds
                #:job-plist-id
                #:job-plist-job-name
                #:job-plist-arguments
                #:job-plist-attempts
                #:job-plist-max-attempts)
  (:import-from #:clails/job/worker
                #:run-worker-tick
                #:run-worker-loop
                #:start-worker
                #:stop-worker
                #:worker-running-p)
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
           #:job-info-function
           #:defjob
           #:*jobs-table-name*
           #:*default-max-attempts*
           #:*default-backoff-base-seconds*
           #:*default-backoff-max-seconds*
           #:enqueue-job
           #:claim-due-job
           #:mark-job-succeeded
           #:mark-job-retry
           #:mark-job-failed
           #:backoff-seconds
           #:job-plist-id
           #:job-plist-job-name
           #:job-plist-arguments
           #:job-plist-attempts
           #:job-plist-max-attempts
           #:run-worker-tick
           #:run-worker-loop
           #:start-worker
           #:stop-worker
           #:worker-running-p))
(in-package #:clails/job)
