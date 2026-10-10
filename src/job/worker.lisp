;;;; Job Worker
;;;; Polls the job queue table and runs due jobs, with retry/backoff.

(defpackage #:clails/job/worker
  (:use #:cl)
  (:import-from #:clails/job/registry
                #:find-job
                #:job-info-function)
  (:import-from #:clails/job/queue
                #:claim-due-job
                #:mark-job-succeeded
                #:mark-job-retry
                #:mark-job-failed
                #:job-plist-id
                #:job-plist-job-name
                #:job-plist-arguments
                #:job-plist-attempts
                #:job-plist-max-attempts)
  (:import-from #:clails/logger/core
                #:log.job)
  (:export #:run-worker-tick
           #:run-worker-loop
           #:start-worker
           #:stop-worker
           #:worker-running-p))

(in-package #:clails/job/worker)

(defvar *worker-thread* nil
  "The bordeaux-threads thread started by start-worker, or nil if the worker
   is not running as a background thread.")

(defvar *worker-stop-requested* nil
  "When T, run-worker-loop exits at the start of its next iteration.")

(defun process-claimed-job (job)
  "Run the handler for a claimed job and record the outcome.

   Looks the job's handler up in the job registry by name and applies it to
   the job's stored arguments. On success the job is marked :succeeded. On
   error, the job is rescheduled with exponential backoff (see
   clails/job/queue:backoff-seconds) if it has not yet reached its
   max-attempts, or marked :failed (with the error message recorded)
   otherwise. A job whose name is not registered in this process is treated
   the same as a handler error, using the same retry/backoff bookkeeping, so
   that a worker started before the job's defjob is loaded still gets a
   chance to retry once it is.

   @param job [plist] Claimed job, as returned by clails/job/queue:claim-due-job
   @return [keyword] One of :succeeded, :retrying, :failed
   "
  (let* ((job-id (job-plist-id job))
         (job-name (job-plist-job-name job))
         (arguments (job-plist-arguments job))
         (attempts (job-plist-attempts job))
         (max-attempts (job-plist-max-attempts job))
         (job-info (find-job job-name)))
    (handler-case
        (progn
          (unless job-info
            (error "No job registered for name ~A" job-name))
          (apply (job-info-function job-info) arguments)
          (mark-job-succeeded job-id)
          :succeeded)
      (error (e)
        (let ((error-message (format nil "~A" e)))
          (if (>= attempts max-attempts)
              (progn
                (mark-job-failed job-id error-message)
                :failed)
              (progn
                (mark-job-retry job-id attempts error-message)
                :retrying)))))))

(defun run-worker-tick ()
  "Claim and run at most one due job.

   @return [keyword] :ran if a job was claimed and processed (regardless of
                     whether it ultimately succeeded, was rescheduled, or
                     failed), :idle if no job was currently due
   "
  (let ((job (claim-due-job)))
    (if job
        (progn (process-claimed-job job) :ran)
        :idle)))

(defun run-worker-loop (&key max-iterations (poll-interval 1))
  "Repeatedly claim and run due jobs until stopped.

   Runs run-worker-tick in a loop. Whenever a tick finds no due job (:idle),
   sleeps for poll-interval seconds before polling again. Intended to be run
   either directly (e.g. from the CLI 'job:work' command, or a bounded
   number of iterations in a test) or on a background thread via
   start-worker.

   @param max-iterations [integer or nil] Stop after this many ticks; nil
                                          (the default) loops forever, until
                                          stop-worker sets
                                          *worker-stop-requested*. Tests
                                          should always pass a small
                                          max-iterations rather than relying
                                          on stop-worker, to keep test runs
                                          bounded.
   @param poll-interval [number] Seconds to sleep between ticks that found no
                                 due job (default: 1)
   @return [null]
   "
  (loop for i from 0
        while (or (null max-iterations) (< i max-iterations))
        while (not *worker-stop-requested*)
        do (let ((result (run-worker-tick)))
             (when (eq result :idle)
               (sleep poll-interval))))
  nil)

(defun worker-running-p ()
  "@return [boolean] T if a worker thread started by start-worker is alive"
  (and *worker-thread*
       (bt:thread-alive-p *worker-thread*)))

(defun start-worker (&key (poll-interval 1))
  "Start the job worker on its own background thread.

   Uses the same connection-pool-per-thread model as the rest of clails
   (see clails/model/connection): the worker thread lazily acquires its own
   pooled connection the first time it claims or finalizes a job.

   @param poll-interval [number] Seconds to sleep between polls that found no
                                 due job (default: 1)
   @return [t] the started thread
   "
  (setf *worker-stop-requested* nil)
  (setf *worker-thread*
        (bt:make-thread (lambda () (run-worker-loop :poll-interval poll-interval))
                        :name "clails-job-worker"))
  (log.job "Job worker started" :poll-interval poll-interval)
  *worker-thread*)

(defun stop-worker ()
  "Signal the background worker thread to stop and wait for it to exit.

   @return [null]
   "
  (setf *worker-stop-requested* t)
  (when *worker-thread*
    (bt:join-thread *worker-thread*)
    (setf *worker-thread* nil)
    (log.job "Job worker stopped"))
  nil)
