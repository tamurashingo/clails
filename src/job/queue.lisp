;;;; Job Queue
;;;; DB-persisted job queue: enqueue, claim, and finalize jobs.
;;;;
;;;; Jobs are stored in a plain database table (see
;;;; template/generate/job-queue-migration.lisp.tmpl for the schema) rather
;;;; than as a clails/model/base-model:defmodel-backed model. A defmodel
;;;; class is introspected from the database the moment
;;;; initialize-table-information runs (typically at server/console/worker
;;;; boot), for every registered model -- which would mean every clails
;;;; application would need the jobs table to exist just to boot, even if it
;;;; never uses the job queue. Talking to the table with plain SQL (the same
;;;; approach clails/model/migration itself uses for its own "migration"
;;;; bookkeeping table) keeps the feature fully opt-in: nothing breaks until
;;;; the application actually calls enqueue-job/starts a worker, at which
;;;; point the table is expected to exist (see document/job-queue.md).
;;;;
;;;; Claiming a due job still reuses the same pessimistic-locking primitives
;;;; documented in document/model.md (SELECT ... FOR UPDATE for
;;;; MySQL/PostgreSQL; BEGIN IMMEDIATE, the same mechanism
;;;; clails/model/lock:with-locked-transaction relies on, for SQLite3) so
;;;; that only one worker can ever claim a given row.

(defpackage #:clails/job/queue
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*
                #:<database-type-mysql>
                #:<database-type-postgresql>
                #:<database-type-sqlite3>
                #:*sqlite3-transaction-mode*)
  (:import-from #:clails/model/connection
                #:get-connection
                #:with-db-connection)
  (:import-from #:clails/model/transaction
                #:with-transaction)
  (:import-from #:clails/model/query
                #:get-last-id-impl)
  (:import-from #:clails/job/registry
                #:find-job
                #:job-info-max-attempts)
  (:import-from #:clails/logger/core
                #:log.job)
  (:import-from #:jonathan
                #:to-json
                #:parse)
  (:export #:*jobs-table-name*
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
           #:job-plist-max-attempts))

(in-package #:clails/job/queue)

(defparameter *jobs-table-name* "clails_jobs"
  "Name of the framework-provided job queue table. See
   template/generate/job-queue-migration.lisp.tmpl.")

(defparameter *default-max-attempts* 25
  "Default number of attempts (including the first) before a failing job is
   marked :failed instead of retried, used when neither enqueue-job nor the
   job's defjob specify :max-attempts.")

(defparameter *default-backoff-base-seconds* 5
  "Base delay, in seconds, for the exponential backoff applied between retry
   attempts: delay = base * 2^(attempt-number - 1), capped at
   *default-backoff-max-seconds*.")

(defparameter *default-backoff-max-seconds* 3600
  "Upper bound, in seconds, on the exponential backoff delay between retries.")


;;;; ----------------------------------------
;;;; datetime helpers
;;;;
;;;; The clails ORM stores DATETIME/TIMESTAMP values as Lisp universal-time
;;;; integers and converts them to/from "YYYY-MM-DD HH:MM:SS" strings inside
;;;; each database backend's per-column :cl-db-fn/:db-cl-fn (see
;;;; src/model/impl/{mysql,postgresql,sqlite3}.lisp). That machinery is keyed
;;;; off a model's introspected column list, which the raw-SQL job queue
;;;; does not have, so the same conversion is reimplemented here directly.

(defun ut->db-string (universal-time)
  "Format a universal-time integer as a \"YYYY-MM-DD HH:MM:SS\" string
   suitable for binding to a DATETIME/TIMESTAMP column on any supported
   backend.

   @param universal-time [integer] Universal time
   @return [string] Formatted datetime string
   "
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour min sec)))

(defun db-value->ut (value)
  "Normalize a DATETIME/TIMESTAMP column value fetched from any supported
   backend to a universal-time integer.

   MySQL/PostgreSQL drivers hand back a universal-time integer already;
   SQLite3 hands back the raw \"YYYY-MM-DD HH:MM:SS\" string.

   @param value [integer, string, or nil] Raw fetched column value
   @return [integer or nil] Universal time
   "
  (cond
    ((null value) nil)
    ((integerp value) value)
    ((stringp value)
     (encode-universal-time (parse-integer value :start 17 :end 19)
                            (parse-integer value :start 14 :end 16)
                            (parse-integer value :start 11 :end 13)
                            (parse-integer value :start 8 :end 10)
                            (parse-integer value :start 5 :end 7)
                            (parse-integer value :start 0 :end 4)))
    (t (error "Unexpected datetime value fetched from ~A: ~S" *jobs-table-name* value))))

(defun job-name->db-string (job-name)
  (string-downcase (string job-name)))

(defun db-string->job-name (string)
  (intern (string-upcase string) :keyword))

(defun normalize-text-value (value)
  "MySQL's driver hands TEXT/BLOB column values back as raw
   (vector (unsigned-byte 8)) octets rather than Lisp strings (the same
   thing src/model/impl/mysql.lisp's :text :db-cl-fn works around for
   model-backed columns); PostgreSQL/SQLite3 already hand back strings.
   Normalize either to a string.

   @param value [string, octet-vector, or nil] Raw fetched TEXT column value
   @return [string or nil]
   "
  (typecase value
    ((vector (unsigned-byte 8)) (babel:octets-to-string value))
    (t value)))

(defun encode-arguments (arguments)
  (to-json (or arguments '()) :from :plist))

(defun decode-arguments (json)
  (let ((json (normalize-text-value json)))
    (if (and json (> (length json) 0))
        (parse json :as :plist)
        nil)))


;;;; ----------------------------------------
;;;; row <-> plist

(defun row->job-plist (row)
  "Convert a raw database row (plist with snake_case keys, as returned by
   dbi-cp:fetch) into a normalized job plist.

   @param row [plist] Raw row
   @return [plist] Normalized job plist with keys :id :job-name :arguments
                   :status :attempts :max-attempts :available-at :last-error
                   :created-at :updated-at
   "
  (list :id (getf row :|id|)
        :job-name (db-string->job-name (getf row :|job_name|))
        :arguments (decode-arguments (getf row :|arguments|))
        :status (getf row :|status|)
        :attempts (getf row :|attempts|)
        :max-attempts (getf row :|max_attempts|)
        :available-at (db-value->ut (getf row :|available_at|))
        :last-error (normalize-text-value (getf row :|last_error|))
        :created-at (db-value->ut (getf row :|created_at|))
        :updated-at (db-value->ut (getf row :|updated_at|))))

(defun job-plist-id (job) (getf job :id))
(defun job-plist-job-name (job) (getf job :job-name))
(defun job-plist-arguments (job) (getf job :arguments))
(defun job-plist-attempts (job) (getf job :attempts))
(defun job-plist-max-attempts (job) (getf job :max-attempts))


;;;; ----------------------------------------
;;;; enqueue

(defun enqueue-job (job-name &key arguments max-attempts run-at)
  "Persist a new job to the queue.

   @param job-name [keyword] Job name (see defjob)
   @param arguments [plist] Arguments passed to the job handler when it runs
   @param max-attempts [integer or nil] Maximum attempts before the job is
                                        marked :failed. When nil, uses the
                                        max-attempts the job was registered
                                        with via defjob, falling back to
                                        *default-max-attempts* if the job
                                        isn't registered (yet) in this
                                        process.
   @param run-at [integer or nil] Universal time at/after which the job
                                  becomes eligible to run. Defaults to now
                                  (run as soon as a worker is free).
   @return [integer] id of the newly inserted job row
   "
  (let* ((now (get-universal-time))
         (job-info (find-job job-name))
         (resolved-max-attempts (or max-attempts
                                    (and job-info (job-info-max-attempts job-info))
                                    *default-max-attempts*))
         (available-at (ut->db-string (or run-at now)))
         (now-string (ut->db-string now))
         (arguments-json (encode-arguments arguments))
         (sql (format nil "INSERT INTO ~A (job_name, arguments, status, attempts, max_attempts, available_at, last_error, created_at, updated_at) VALUES (?, ?, 'pending', 0, ?, ?, NULL, ?, ?)"
                     *jobs-table-name*)))
    (with-db-connection (connection)
      (dbi-cp:execute (dbi-cp:prepare connection sql)
                      (list (job-name->db-string job-name)
                            arguments-json
                            resolved-max-attempts
                            available-at
                            now-string
                            now-string))
      (let ((id (get-last-id-impl *database-type* connection)))
        (log.job "Job enqueued" :job-name job-name :job-id id :available-at available-at)
        id))))


;;;; ----------------------------------------
;;;; claim (pessimistic-locking select)

(defun select-for-update-clause ()
  "Lock clause appended to the claim SELECT for backends with row-level
   locking. Mirrors the modes documented for
   clails/model/lock:with-locked-transaction. SQLite3 has no row-level lock;
   it is handled instead by running the claim inside a BEGIN IMMEDIATE
   transaction (see claim-due-job)."
  (cond
    ((typep *database-type* '<database-type-mysql>) "FOR UPDATE SKIP LOCKED")
    ((typep *database-type* '<database-type-postgresql>) "FOR UPDATE SKIP LOCKED")
    (t nil)))

(defun claim-due-job ()
  "Atomically claim the single most-overdue pending job, if any.

   Runs a SELECT ... FOR UPDATE SKIP LOCKED (MySQL/PostgreSQL) or a claim
   inside a BEGIN IMMEDIATE transaction (SQLite3) followed by an UPDATE that
   marks the row :running and increments its attempt count, all within one
   transaction, so that concurrent workers never claim the same row.

   @return [plist or nil] The claimed job (see row->job-plist), or nil if no
                          job is currently due
   "
  (flet ((do-claim ()
           (let* ((connection (get-connection))
                  (now-string (ut->db-string (get-universal-time)))
                  (lock-clause (select-for-update-clause))
                  (select-sql (format nil "SELECT * FROM ~A WHERE status = 'pending' AND available_at <= ? ORDER BY available_at ASC, id ASC LIMIT 1~@[ ~A~]"
                                      *jobs-table-name* lock-clause))
                  (row (dbi-cp:fetch (dbi-cp:execute (dbi-cp:prepare connection select-sql)
                                                     (list now-string)))))
             (when row
               (let ((id (getf row :|id|)))
                 (dbi-cp:execute
                  (dbi-cp:prepare connection
                                  (format nil "UPDATE ~A SET status = 'running', attempts = attempts + 1, updated_at = ? WHERE id = ?"
                                          *jobs-table-name*))
                  (list now-string id))
                 ;; Reflect the attempt increment we just performed without a
                 ;; second round-trip.
                 (incf (getf row :|attempts|))
                 (let ((job (row->job-plist row)))
                   (log.job "Job claimed" :job-name (job-plist-job-name job) :job-id id
                            :attempt (job-plist-attempts job))
                   job))))))
    (if (typep *database-type* '<database-type-sqlite3>)
        (let ((*sqlite3-transaction-mode* :immediate))
          (with-transaction (do-claim)))
        (with-transaction (do-claim)))))


;;;; ----------------------------------------
;;;; finalize

(defun mark-job-succeeded (job-id)
  "Mark a claimed job as :succeeded.

   @param job-id [integer] id of the job row
   @return [null]
   "
  (with-transaction
    (dbi-cp:execute
     (dbi-cp:prepare (get-connection)
                     (format nil "UPDATE ~A SET status = 'succeeded', last_error = NULL, updated_at = ? WHERE id = ?"
                             *jobs-table-name*))
     (list (ut->db-string (get-universal-time)) job-id)))
  (log.job "Job succeeded" :job-id job-id)
  nil)

(defun backoff-seconds (attempt-number &key (base *default-backoff-base-seconds*)
                                            (max *default-backoff-max-seconds*))
  "Compute the exponential backoff delay, in seconds, before retrying after
   the ATTEMPT-NUMBERth attempt has failed.

   @param attempt-number [integer] 1-based attempt number that just failed
   @param base [integer] Base delay in seconds
   @param max [integer] Upper bound on the delay in seconds
   @return [integer] Delay in seconds
   "
  (min max (* base (expt 2 (max 0 (1- attempt-number))))))

(defun mark-job-retry (job-id attempt-number error-message)
  "Reschedule a failed job for a later retry using exponential backoff.

   @param job-id [integer] id of the job row
   @param attempt-number [integer] 1-based attempt number that just failed
   @param error-message [string] Error message to record
   @return [null]
   "
  (let* ((delay (backoff-seconds attempt-number))
         (now (get-universal-time))
         (available-at (ut->db-string (+ now delay))))
    (with-transaction
      (dbi-cp:execute
       (dbi-cp:prepare (get-connection)
                       (format nil "UPDATE ~A SET status = 'pending', available_at = ?, last_error = ?, updated_at = ? WHERE id = ?"
                               *jobs-table-name*))
       (list available-at error-message (ut->db-string now) job-id)))
    (log.job "Job failed, will retry" :job-id job-id :attempt attempt-number
             :retry-in-seconds delay :error error-message))
  nil)

(defun mark-job-failed (job-id error-message)
  "Mark a job as permanently :failed (no further retries).

   @param job-id [integer] id of the job row
   @param error-message [string] Error message to record
   @return [null]
   "
  (with-transaction
    (dbi-cp:execute
     (dbi-cp:prepare (get-connection)
                     (format nil "UPDATE ~A SET status = 'failed', last_error = ?, updated_at = ? WHERE id = ?"
                             *jobs-table-name*))
     (list error-message (ut->db-string (get-universal-time)) job-id)))
  (log.job "Job failed permanently" :job-id job-id :error error-message)
  nil)
