(in-package #:cl-user)
(defpackage #:clails-test/job/queue
  (:use #:cl
        #:rove)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool
                #:with-db-connection-direct)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate)
  (:import-from #:clails/job
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
                #:register-job
                #:clear-registry))

(defpackage #:clails-test/job/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/job/queue)


(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0014" "/app/test/data/0014-job-queue-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (db-create)
  (db-migrate)
  (startup-connection-pool))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (shutdown-connection-pool))


;;; Helper: rove only calls setup/teardown once per file, so each deftest
;;; clears the table (and job registry) itself.

(defun clean-jobs-table ()
  (with-db-connection-direct (connection)
    (dbi:do-sql connection "delete from clails_jobs")))

(defun text-value (value)
  "MySQL hands TEXT column values back as octet vectors, not strings."
  (typecase value
    ((vector (unsigned-byte 8)) (babel:octets-to-string value))
    (t value)))


(deftest enqueue-job-test
  (testing "enqueue-job persists a pending row with the given arguments"
    (clean-jobs-table)
    (clear-registry)

    (let ((id (enqueue-job :send-email :arguments (list :to "a@example.com" :subject "Hi"))))
      (ok (integerp id))

      (let ((job (claim-due-job)))
        (ok job)
        (ok (= id (job-plist-id job)))
        (ok (eq :send-email (job-plist-job-name job)))
        ;; arguments round-trip through JSON, which doesn't guarantee key
        ;; order, so compare values rather than list identity
        (ok (string= "a@example.com" (getf (job-plist-arguments job) :to)))
        (ok (string= "Hi" (getf (job-plist-arguments job) :subject)))
        (ok (= 1 (job-plist-attempts job)))
        (ok (= 25 (job-plist-max-attempts job)))))))

(deftest enqueue-job-uses-registered-max-attempts-test
  (testing "enqueue-job defaults :max-attempts to the job's registered default"
    (clean-jobs-table)
    (clear-registry)

    (register-job :limited-job :max-attempts 4 :function (lambda ()))
    (enqueue-job :limited-job)

    (let ((job (claim-due-job)))
      (ok (= 4 (job-plist-max-attempts job))))))

(deftest enqueue-job-explicit-max-attempts-overrides-test
  (testing "enqueue-job :max-attempts overrides the job's registered default"
    (clean-jobs-table)
    (clear-registry)

    (register-job :limited-job :max-attempts 4 :function (lambda ()))
    (enqueue-job :limited-job :max-attempts 9)

    (let ((job (claim-due-job)))
      (ok (= 9 (job-plist-max-attempts job))))))

(deftest claim-due-job-skips-future-jobs-test
  (testing "claim-due-job does not claim a job scheduled in the future"
    (clean-jobs-table)
    (clear-registry)

    (enqueue-job :future-job :run-at (+ (get-universal-time) 3600))

    (ok (null (claim-due-job)))))

(deftest claim-due-job-skips-already-running-jobs-test
  (testing "claim-due-job does not re-claim a job that is already running"
    (clean-jobs-table)
    (clear-registry)

    (enqueue-job :one-job)
    (let ((first-claim (claim-due-job)))
      (ok first-claim))

    (ok (null (claim-due-job)))))

(deftest claim-due-job-orders-by-available-at-test
  (testing "claim-due-job claims the most-overdue job first"
    (clean-jobs-table)
    (clear-registry)

    (let ((later-id (enqueue-job :later-job :run-at (- (get-universal-time) 10)))
          (earlier-id (enqueue-job :earlier-job :run-at (- (get-universal-time) 100))))
      (declare (ignore later-id))

      (let ((job (claim-due-job)))
        (ok (= earlier-id (job-plist-id job)))))))

(deftest mark-job-succeeded-test
  (testing "mark-job-succeeded transitions a claimed job to succeeded"
    (clean-jobs-table)
    (clear-registry)

    (enqueue-job :ok-job)
    (let* ((job (claim-due-job))
           (id (job-plist-id job)))
      (mark-job-succeeded id)

      (with-db-connection-direct (connection)
        (let ((row (dbi:fetch (dbi:execute (dbi:prepare connection "select status from clails_jobs where id = ?")
                                           (list id)))))
          (ok (string= "succeeded" (getf row :|status|))))))))

(deftest mark-job-retry-reschedules-with-backoff-test
  (testing "mark-job-retry sets status back to pending, records the error, and delays available_at"
    (clean-jobs-table)
    (clear-registry)

    (enqueue-job :flaky-job)
    (let* ((job (claim-due-job))
           (id (job-plist-id job))
           (before (get-universal-time)))
      (mark-job-retry id 1 "boom")

      (with-db-connection-direct (connection)
        (let ((row (dbi:fetch (dbi:execute (dbi:prepare connection "select status, last_error, available_at from clails_jobs where id = ?")
                                           (list id)))))
          (ok (string= "pending" (getf row :|status|)))
          (ok (string= "boom" (text-value (getf row :|last_error|))))
          ;; available_at should have moved into the future by roughly
          ;; backoff-seconds(1), i.e. *default-backoff-base-seconds*
          (ok (>= (getf row :|available_at|)
                  (+ before clails/job:*default-backoff-base-seconds*)))
          (ok (< (getf row :|available_at|)
                 (+ before clails/job:*default-backoff-base-seconds* 10)))))

      ;; the retried job should not be immediately re-claimable
      (ok (null (claim-due-job))))))

(deftest mark-job-failed-test
  (testing "mark-job-failed marks the job permanently failed with the error recorded"
    (clean-jobs-table)
    (clear-registry)

    (enqueue-job :doomed-job)
    (let* ((job (claim-due-job))
           (id (job-plist-id job)))
      (mark-job-failed id "fatal error")

      (with-db-connection-direct (connection)
        (let ((row (dbi:fetch (dbi:execute (dbi:prepare connection "select status, last_error from clails_jobs where id = ?")
                                           (list id)))))
          (ok (string= "failed" (getf row :|status|)))
          (ok (string= "fatal error" (text-value (getf row :|last_error|))))))

      (ok (null (claim-due-job))))))

(deftest backoff-seconds-grows-exponentially-and-is-capped-test
  (testing "backoff-seconds doubles per attempt and is capped at the configured max"
    (ok (= clails/job:*default-backoff-base-seconds* (backoff-seconds 1)))
    (ok (= (* 2 clails/job:*default-backoff-base-seconds*) (backoff-seconds 2)))
    (ok (= (* 4 clails/job:*default-backoff-base-seconds*) (backoff-seconds 3)))
    (ok (<= (backoff-seconds 100) clails/job:*default-backoff-max-seconds*))))
