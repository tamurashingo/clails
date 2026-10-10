(in-package #:cl-user)
(defpackage #:clails-test/job/worker
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
                #:defjob
                #:register-job
                #:clear-registry
                #:run-worker-loop
                #:run-worker-tick))

;;; Redeclared independently in each test file that migrates the clails_jobs
;;; table (this mirrors test/model/connection.lisp and
;;; test/model/migration.lisp both independently declaring
;;; clails-test/model/db): package-inferred-system loads each test file as
;;; its own component, so the package the migration file (in
;;; test/data/0014-job-queue-test/db/migrate/) is loaded into must already
;;; exist regardless of which of these test files runs first.
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

(in-package #:clails-test/job/worker)


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


;;; rove only calls setup/teardown once per file, so each deftest cleans up
;;; after itself.

(defun clean-jobs-table ()
  (with-db-connection-direct (connection)
    (dbi:do-sql connection "delete from clails_jobs")))

(defun text-value (value)
  "MySQL hands TEXT column values back as octet vectors, not strings."
  (typecase value
    ((vector (unsigned-byte 8)) (babel:octets-to-string value))
    (t value)))

(defun job-row (id)
  (with-db-connection-direct (connection)
    (dbi:fetch (dbi:execute (dbi:prepare connection
                                        "select status, attempts, last_error from clails_jobs where id = ?")
                            (list id)))))


(deftest worker-runs-a-successful-job-test
  (testing "run-worker-loop claims and successfully runs a due job"
    (clean-jobs-table)
    (clear-registry)

    (let ((seen nil))
      (defjob :say-hello
        :function (lambda (&key name) (setf seen name)))

      (let ((id (enqueue-job :say-hello :arguments (list :name "clails"))))
        (run-worker-loop :max-iterations 5 :poll-interval 0.05)

        (ok (string= "clails" seen))
        (let ((row (job-row id)))
          (ok (string= "succeeded" (getf row :|status|)))
          (ok (= 1 (getf row :|attempts|))))))))

(deftest worker-retries-then-succeeds-test
  (testing "a job that fails twice then succeeds ends up succeeded, having retried with backoff"
    (clean-jobs-table)
    (clear-registry)

    (let ((clails/job:*default-backoff-base-seconds* 0)
          (attempt-count 0))
      (defjob :flaky
        :max-attempts 5
        :function (lambda ()
                    (incf attempt-count)
                    (when (< attempt-count 3)
                      (error "still flaky (attempt ~A)" attempt-count))
                    :done))

      (let ((id (enqueue-job :flaky)))
        (run-worker-loop :max-iterations 20 :poll-interval 0.05)

        (ok (= 3 attempt-count))
        (let ((row (job-row id)))
          (ok (string= "succeeded" (getf row :|status|)))
          (ok (= 3 (getf row :|attempts|))))))))

(deftest worker-fails-permanently-after-max-attempts-test
  (testing "a job that always fails is marked failed once max-attempts is reached, with the error recorded"
    (clean-jobs-table)
    (clear-registry)

    (let ((clails/job:*default-backoff-base-seconds* 0)
          (attempt-count 0))
      (defjob :always-broken
        :max-attempts 3
        :function (lambda ()
                    (incf attempt-count)
                    (error "boom ~A" attempt-count)))

      (let ((id (enqueue-job :always-broken)))
        (run-worker-loop :max-iterations 20 :poll-interval 0.05)

        (ok (= 3 attempt-count))
        (let ((row (job-row id)))
          (ok (string= "failed" (getf row :|status|)))
          (ok (= 3 (getf row :|attempts|)))
          (ok (search "boom 3" (text-value (getf row :|last_error|)))))))))

(deftest worker-tick-is-idle-when-nothing-due-test
  (testing "run-worker-tick returns :idle when there is no due job"
    (clean-jobs-table)
    (clear-registry)

    (ok (eq :idle (run-worker-tick)))))

(deftest worker-handles-unregistered-job-name-test
  (testing "a job whose name is not (yet) registered is retried like any other failure, then fails permanently"
    (clean-jobs-table)
    (clear-registry)

    (let ((clails/job:*default-backoff-base-seconds* 0))
      (let ((id (enqueue-job :nobody-home :max-attempts 2)))
        (run-worker-loop :max-iterations 10 :poll-interval 0.05)

        (let ((row (job-row id)))
          (ok (string= "failed" (getf row :|status|)))
          (ok (= 2 (getf row :|attempts|)))
          (ok (search "No job registered" (text-value (getf row :|last_error|)))))))))
