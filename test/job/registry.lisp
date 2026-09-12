;;;; Test for Job Registry

(defpackage #:clails-test/job/registry
  (:use #:cl
        #:rove
        #:clails/job))

(in-package #:clails-test/job/registry)

(deftest test-register-and-find-job
  (testing "Register a job and find it"
    (clear-registry)

    (let ((key (register-job :send-email
                             :description "Send an email"
                             :max-attempts 7
                             :function (lambda (&key to) (declare (ignore to))))))
      (ok (eq :send-email key))

      (let ((job (find-job :send-email)))
        (ok job)
        (ok (eq :send-email (job-info-name job)))
        (ok (string= "Send an email" (job-info-description job)))
        (ok (= 7 (job-info-max-attempts job)))))))

(deftest test-register-job-default-max-attempts
  (testing "Registering a job without :max-attempts defaults to 25"
    (clear-registry)

    (register-job :some-job :function (lambda ()))
    (ok (= 25 (job-info-max-attempts (find-job :some-job))))))

(deftest test-job-exists-p
  (testing "Check if job exists"
    (clear-registry)

    (register-job :foo :function (lambda () nil))

    (ok (job-exists-p :foo))
    (ok (not (job-exists-p :nonexistent)))))

(deftest test-list-jobs
  (testing "List all jobs"
    (clear-registry)

    (register-job :job1 :function (lambda () nil))
    (register-job :job2 :function (lambda () nil))
    (register-job :job3 :function (lambda () nil))

    (ok (= 3 (length (list-jobs))))))

(deftest test-remove-job
  (testing "Remove a job"
    (clear-registry)

    (register-job :to-remove :function (lambda () nil))
    (ok (job-exists-p :to-remove))

    (remove-job :to-remove)
    (ok (not (job-exists-p :to-remove)))))

(deftest test-clear-registry
  (testing "Clear all jobs"
    (clear-registry)

    (register-job :job1 :function (lambda () nil))
    (register-job :job2 :function (lambda () nil))
    (ok (= 2 (length (list-jobs))))

    (clear-registry)
    (ok (= 0 (length (list-jobs))))))
