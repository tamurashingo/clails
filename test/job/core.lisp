;;;; Test for Job Core (defjob)

(defpackage #:clails-test/job/core
  (:use #:cl
        #:rove
        #:clails/job))

(in-package #:clails-test/job/core)

(deftest test-defjob-registers-job
  (testing "defjob registers a job with the given name and options"
    (clear-registry)

    (defjob :greet
      :description "Print a greeting"
      :max-attempts 3
      :function (lambda (&key name) (format nil "Hello, ~A!" name)))

    (let ((job (find-job :greet)))
      (ok job)
      (ok (eq :greet (job-info-name job)))
      (ok (string= "Print a greeting" (job-info-description job)))
      (ok (= 3 (job-info-max-attempts job)))
      (ok (string= "Hello, World!" (funcall (job-info-function job) :name "World"))))))

(deftest test-defjob-default-max-attempts
  (testing "defjob without :max-attempts defaults to 25"
    (clear-registry)

    (defjob :no-options :function (lambda ()))
    (ok (= 25 (job-info-max-attempts (find-job :no-options))))))
