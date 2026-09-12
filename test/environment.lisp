;;;; Test for environment resolution (src/environment.lisp)

(defpackage #:clails-test/environment
  (:use #:cl
        #:rove)
  (:import-from #:clails/environment
                #:*project-environment*
                #:set-environment
                #:resolve-project-environment))

(in-package #:clails-test/environment)

;; NOTE: rove runs setup/teardown once per test FILE, not once per deftest.
;; *project-environment* is shared, mutable global state, so each deftest
;; below resets it explicitly instead of relying on setup/teardown.

(deftest test-resolve-project-environment-default-only
  (testing "keeps the current default when no env-var or forced override is given"
    (setf *project-environment* :develop)
    (ok (eq :develop (resolve-project-environment)))
    (ok (eq :develop *project-environment*))))

(deftest test-resolve-project-environment-env-var-overrides-default
  (testing "an env-var value overrides the default"
    (setf *project-environment* :develop)
    (ok (eq :production (resolve-project-environment :env-var "production")))
    (ok (eq :production *project-environment*)))

  (testing "a nil env-var leaves the default in place"
    (setf *project-environment* :develop)
    (ok (eq :develop (resolve-project-environment :env-var nil)))
    (ok (eq :develop *project-environment*)))

  (testing "an invalid env-var value is ignored, default is kept"
    (setf *project-environment* :develop)
    (ok (eq :develop (resolve-project-environment :env-var "not-a-real-environment")))
    (ok (eq :develop *project-environment*))))

(deftest test-resolve-project-environment-forced-overrides-env-var-and-default
  (testing "a forced override wins over both default and env-var"
    (setf *project-environment* :develop)
    (ok (eq :test (resolve-project-environment :env-var "production" :forced "test")))
    (ok (eq :test *project-environment*)))

  (testing "a forced override wins over the default when no env-var is given"
    (setf *project-environment* :develop)
    (ok (eq :test (resolve-project-environment :forced "test")))
    (ok (eq :test *project-environment*))))

(deftest test-resolve-project-environment-independent-sequential-calls
  (testing "sequential calls mirror the real startup sequence: default set directly, then env-var, then a later forced override"
    (setf *project-environment* :develop)
    ;; step 1: default already set above (as app/config/environment.lisp would do)
    ;; step 2: clails.boot resolves against CLAILS_ENV
    (resolve-project-environment :env-var "test")
    (ok (eq :test *project-environment*))
    ;; step 3: the `test` command later forces :test regardless of the above
    (resolve-project-environment :forced "test")
    (ok (eq :test *project-environment*))))
