(in-package #:cl-user)
(defpackage #:clails-test/environment
  (:use #:cl
        #:rove)
  (:import-from #:clails/environment
                #:*startup-hooks*
                #:*shutdown-hooks*
                #:*project-environment*
                #:add-startup-hook
                #:add-shutdown-hook
                #:set-environment
                #:resolve-project-environment))
(in-package #:clails-test/environment)

(defparameter *saved-startup-hooks* nil)
(defparameter *saved-shutdown-hooks* nil)

(setup
  (setf *saved-startup-hooks* *startup-hooks*)
  (setf *saved-shutdown-hooks* *shutdown-hooks*))

(teardown
  (setf *startup-hooks* *saved-startup-hooks*)
  (setf *shutdown-hooks* *saved-shutdown-hooks*))


;;; Test: add-startup-hook / add-shutdown-hook append in registration order

(deftest add-startup-hook-appends-in-registration-order-test
  (testing "add-startup-hook appends to *startup-hooks*, preserving registration order"
    (setf *startup-hooks* nil)
    (add-startup-hook "hook-a")
    (add-startup-hook "hook-b")
    (add-startup-hook "hook-c")
    (ok (equal '("hook-a" "hook-b" "hook-c") *startup-hooks*)
        "hooks should appear in the same order they were registered, not reversed")))

(deftest add-shutdown-hook-appends-in-registration-order-test
  (testing "add-shutdown-hook appends to *shutdown-hooks*, preserving registration order"
    (setf *shutdown-hooks* nil)
    (add-shutdown-hook "hook-x")
    (add-shutdown-hook "hook-y")
    (ok (equal '("hook-x" "hook-y") *shutdown-hooks*)
        "hooks should appear in the same order they were registered, not reversed")))

(deftest add-startup-hook-runs-after-existing-default-hook-test
  (testing "a hook added via add-startup-hook runs after a hook already present"
    (setf *startup-hooks* (list "clails/model/connection:startup-connection-pool"))
    (add-startup-hook "project/config/logger:initialize-logger")
    (ok (equal '("clails/model/connection:startup-connection-pool"
                 "project/config/logger:initialize-logger")
               *startup-hooks*)
        "the framework's own default hook should stay first, with newly added hooks following it")))


;;; Test: clails/cmd:call-startup-hooks / call-shutdown-hooks execute in list order

(deftest call-startup-hooks-executes-in-registration-order-test
  (testing "call-startup-hooks runs hooks in the order they were registered"
    (setf *startup-hooks* nil)
    (let ((order '()))
      (add-startup-hook (lambda () (setf order (append order (list :first)))))
      (add-startup-hook (lambda () (setf order (append order (list :second)))))
      (add-startup-hook (lambda () (setf order (append order (list :third)))))
      (clails/cmd::call-startup-hooks)
      (ok (equal '(:first :second :third) order)
          "hooks should run first-registered-first, matching add-startup-hook's registration order"))))

(deftest call-shutdown-hooks-executes-in-registration-order-test
  (testing "call-shutdown-hooks runs hooks in the order they were registered"
    (setf *shutdown-hooks* nil)
    (let ((order '()))
      (add-shutdown-hook (lambda () (setf order (append order (list :first)))))
      (add-shutdown-hook (lambda () (setf order (append order (list :second)))))
      (clails/cmd::call-shutdown-hooks)
      (ok (equal '(:first :second) order)
          "hooks should run first-registered-first, matching add-shutdown-hook's registration order"))))


;;; Test: resolve-project-environment

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
