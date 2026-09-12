(in-package #:cl-user)
(defpackage #:clails-test/environment
  (:use #:cl
        #:rove)
  (:import-from #:clails/environment
                #:*startup-hooks*
                #:*shutdown-hooks*
                #:add-startup-hook
                #:add-shutdown-hook))
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
