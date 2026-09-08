;;;; Test for detect-server-backend (src/cmd.lisp)

(defpackage #:clails-test/cmd
  (:use #:cl
        #:rove))

(in-package #:clails-test/cmd)

(deftest test-detect-server-backend
  (testing "uses the sole declared clack-handler-* dependency"
    (asdf:defsystem "clails-test-fake-app-single"
      :depends-on ("clails" "clack-handler-woo"))
    (ok (eq (clails/cmd::detect-server-backend "clails-test-fake-app-single") :woo)))

  (testing "prefers whichever clack-handler-* is declared first"
    (asdf:defsystem "clails-test-fake-app-order-a"
      :depends-on ("clails" "clack-handler-woo" "clack-handler-hunchentoot"))
    (ok (eq (clails/cmd::detect-server-backend "clails-test-fake-app-order-a") :woo))

    (asdf:defsystem "clails-test-fake-app-order-b"
      :depends-on ("clails" "clack-handler-hunchentoot" "clack-handler-woo"))
    (ok (eq (clails/cmd::detect-server-backend "clails-test-fake-app-order-b") :hunchentoot)))

  (testing "returns nil when no clack-handler-* dependency is declared"
    (asdf:defsystem "clails-test-fake-app-none"
      :depends-on ("clails" "swank"))
    (ok (null (clails/cmd::detect-server-backend "clails-test-fake-app-none")))))
