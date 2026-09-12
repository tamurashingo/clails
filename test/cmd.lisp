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

(deftest test-routes
  (testing "prints path and controller for each configured route"
    (let ((clails/environment:*routing-tables*
            '((:path "/"
               :controller "myapp/controllers/application-controller:<application-controller>")
              (:path "/users/:id"
               :controller "myapp/controllers/users-controller:<users-controller>"))))
      (let ((output (with-output-to-string (*standard-output*)
                      (clails/cmd:routes))))
        (ok (search "/" output))
        (ok (search "myapp/controllers/application-controller:<application-controller>" output))
        (ok (search "/users/:id" output))
        (ok (search "myapp/controllers/users-controller:<users-controller>" output)))))

  (testing "prints scanner and keys when configured"
    (let ((clails/environment:*routing-tables*
            '((:path "/spa/*"
               :controller "myapp/controllers/spa-controller:<spa-controller>"
               :scanner "^/spa/.*$")
              (:path "/static/*"
               :controller "myapp/controllers/static-controller:<static-controller>"
               :scanner "^/static/(.*)$"
               :keys ("filepath")))))
      (let ((output (with-output-to-string (*standard-output*)
                      (clails/cmd:routes))))
        (ok (search "^/spa/.*$" output))
        (ok (search "^/static/(.*)$" output))
        (ok (search "filepath" output))))))
