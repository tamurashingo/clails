;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)

(defpackage #:clails-system
  (:use #:asdf #:cl))

(in-package #:clails-system)

(defsystem clails
  :class :package-inferred-system
  :version "0.1.0"
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("anaphora"
               "babel"
               "cl-fad"
               "cl-dbi-connection-pool"
               "cl-ppcre"
               "cl-template"
               "clack"
               "getcmd"
               "jonathan"
               "lack"
               ;; lack-middleware-csrf and lack-middleware-session are both
               ;; ordinary (non-package-inferred) systems defining several
               ;; packages each; they are named here, in the hyphenated
               ;; spelling, and imported from a single package apiece.  See the
               ;; note at the top of src/middleware/csrf-middleware.lisp.
               "lack-middleware-csrf"
               "lack-middleware-session"
               "lack/middleware/static"
               "local-time"
               "str"
               "quri"
               "rove"
               "clails/main"
               "clails/test")
  :in-order-to ((test-op (test-op "clails-test"))))


