;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)

(defpackage #:clails-system
  (:use #:asdf #:cl))

(in-package #:clails-system)

(defsystem clails
  :class :package-inferred-system
  :version "0.0.3-alpha"
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
               "lack/middleware/static"
               "local-time"
               "str"
               "quri"
               "rove"
               "clails/main"
               "clails/test")
  :in-order-to ((test-op (test-op "clails-test"))))


