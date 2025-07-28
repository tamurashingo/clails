;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)

(defpackage #:clails-system
  (:use #:asdf #:cl))

(in-package #:clails-system)

(defsystem clails
  :class :package-inferred-system
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("anaphora"
               "babel"
               "cl-dbi-connection-pool"
               "cl-ppcre"
               "cl-template"
               "clack"
               "getcmd"
               "lack"
               "local-time"
               "str"
               "clails/main"))


