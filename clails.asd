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
               "cl-ppcre"
               "cl-dbi-connection-pool"
               "cl-template"
               "closer-mop"
               "clails/main"
               "dbi-cp"))


