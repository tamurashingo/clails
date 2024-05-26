;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)

(defpackage #:clails-cli-system
  (:use #:asdf #:cl))

(in-package #:clails-cli-system)

(defsystem clails-cli
  :class :package-inferred-system
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("cl-template"
               "clails-cli/main"))
