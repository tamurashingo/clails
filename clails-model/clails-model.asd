;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:clails-model-system
  (:use #:cl #:asdf))
(in-package #:clails-model-system)

(defsystem clails-model
  :class :package-inferred-system
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("closer-mop"
               "cl-ppcre"
               "clails-model/migration"
               "clails-model/base-model")
  :description "clails model")

