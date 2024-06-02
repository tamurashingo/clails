;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:clails-model-postgresql-system
  (:use #:cl #:asdf))
(in-package #:clails-model-postgresql-system)

(defsystem clails-model-postgresql
  :class :package-inferred-system
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("cl-dbi"
               "clails-model"))

