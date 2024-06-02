;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:clails-model-sqlite3-system
  (:use #:cl #:asdf))
(in-package #:clails-model-sqlite3-system)

(defsystem clails-model-sqlite3
  :class :package-inferred-system
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("cl-dbi"
               "clails-model"))

