;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:clails-model-impl-system
  (:use #:cl #:asdf))
(in-package #:clails-model-impl-system)

(defsystem clails-model-mysql
  :class :package-inferred-system
  :author "tamura shingo"
  :license "MIT"
  :pathname "src"
  :depends-on ("clails-model"
               "clails-model/impl/mysql"))


