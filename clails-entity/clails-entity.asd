;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package #:cl-user)
(defpackage #:clails-entity-system
  (:use #:cl #:asdf))
(in-package #:clails-entity-system)

(defsystem clails-entity
  :author "tamura shingo"
  :license "MIT"
  :depends-on ("closer-mop"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "migration")
                 (:file "base-entity"))))
  :description "clails entity")

