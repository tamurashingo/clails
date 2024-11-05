(in-package #:cl-user)
(defpackage #:clails/model/base-model
  (:use #:cl)
  (:export #:<base-model>))
(in-package #:clails/model/base-model)

(defclass <base-model> ()
  ((id
     :initarg :id
     :accessor id)
   (created-at
     :initarg :created-at
     :accessor created-at)
   (updated-at
     :initarg :updated-at
     :accessor updated-at)))
