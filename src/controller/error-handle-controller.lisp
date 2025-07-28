(in-package #:cl-user)
(defpackage #:clails/controller/error-handle-controller
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>)
  (:export #:<error-handle-controller>))

(in-package #:clails/controller/error-handle-controller)

(defclass <error-handle-controller> (<web-controller>)
  ((exception :initarg :exception
              :reader exception)))

