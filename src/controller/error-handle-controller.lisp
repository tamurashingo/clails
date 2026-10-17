(in-package #:cl-user)
(defpackage #:clails/controller/error-handle-controller
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>)
  (:export #:<error-handle-controller>))

(in-package #:clails/controller/error-handle-controller)

;;; NOTE: nothing instantiates this class.  It is imported by
;;; clails/middleware/clails-middleware and dispatched on by
;;; clails/view/view-resolver:resolve-view, but no code path ever reaches
;;; MAKE-INSTANCE, so no request is answered through it today.
;;;
;;; It was deliberately left that way by M2 ("being able to return a 403"):
;;; a stage that rejects a request runs outside the controller stage, where
;;; there is neither a controller nor a resolved route, so it answers with a
;;; response triple of its own instead -- see src/middleware/rejection.lisp.
;;; Giving this class a caller means deciding how error pages are rendered in
;;; general (the resolve-view method for it still answers a hard-coded
;;; "error" body and ignores the condition's template-name), i.e. a
;;; request-wide condition-to-response mechanism.  That is a separate piece
;;; of work and not a prerequisite for rejecting a request.
(defclass <error-handle-controller> (<web-controller>)
  ((exception :initarg :exception
              :reader exception
              :documentation "Exception object that was raised"))
  (:documentation "Controller for handling and displaying error pages."))

