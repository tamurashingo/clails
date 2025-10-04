(in-package #:cl-user)
(defpackage #:clails/view/view-helper
  (:use #:cl)
  (:export #:view
           #:*view-context*))

(in-package #:clails/view/view-helper)

;;; Special variable to hold current view context (plist)
(defvar *view-context* nil
  "Current view rendering context. Bound to the data plist during template execution.")

(defun view (key)
  "Access view data from current rendering context.
   Example: (view :user) returns the value associated with :user key."
  (getf *view-context* key))
