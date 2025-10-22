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
   
   Retrieves the value associated with the given key from the current
   view context. Must be called within a view template rendering scope.
   
   @param key [keyword] Key to look up in the view context
   @return [t] Value associated with the key, or NIL if not found
   "
  (getf *view-context* key))
