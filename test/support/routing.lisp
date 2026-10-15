(in-package #:cl-user)
(defpackage #:clails-test/support/routing
  (:use #:cl)
  (:import-from #:clails/environment
                #:*routing-tables*)
  (:import-from #:clails/controller/base-controller
                #:initialize-routing-tables)
  (:export #:with-routing-table
           #:current-router
           #:router-paths))
(in-package #:clails-test/support/routing)


(defmacro with-routing-table ((routes &key (silence-warnings t)) &body body)
  "Install ROUTES as the routing table for the dynamic extent of BODY.

   Both *ROUTING-TABLES* (the source table) and the compiled router are
   rebound with LET, and INITIALIZE-ROUTING-TABLES is called inside those
   bindings, so the table a test installs cannot leak into another test and
   the table a test finds on entry is whatever the suite had globally.

   ROUTES is an ordinary list of route plists -- hand-written entries, or the
   output of CLAILS/CONTROLLER/BASE-CONTROLLER:RESOURCES, or both appended.

   INITIALIZE-ROUTING-TABLES prints a deprecation warning for every route
   without :action.  Those warnings are swallowed by default so that a test
   installing legacy routes does not pollute the test output; pass
   :silence-warnings nil to see them (or to capture them yourself)."
  `(let ((*routing-tables* ,routes)
         (clails/controller/base-controller::*router* nil))
     (let ((*error-output* (if ,silence-warnings
                               (make-string-output-stream)
                               *error-output*)))
       (initialize-routing-tables))
     ,@body))

(defun current-router ()
  "The compiled routing table currently in effect."
  clails/controller/base-controller::*router*)

(defun router-paths ()
  "The :path of every route in the compiled routing table, in order."
  (mapcar (lambda (route) (getf route :path)) (current-router)))
