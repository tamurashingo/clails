(in-package #:cl-user)
(defpackage #:clails-test/view/view-context
  (:use #:cl
        #:rove
        #:clails-test/support/view)
  ;; The view namespace, imported the way a generated view package imports it
  ;; -- which is the first of the two calling conventions, and the reason the
  ;; templates below can say (current-session) rather than naming a package.
  (:import-from #:clails/view/view-helper
                #:view
                #:*view-context*
                #:*view-controller*
                #:view-context-error
                #:in-view-p
                #:current-controller
                #:current-request
                #:current-env
                #:current-session
                #:current-param)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:view-data
                #:view-package)
  (:import-from #:clails/view/view-resolver
                #:resolve-view)
  (:import-from #:lack/request
                #:request-method
                #:request-path-info))
(in-package #:clails-test/view/view-context)


;;; M7-T1: what a template can reach.
;;;
;;; The renderer wraps a template's execution in two dynamic bindings -- the
;;; data plist an action passed, and the controller whose action is being
;;; rendered -- and these are the tests of the second one: that it is in
;;; effect inside a rendering, that it is NOT in effect outside one, and that
;;; whether the action passed data makes no difference to it.
;;;
;;; The token half of the same binding is not tested here, because it cannot
;;; be: CSRF-TOKEN only works while the verification stage is handling the
;;; request, so it is tested from inside a stack in
;;; clails-test/view/form-helper and clails-test/view/form-helper-roundtrip.


(defparameter +this-package+ (find-package :clails-test/view/view-context)
  "The package the templates below are read in.  Passed explicitly: *PACKAGE*
   while a test runs is the runner's, not this file's.")

(defun render-here (template &key data controller)
  "Render TEMPLATE in this package."
  (render-template-string template
                          :data data
                          :controller controller
                          :package +this-package+))


;;; ------------------------------------------------------------------
;;; The binding is in effect inside a rendering
;;; ------------------------------------------------------------------

(deftest view-controller-binding-test
  (testing "a template can reach the controller being rendered"
    (let ((controller (make-view-controller)))
      (ok (string= (render-here "<%= (eq (current-controller) (view :self)) %>"
                                :data (list :self controller)
                                :controller controller)
                   "T")
          "(current-controller) is the controller RENDER was given")))

  (testing "a template can reach the request"
    (let ((controller (make-view-controller :uri "/todos/7" :method :get)))
      (ok (string= (render-here "<%= (request-path-info (current-request)) %>"
                                :controller controller)
                   "/todos/7"))
      (ok (string= (render-here "<%= (request-method (current-request)) %>"
                                :controller controller)
                   "GET"))))

  (testing "a template can reach the request environment"
    (let ((controller (make-view-controller :uri "/todos")))
      (ok (string= (render-here "<%= (getf (current-env) :path-info) %>"
                                :controller controller)
                   "/todos"))))

  (testing "a template can reach the session"
    (let ((controller (make-view-controller)))
      (setf (gethash "greeting" (controller-session controller)) "hello")
      (ok (string= (render-here "<%= (gethash \"greeting\" (current-session)) %>"
                                :controller controller)
                   "hello"))))

  (testing "the session a template sees is the controller's own session"
    ;; Not a copy: a template that reads a key an action has just written sees
    ;; the value, and the lifecycle API's reserved keys are in the same table.
    (let* ((controller (make-view-controller))
           (table (controller-session controller)))
      (ok (string= (render-here "<%= (eq (current-session) (view :table)) %>"
                                :data (list :table table)
                                :controller controller)
                   "T"))))

  (testing "a template can reach the request parameters"
    (let ((controller (make-view-controller :params '(("id" . "7")
                                                      ("title" . "milk")))))
      (ok (string= (render-here "<%= (current-param \"title\") %>"
                                :controller controller)
                   "milk"))
      (ok (string= (render-here "<%= (current-param \"id\") %>"
                                :controller controller)
                   "7"))
      (ok (string= (render-here "<%= (null (current-param \"absent\")) %>"
                                :controller controller)
                   "T"))))

  (testing "IN-VIEW-P is true inside a rendering"
    (let ((controller (make-view-controller)))
      (ok (string= (render-here "<%= (in-view-p) %>" :controller controller)
                   "T")))))


;;; ------------------------------------------------------------------
;;; With and without data passed by the action
;;; ------------------------------------------------------------------

(deftest view-controller-binding-independent-of-data-test
  ;; The point of binding the controller separately rather than merging it
  ;; into the data plist: the two are independent, so an action that passes
  ;; data and an action that passes none reach the request in exactly the same
  ;; way.
  (testing "the binding is in effect when the action passed data"
    (let ((controller (make-view-controller)))
      (setf (gethash "who" (controller-session controller)) "session")
      (ok (string= (render-here "<%= (view :who) %>/<%= (gethash \"who\" (current-session)) %>"
                                :data (list :who "data")
                                :controller controller)
                   "data/session"))))

  (testing "the binding is in effect when the action passed no data at all"
    (let ((controller (make-view-controller)))
      (setf (gethash "who" (controller-session controller)) "session")
      (ok (string= (render-here "<%= (gethash \"who\" (current-session)) %>"
                                :data nil
                                :controller controller)
                   "session"))))

  (testing "the data plist keeps its exact meaning, both ways"
    (let ((controller (make-view-controller)))
      (ok (string= (render-here "<%= (null (view :anything)) %>"
                                :data nil
                                :controller controller)
                   "T")
          "no data means (view :key) is NIL, as before")
      (ok (string= (render-here "<%= (view :title) %>"
                                :data (list :title "x")
                                :controller controller)
                   "x"))))

  (testing "an action's own :controller key is not overwritten by the framework"
    ;; Why the controller is NOT merged into the data plist: this key belongs
    ;; to the application.  A template reading (view :controller) gets what
    ;; the action put there, and still reaches the real controller through
    ;; (current-controller).
    (let ((controller (make-view-controller)))
      (ok (string= (render-here "<%= (view :controller) %>|<%= (eq (current-controller) (view :self)) %>"
                                :data (list :controller "mine" :self controller)
                                :controller controller)
                   "mine|T")))))


;;; ------------------------------------------------------------------
;;; The binding is NOT in effect outside a rendering
;;; ------------------------------------------------------------------

(deftest view-controller-binding-absent-outside-rendering-test
  (testing "outside any rendering the variable is NIL and IN-VIEW-P is false"
    (ok (null *view-controller*))
    (ok (not (in-view-p))))

  (testing "every reader signals VIEW-CONTEXT-ERROR outside a rendering"
    (dolist (thunk (list (lambda () (current-controller))
                         (lambda () (current-request))
                         (lambda () (current-env))
                         (lambda () (current-session))
                         (lambda () (current-param "id"))))
      (ok (signals (funcall thunk) 'view-context-error))))

  (testing "the error says what is missing"
    (ok (search "no view rendering in progress"
                (handler-case (progn (current-session) "")
                  (view-context-error (c) (princ-to-string c))))))

  (testing "RENDER without a controller renders, but the readers still signal"
    ;; A template rendered outside a request (a mail body, a test) is a
    ;; legitimate thing to do: the data plist works, and only the helpers that
    ;; need a request refuse.
    (ok (string= (render-here "<%= (view :title) %>" :data (list :title "x"))
                 "x"))
    (ok (string= (render-here "<%= (in-view-p) %>") "NIL"))
    (ok (signals (render-here "<%= (current-session) %>") 'view-context-error)))

  (testing "the binding does not outlive the rendering"
    (let ((controller (make-view-controller)))
      (render-here "<%= (in-view-p) %>" :controller controller)
      (ok (null *view-controller*) "unwound after RENDER returned")
      (ok (not (in-view-p)))))

  (testing "a rendering with no controller does not inherit an outer one"
    ;; RENDER binds the variable unconditionally, so a nested render that was
    ;; given no controller sees none -- it cannot silently answer with the
    ;; controller of whatever was rendering around it.
    (let ((controller (make-view-controller)))
      (let ((*view-controller* controller))
        (ok (string= (render-here "<%= (in-view-p) %>") "NIL"))))))


;;; ------------------------------------------------------------------
;;; The binding as the view resolver establishes it
;;; ------------------------------------------------------------------
;;;
;;; The tests above call RENDER directly.  These go through the one call site
;;; that has a controller to give, so that the wiring itself is covered and
;;; not just the renderer's argument.

(defclass <page-controller> (<web-controller>)
  ())

(deftest resolve-view-passes-the-controller-test
  (testing "RESOLVE-VIEW renders with the controller it was called on"
    (with-template (path "<%= (eq (current-controller) (view :self)) %>|<%= (gethash \"k\" (current-session)) %>")
      (let ((controller (make-view-controller :class '<page-controller>)))
        (setf (gethash "k" (controller-session controller)) "v")
        (setf (clails/controller/base-controller:view controller) path)
        (setf (view-package controller) :clails-test/view/view-context)
        (setf (view-data controller) (list :self controller))
        (destructuring-bind (status headers body) (resolve-view controller)
          (declare (ignore headers))
          (ok (eql status 200))
          (ok (string= (first body) "T|v"))))))

  (testing "and with no data passed, where (view :controller) is the old fallback"
    (with-template (path "<%= (eq (view :controller) (current-controller)) %>")
      (let ((controller (make-view-controller :class '<page-controller>)))
        (setf (clails/controller/base-controller:view controller) path)
        (setf (view-package controller) :clails-test/view/view-context)
        (ok (string= (first (third (resolve-view controller))) "T")
            "the pre-M7 fallback is untouched and agrees with the new binding")))))

(deftest resolve-view-without-a-view-test
  ;; The pitfall M1 reported: the VIEW slot had no initform, so an action that
  ;; called neither SET-VIEW nor SET-REDIRECT made RESOLVE-VIEW read an
  ;; unbound slot and the request died.  It is NIL now, which is the value
  ;; SET-REDIRECT already wrote and which the "nothing to render" branch of
  ;; RESOLVE-VIEW was always written for.
  (testing "an action that set no view answers an empty body, not an error"
    (let ((controller (make-view-controller :class '<page-controller>)))
      (destructuring-bind (status headers body) (resolve-view controller)
        (declare (ignore headers))
        (ok (eql status 200))
        (ok (equal body '(""))))))

  (testing "the slot reads as NIL on a fresh controller"
    (ok (null (clails/controller/base-controller:view
               (make-instance '<page-controller>))))))
