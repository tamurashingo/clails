(in-package #:cl-user)
(defpackage #:clails/view/view-helper
  (:use #:cl)
  ;; The controller base is imported for its readers only: a view reaches the
  ;; request, the environment, the parameters and the session THROUGH the
  ;; controller that is being rendered, so that there is exactly one
  ;; definition of "the session of this request" (the controller's) and the
  ;; view cannot drift from it.
  ;;
  ;; NOTE: #:VIEW is deliberately NOT imported.  CLAILS/CONTROLLER/BASE-CONTROLLER
  ;; exports a VIEW accessor (the template a <web-controller> is going to
  ;; render) and this package defines its own VIEW function (the value an
  ;; action passed to the template).  They are two different things with one
  ;; name; importing the accessor here would be a package conflict.
  ;;
  ;; There is no dependency cycle: the controller base knows nothing about the
  ;; view layer (CLAILS/VIEW/VIEW-RESOLVER is what brings the two together,
  ;; and it sits above both).
  (:import-from #:clails/controller/base-controller
                #:request
                #:env
                #:param
                #:session)
  (:export #:view
           #:*view-context*
           #:*view-controller*
           #:view-context-error
           #:view-context-error-helper
           #:in-view-p
           #:current-controller
           #:current-request
           #:current-env
           #:current-session
           #:current-param))

(in-package #:clails/view/view-helper)

;;; ------------------------------------------------------------------
;;; What a template can reach, and how (M7-T1)
;;; ------------------------------------------------------------------
;;;
;;; A template is compiled into an ordinary function and called by
;;; CLAILS/VIEW/RENDERER:RENDER, which wraps that call in dynamic bindings.
;;; Everything a template can see besides its own lexical scope comes from one
;;; of those bindings, and there are two:
;;;
;;;   *VIEW-CONTEXT*     the plist an action passed to SET-VIEW -- the
;;;                      application's own data, read with (VIEW :key)
;;;   *VIEW-CONTROLLER*  the controller whose action is being rendered --
;;;                      the framework's own channel, read with
;;;                      (CURRENT-REQUEST), (CURRENT-SESSION), ...
;;;
;;; Why the controller is a binding of its own, and not a key in the plist
;;; ---------------------------------------------------------------------
;;; The alternative considered was to always put the controller into the data
;;; plist (today it is there only when an action passed NO data: see
;;; CLAILS/VIEW/VIEW-RESOLVER:EXTRACT-TEMPLATE, which falls back to
;;; (LIST :CONTROLLER CONTROLLER)).  A second binding was chosen instead:
;;;
;;;   * the data plist is the APPLICATION's namespace.  An action is free to
;;;     pass (:controller x) for its own reasons -- a page that renders a
;;;     list of controller names, say -- and with the fallback turned into an
;;;     unconditional merge, either the framework would silently overwrite
;;;     the action's value or the action would silently overwrite the
;;;     framework's.  Neither can be detected at the point where it goes
;;;     wrong;
;;;   * nothing could then guarantee the value IS a controller.  RENDER is
;;;     also called directly (by tests, and by anything that renders a
;;;     template outside a request), and a plist key carries no such
;;;     guarantee, while a binding established only by the renderer does;
;;;   * "the framework's channel" wants to fail LOUDLY outside a request.
;;;     (VIEW :controller) returning NIL outside rendering is indistinguishable
;;;     from "the action passed nothing", whereas (CURRENT-SESSION) signals
;;;     VIEW-CONTEXT-ERROR and says what is wrong -- which is what the
;;;     milestone asks for;
;;;   * it is strictly additive.  *VIEW-CONTEXT* keeps its exact meaning, so
;;;     every existing template -- including one that reads (VIEW :controller)
;;;     today -- renders unchanged.
;;;
;;; The fallback in EXTRACT-TEMPLATE is therefore left exactly as it was.
;;;
;;; Why the token is reachable from here at all
;;; -------------------------------------------
;;; Rendering happens INSIDE the CSRF verification stage (the controller stage
;;; is the innermost one; see CLAILS/MIDDLEWARE/CORE:*CLAILS-MIDDLEWARE-STACK*),
;;; so while a template runs, that stage's own per-request dynamic variables
;;; are still bound and CLAILS/MIDDLEWARE/CSRF-MIDDLEWARE:CSRF-TOKEN can be
;;; called.  That is why the form helpers of CLAILS/VIEW/FORM-HELPER work from
;;; a template and would not work from, say, a response filter placed outside
;;; the stage.

(defvar *view-context* nil
  "Current view rendering context: the data plist an action passed to
   SET-VIEW, read with (VIEW :key).

   Bound by CLAILS/VIEW/RENDERER:RENDER for the duration of one template
   execution, and NIL outside rendering.")

(defvar *view-controller* nil
  "The controller whose action is being rendered, or NIL outside rendering.

   Bound by CLAILS/VIEW/RENDERER:RENDER, in the same place and in the same
   manner as *VIEW-CONTEXT*, and bound independently of the data plist -- so
   it is available whether or not the action passed any data.

   Templates should not read this variable directly; the CURRENT-* readers
   below give a clearer error when there is no controller to read.")


;;; ------------------------------------------------------------------
;;; Reading the data an action passed
;;; ------------------------------------------------------------------

(defun view (key)
  "Access view data from current rendering context.

   Retrieves the value associated with the given key from the current
   view context. Must be called within a view template rendering scope.

   @param key [keyword] Key to look up in the view context
   @return [t] Value associated with the key, or NIL if not found
   "
  (getf *view-context* key))


;;; ------------------------------------------------------------------
;;; Reading the request being rendered
;;; ------------------------------------------------------------------

(define-condition view-context-error (error)
  ((helper :initarg :helper
           :initform nil
           :reader view-context-error-helper
           :documentation "Name of the helper that was called, when known."))
  (:report (lambda (condition stream)
             (format stream
                     "~@[~A: ~]there is no view rendering in progress. ~
                      The request, the session and the CSRF token are only ~
                      reachable from inside a template rendered for a ~
                      controller action; CLAILS/VIEW/RENDERER:RENDER has to ~
                      be given that controller (:CONTROLLER), which is what ~
                      CLAILS/VIEW/VIEW-RESOLVER:RESOLVE-VIEW does on every ~
                      request."
                     (view-context-error-helper condition))))
  (:documentation "Signalled when a view helper that needs the request is
   called outside a template rendering, i.e. while *VIEW-CONTROLLER* is NIL.

   This is the deliberate, explicit failure for \"the binding is not in
   effect here\": it names what is missing instead of letting an unbound slot
   or an unbound variable surface from somewhere deeper."))

(defun in-view-p ()
  "True when a template is being rendered for a controller action.

   Lets a helper that can do something useful without a request decide for
   itself, rather than having to catch VIEW-CONTEXT-ERROR.

   @return [boolean] T while *VIEW-CONTROLLER* holds a controller
   "
  (not (null *view-controller*)))

(defun current-controller (&optional (helper 'current-controller))
  "The controller whose action is being rendered.

   @param helper [symbol] Name reported by the error, when a caller wants its
     own name in the message instead of CURRENT-CONTROLLER's
   @return [<base-controller>] The controller being rendered
   @condition view-context-error Signalled outside a template rendering
   "
  (or *view-controller*
      (error 'view-context-error :helper helper)))

(defun current-request (&optional (helper 'current-request))
  "The request object of the request being rendered.

   @param helper [symbol] Name reported by the error (see CURRENT-CONTROLLER)
   @return [lack/request:request] The request being answered
   @condition view-context-error Signalled outside a template rendering
   "
  (request (current-controller helper)))

(defun current-env (&optional (helper 'current-env))
  "The Clack/Lack request environment plist of the request being rendered.

   @param helper [symbol] Name reported by the error (see CURRENT-CONTROLLER)
   @return [list] The request environment
   @condition view-context-error Signalled outside a template rendering
   "
  (env (current-controller helper)))

(defun current-session (&optional (helper 'current-session))
  "The session hash table of the request being rendered.

   Note that the session carries the framework's own reserved keys as well as
   the application's (they are the ones whose name starts with
   CLAILS/MIDDLEWARE/SESSION/LIFECYCLE:+SESSION-RESERVED-KEY-PREFIX+, e.g.
   the last-access timestamp and the CSRF token).  A template that walks the
   whole table -- a debug page, say -- will see them; writing to them from a
   view is never correct.

   @param helper [symbol] Name reported by the error (see CURRENT-CONTROLLER)
   @return [hash-table] The request's session
   @condition view-context-error Signalled outside a template rendering
   @condition error Signalled by SESSION when the session stage is not enabled
   "
  (session (current-controller helper)))

(defun current-param (key &optional (helper 'current-param))
  "One request parameter of the request being rendered.

   The same lookup an action makes with (PARAM CONTROLLER KEY): query string,
   body and path parameters, merged by the controller stage.  This is what a
   form that re-renders itself after a rejected submission reads to put the
   submitted values back into its fields.

   @param key [string] Parameter name
   @param helper [symbol] Name reported by the error (see CURRENT-CONTROLLER)
   @return [t] The parameter value, or NIL when the request carries no such
     parameter
   @condition view-context-error Signalled outside a template rendering
   "
  (param (current-controller helper) key))
