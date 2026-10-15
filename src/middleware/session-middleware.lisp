(in-package #:cl-user)
(defpackage #:clails/middleware/session-middleware
  (:use #:cl)
  ;; NOTE: lack-middleware-session is one ordinary (non-package-inferred)
  ;; ASDF system that defines several packages (lack/middleware/session,
  ;; lack/middleware/session/store/memory, lack/middleware/session/state/cookie, ...).
  ;; Only its top-level package is registered via register-system-packages,
  ;; so we depend on it through a single :import-from here and refer to the
  ;; other (sub-)packages with fully-qualified symbols below. Adding
  ;; additional :import-from clauses for those sub-packages would make
  ;; clails' package-inferred-system try (and fail) to find matching ASDF
  ;; components named after them.
  (:import-from #:lack/middleware/session
                #:*lack-middleware-session*)
  (:import-from #:clails/middleware/session/store/memory
                #:make-memory-session-store)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log-package.trace)
  (:export #:*enable-session-middleware*
           #:*session-store*
           #:*session-state*
           #:*lack-middleware-clails-session*))
(in-package #:clails/middleware/session-middleware)


(defvar *enable-session-middleware* nil
  "Flag to enable/disable the session middleware.

   When NIL (the default), clails does not manage cookie sessions at all,
   and calling (session controller) / (current-user controller) will signal
   an error.

   Set this to T (typically in app/config/environment.lisp) to opt in to
   cookie-based sessions backed by lack-middleware-session:

     (setf clails/middleware/session-middleware:*enable-session-middleware* t)

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-store* nil
  "Session store used by the session middleware.

   When NIL, a fresh clails in-memory store
   (clails/middleware/session/store/memory:make-memory-session-store) is
   created the first time the middleware stack is built. Set this to a
   different store instance before starting the server to keep sessions
   elsewhere:

     (setf clails/middleware/session-middleware:*session-store*
           (make-instance 'my-app/redis-session-store ...))

   Two kinds of object are accepted here, and both keep working:

     * an instance of a class deriving from
       clails/middleware/session/store:<session-store> -- the clails
       abstraction, and the only kind that supports the fourth operation
       (TOUCH-SESSION, i.e. idle-timeout expiry);
     * a lack session store structure (lack's own memory, DBI or Redis
       store), which earlier versions of clails documented here.  The session
       stage passes the value straight to lack, and lack dispatches on it
       generically, so such a store behaves exactly as it did before.  It
       simply has no last-access time: a lack structure cannot inherit from a
       CLOS class, so it cannot pick up the default TOUCH-SESSION.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-state* nil
  "Session state (cookie handling) used by the session middleware.

   When NIL, a default cookie state (lack/middleware/session/state/cookie:make-cookie-state)
   is created the first time the middleware stack is built. Set this to a
   different lack session state instance to customize cookie name, path,
   domain, secure/httponly flags, expiration, etc. For example:

     (setf clails/middleware/session-middleware:*session-state*
           (lack/middleware/session/state/cookie:make-cookie-state
             :cookie-key \"_myapp_session\"
             :httponly t))

   This can be overridden in <project>/app/config/environment.lisp")


(defparameter *lack-middleware-clails-session*
  (lambda (app)
    "Clails session middleware.

     A thin, opt-in wrapper around lack-middleware-session. When
     *enable-session-middleware* is NIL (the default), requests pass through
     untouched and no cookie is ever set. When T, wraps the application with
     lack/middleware/session:*lack-middleware-session*, using *session-store*
     and *session-state* (or their defaults) so that (getf env :lack.session)
     is populated for every downstream middleware/controller.
     "
    (if (not *enable-session-middleware*)
        app
        (let ((store (or *session-store*
                          (make-memory-session-store)))
              (state (or *session-state*
                          (lack/middleware/session/state/cookie:make-cookie-state))))
          (when (log-level-enabled-p :trace)
            (log-package.trace "Session middleware: enabled"))
          (funcall *lack-middleware-session* app :store store :state state))))
  "Lack middleware function for opt-in cookie session management.")
