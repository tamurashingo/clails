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
  (:import-from #:clails/middleware/session/store/wrapper
                #:ensure-session-store-wrapper
                #:wrap-session-container)
  ;; The session cookie clails emits: HttpOnly, SameSite stated explicitly, a
  ;; name that does not advertise the implementation, and no expiry date.
  (:import-from #:clails/middleware/session/state/cookie
                #:make-clails-cookie-state
                #:+default-session-cookie-name+)
  ;; Server-side expiry and its periodic cleanup.  The configuration
  ;; variables of both are defined in their own session packages and
  ;; re-exported here, so that an application sets every session option
  ;; through one package (and clails/middleware re-exports them again).
  (:import-from #:clails/middleware/session/expiry
                #:*session-idle-timeout*
                #:wrap-session-expiry)
  (:import-from #:clails/middleware/session/sweeper
                #:*session-sweep-enabled*
                #:*session-sweep-interval*
                #:register-session-sweep-store)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log-package.trace)
  (:export #:*enable-session-middleware*
           #:*session-store*
           #:*session-state*
           #:*session-cookie-secure*
           #:*session-idle-timeout*
           #:*session-sweep-enabled*
           #:*session-sweep-interval*
           #:+default-session-cookie-name+
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

   Whatever is put here, the session stage wraps it in a
   clails/middleware/session/store/wrapper:<session-store-wrapper>, which
   serialises access to one session and provides WITH-SESSION-LOCK.  The
   wrapping is transparent -- the operations are forwarded to the store set
   here, and the store set here is where the sessions end up -- and
   idempotent, so putting a wrapper here yourself does not add a second
   layer.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-state* nil
  "Session state (cookie handling) used by the session middleware.

   When NIL, a default clails cookie state
   (clails/middleware/session/state/cookie:make-clails-cookie-state) is
   created the first time the middleware stack is built, with :SECURE taken
   from *SESSION-COOKIE-SECURE*.  That default emits

     <name>=<id>; path=/; HttpOnly; SameSite=Lax

   and deliberately NO expiry date -- the deadline is the server-side idle
   timeout, and a cookie date would contradict it (the reasoning is written
   out in clails/middleware/session/state/cookie).

   Set this to a state of your own to change the cookie name, path, domain,
   SameSite or the flags:

     (setf clails/middleware/session-middleware:*session-state*
           (clails/middleware/session/state/cookie:make-clails-cookie-state
             :cookie-key \"_myapp_session\"
             :path \"/\"
             :samesite :strict))

   A lack session state is accepted here as well, and is the way to ask for a
   persistent cookie (one that survives the browser being closed), which the
   clails state deliberately cannot express:

     (setf clails/middleware/session-middleware:*session-state*
           (lack/middleware/session/state/cookie:make-cookie-state ...))

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-cookie-secure* nil
  "Whether the session cookie is marked Secure, i.e. sent over HTTPS only.

   NIL in the framework, and that default is a deliberate exception to
   \"default to the safe value\".  A Secure cookie is not sent over plain
   HTTP AT ALL, so turning it on unconditionally would break every
   development environment that runs without TLS -- sessions would simply
   stop working for a developer who configured nothing.  Every other cookie
   default did move to the safe value in this release (HttpOnly on, SameSite
   stated, a name that does not advertise the implementation), because none
   of those breaks a plain-HTTP setup.

   Contrast the `strict' configuration check, whose framework default IS the
   safe value: a stricter check cannot break an existing project (there is
   nothing for it to reject yet), while a Secure cookie breaks a working one.
   The asymmetry is intentional and is why these two settings point in
   opposite directions.

   THE FRAMEWORK NEVER LOOKS AT WHICH ENVIRONMENT IT IS RUNNING IN.  It reads
   this variable and nothing else; putting T in it for production is the
   generated project's configuration, not a decision clails makes by
   inspecting *PROJECT-ENVIRONMENT*.  Turn it on wherever the application is
   served over HTTPS:

     (setf clails/middleware/session-middleware:*session-cookie-secure* t)

   An existing project whose configuration files predate this variable keeps
   the NIL default, so nothing changes for it until that line is added.

   Only read when the session stage builds its own default state; a state set
   in *SESSION-STATE* by hand carries its own :SECURE.

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
        ;; The configured store is wrapped so that access to one session is
        ;; serialised, and the application is wrapped so that it runs with a
        ;; session container that tolerates concurrent writes.  Both halves
        ;; live in clails/middleware/session/store/wrapper, which explains why
        ;; neither is optional: the default server runs one thread per request
        ;; and a single page load sends several requests carrying the same
        ;; session cookie, so without them concurrent requests corrupt the
        ;; session table (measured).  Wrapping is idempotent, so a store that
        ;; already is a wrapper is used as it is.
        (let ((store (ensure-session-store-wrapper
                      (or *session-store*
                          (make-memory-session-store))))
              (state (or *session-state*
                         (make-clails-cookie-state
                          :secure *session-cookie-secure*))))
          (when (log-level-enabled-p :trace)
            (log-package.trace "Session middleware: enabled"))
          ;; The periodic sweeper can only walk a store that knows which
          ;; session ids exist, which is the wrapping store built just above
          ;; -- and this is the only place that knows it.  Registering it here
          ;; also means a process that never builds a stack (a task, the
          ;; console) ends up with no sweeper thread at all.
          (register-session-sweep-store store)
          (funcall *lack-middleware-session*
                   ;; Two stages live INSIDE lack's session stage, in this
                   ;; order from the outside in:
                   ;;
                   ;;   WRAP-SESSION-CONTAINER  exchanges the session
                   ;;     container for one that tolerates concurrent writes,
                   ;;     and publishes the store in the environment;
                   ;;   WRAP-SESSION-EXPIRY     enforces the server-side idle
                   ;;     timeout: it empties an expired session, has its id
                   ;;     replaced and drops it from the store, and records
                   ;;     the access of a live one.
                   ;;
                   ;; Expiry has to be inside the session stage, because that
                   ;; is the only place where the session and its options
                   ;; both exist; and inside the container stage, so that the
                   ;; table it empties is the one the application will use.
                   (wrap-session-container
                    (wrap-session-expiry app store state)
                    store)
                   :store store
                   :state state))))
  "Lack middleware function for opt-in cookie session management.")
