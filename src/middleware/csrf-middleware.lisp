(in-package #:cl-user)
(defpackage #:clails/middleware/csrf-middleware
  (:use #:cl)
  ;; NOTE: lack-middleware-csrf is one ordinary (non-package-inferred) ASDF
  ;; system, exactly like lack-middleware-session (see
  ;; src/middleware/session-middleware.lisp L4-L12 for the full story).  A
  ;; SINGLE :import-from #:lack/middleware/csrf is resolvable, because
  ;; lack.asd additionally defines a forwarding system named
  ;; "lack/middleware/csrf" whose only job is to depend on the hyphenated
  ;; system that holds the code; clails' package-inferred-system looks for a
  ;; system of that name and finds it.  Adding :import-from clauses for
  ;; further lack sub-packages that have no such forwarding definition would
  ;; make the package-inferred-system try (and fail) to find matching
  ;; components -- so anything else from lack is referred to with
  ;; fully-qualified symbols instead.  "lack-middleware-csrf" is listed in
  ;; clails.asd's :depends-on for the same reason "lack-middleware-session"
  ;; is.
  (:import-from #:lack/middleware/csrf
                #:*lack-middleware-csrf*
                #:csrf-token
                #:csrf-html-tag)
  (:import-from #:lack/request
                #:make-request
                #:request-body-parameters)
  (:import-from #:clails/environment
                #:*strict-configuration*)
  (:import-from #:clails/middleware/session-middleware
                #:*enable-session-middleware*)
  (:import-from #:clails/middleware/rejection
                #:reject-forbidden)
  ;; LOG-MESSAGE-TO is the function the log.audit macro expands into; it is
  ;; used directly because the context plist is assembled at run time.  The
  ;; configuration warning is written at :warn, which the :audit logger (level
  ;; :info in all three generated environments) still passes on.
  (:import-from #:clails/logger
                #:log-message-to
                #:log-level-enabled-p
                #:log-package.trace)
  (:export #:*enable-csrf-middleware*
           #:*csrf-session-key*
           #:*csrf-form-token*
           #:*csrf-header-name*
           #:*csrf-one-time-token*
           #:*csrf-forbidden-message*
           #:+csrf-marker-key+
           #:+csrf-default-session-key+
           #:csrf-configuration-error
           #:csrf-configuration-error-message
           #:check-csrf-configuration
           #:constant-time-string=
           #:csrf-token
           #:csrf-html-tag
           #:*lack-middleware-clails-csrf-body-guard*
           #:*lack-middleware-clails-csrf*))
(in-package #:clails/middleware/csrf-middleware)

;;; ------------------------------------------------------------------
;;; The CSRF verification stage
;;; ------------------------------------------------------------------
;;;
;;; What this stage is
;;; ------------------
;;; lack/middleware/csrf already implements the whole of the usual CSRF
;;; scheme: it mints a per-session token, keeps it in the session, looks the
;;; token up again on an unsafe request and refuses the request when the two
;;; do not match.  clails adopts that component as its verification stage
;;; rather than re-implementing it, and supplies the four things it does not
;;; do:
;;;
;;;   * the rejection is a 403 built by clails/middleware/rejection, and it
;;;     is recorded on the :audit logger (M6-T2) -- lack's own default is a
;;;     400 that nothing records;
;;;   * a token sent in a request HEADER is accepted (M6-T4) -- lack only
;;;     ever looks at a top-level body parameter;
;;;   * a body whose shape is unexpected (broken JSON, a JSON array at the
;;;     top level, an empty body, a body that disagrees with its declared
;;;     Content-Type) is turned into that same 403 instead of letting the
;;;     parse error escape the whole middleware stack (M6-T3);
;;;   * the token comparison is a length-independent, early-exit-free one
;;;     (M6-T5) -- lack compares with EQUAL.
;;;
;;; Where it sits in the stack
;;; --------------------------
;;; Inside the session stage (it needs (getf env :lack.session)) and outside
;;; the routing stage (a request must be refused before it can reach an
;;; action, and rendering happens inside routing, so the token has to be
;;; available there).  That is the position the stage occupies in
;;; clails/middleware/core:*clails-middleware-stack*, and
;;; clails-test/middleware/csrf-http-roundtrip asserts it from the outside,
;;; by sending requests through a stack.
;;;
;;; M9 adds the request-method rewriting stage INSIDE this one: a request that
;;; forges its method must be verified before the forgery is honoured.  To let
;;; M9 check that it really is on the inside, this stage leaves a marker in
;;; the request environment on every request it lets through (see
;;; +CSRF-MARKER-KEY+).
;;;
;;; How a request is decided
;;; ------------------------
;;; The decision is taken in the outer half of the stage, before lack's stage
;;; runs, because three of the four additions above have to happen before the
;;; body is parsed by lack:
;;;
;;;   safe method (GET/HEAD/OPTIONS/...) -> straight through, body untouched
;;;   no :lack.session                   -> straight through, so that lack's
;;;                                         own explicit error still happens
;;;                                         (see CHECK-CSRF-CONFIGURATION)
;;;   body cannot be parsed, or parses
;;;   to something that is not an alist
;;;   of string keys                     -> 403
;;;   token missing or not equal         -> 403
;;;   token equal                        -> lack's stage, then the
;;;                                         application
;;;
;;; On the accepting path lack's stage runs the comparison a second time, and
;;; that is deliberate: it is the half that binds the dynamic variables
;;; CSRF-TOKEN and CSRF-HTML-TAG read, so that an action (or the form helper a
;;; view calls) can ask for the session's token at all.  The token handed to
;;; it is the string the session itself holds, so its EQUAL never decides
;;; anything this stage has not already decided with CONSTANT-TIME-STRING=,
;;; and a wrong token never reaches it.
;;;
;;; What lack is NOT asked to do is consume a one-time token: see
;;; MAKE-PASSAGE-APP.

(defparameter +csrf-default-session-key+ "clails.csrf-token"
  "The session key the CSRF token is stored under, by default.

   lack/middleware/csrf defaults to \"_csrf_token\", which carries no
   framework prefix: an application keeping its own value under that name in
   the session would collide with the framework's token -- silently, since
   both sides are just a string in the same hash-table.  Every session key
   clails itself uses is therefore prefixed with \"clails.\" (the existing
   reserved key \"clails.current-user\" is the precedent), and lack takes the
   name as an argument, so the change costs nothing.")

(defparameter +csrf-marker-key+ :clails.csrf-verified
  "The request-environment key this stage marks a verified request with.

   The stage puts (getf env :clails.csrf-verified) = T into the environment of
   every request it lets through -- a safe method as well as an unsafe one
   whose token matched -- and a request it rejects never reaches the marking.
   When the stage is disabled it is a pass-through and marks nothing.

   This exists for the configuration check M9 needs (checked configuration 2
   of the strict check, see *STRICT-CONFIGURATION*): the request-method
   rewriting stage M9 adds must run INSIDE this stage, and the only way for it
   to tell is to look for this marker.  A structural check (walking the
   middleware list) would misjudge the two configurations the public
   documentation itself shows -- wrapping a stage, and replacing the whole
   list -- whereas a marker reports the order the request actually took.

   Reading the marker is M9-T1's half of that mechanism; this module only
   places it.  The name follows the same \"clails.\" prefix rule as the
   session keys.")

(defvar *enable-csrf-middleware* nil
  "Flag to enable/disable the CSRF verification middleware.

   When NIL (the default), the stage is a pass-through: unsafe requests are
   not verified, no token is kept in the session and no marker is left in the
   request environment -- an existing project therefore behaves exactly as it
   did before.  The framework default is NIL for the same reason the session
   middleware's is: enabling it changes how every state-changing request is
   answered.  A project generated by `clails new' switches it on in
   app/config/environment.lisp.

   Set this to T to verify every state-changing request (POST / PUT / PATCH /
   DELETE):

     (setf clails/middleware/csrf-middleware:*enable-csrf-middleware* t)

   The session middleware must be enabled as well -- the token lives in the
   session.  A configuration that enables this stage without the session
   stage is refused when the middleware stack is built; see
   CHECK-CSRF-CONFIGURATION.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *csrf-session-key* +csrf-default-session-key+
  "Session key the per-session CSRF token is stored under.

   Defaults to \"clails.csrf-token\" rather than lack's own
   \"_csrf_token\" (see +CSRF-DEFAULT-SESSION-KEY+ for why).  It is passed to
   lack/middleware/csrf as :session-key, so the token really is kept under
   this name and CSRF-TOKEN reads it from there.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *csrf-form-token* "_csrf_token"
  "Name of the body parameter (hidden form field) the token is read from.

   This one keeps lack's default: it is part of the HTML a form emits, not a
   session key, and CSRF-HTML-TAG renders a field of exactly this name.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *csrf-header-name* "X-CSRF-Token"
  "Request header the token may be sent in instead of a body parameter.

   lack/middleware/csrf only ever looks at a top-level body parameter, which
   leaves a fetch/XHR request that sends JSON it does not control no way to
   carry the token; clails reads this header as well (M6-T4).  Header names
   are matched case-insensitively, as the request environment holds them
   downcased.

   The body parameter wins when a request carries both: the form field is the
   explicit, visible place to put the token, and letting a header silently
   override it would turn a header injected anywhere in the request path into
   the deciding value.

   Note what is deliberately NOT read: the query string, and a token nested
   inside a JSON body.  A token in the query string ends up in logs, proxy
   histories and Referer headers; a nested token would make the stage's
   decision depend on an application's JSON schema.  Both are measured
   non-starters rather than oversights.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *csrf-one-time-token* nil
  "Whether a CSRF token is consumed by the request that uses it.

   NIL (the default) keeps the token for the whole session, which is what
   every form on every page of that session then shares.  T switches to
   one-time tokens: the token is removed from the session by the request that
   verifies successfully, so a replay of the same token is refused.

   The default is deliberately NOT changed to T.  One-time tokens cost
   usability in ways that are easy to hit:

     * a resubmission is always refused -- the back button, a double-clicked
       submit button, a naive retry;
     * every form on one page shares one token, so submitting one form
       invalidates the others (a list screen with a button per row suffers
       most);
     * concurrent submissions resolve to exactly one winner, so an
       application that submits in parallel has to expect the losers;
     * the action body of a successful request can no longer read the token,
       because it has already been consumed -- re-rendering the form mints a
       new one, which is why \"submit, then re-render\" is self-healing.

   What one-time tokens do NOT cost (both measured, both kept as regression
   tests in clails-test/middleware/csrf-one-time): a FAILED verification does
   not consume the token, so a third party cannot destroy a user's token by
   throwing invalid ones at it; and opening a second tab does not invalidate
   the first tab's form, because rendering twice returns the same token.

   One-time tokens suit a single-form screen with no concurrency and no
   retries.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *csrf-forbidden-message* "Forbidden: invalid CSRF token. Reload the page and try again."
  "Body of the 403 response a refused request gets.

   It follows the body format M2 decided for a rejection -- one plain-text
   line, no markup -- and adds the one thing a legitimate user refused by
   this stage needs to be told: reload and retry.  That matters most with
   one-time tokens (see *CSRF-ONE-TIME-TOKEN*), where a legitimate user is
   refused far more often, but the advice is right in every case: a stale or
   missing token is fixed by fetching the form again.

   This can be overridden in <project>/app/config/environment.lisp")


(define-condition csrf-configuration-error (error)
  ((message :initarg :message
            :reader csrf-configuration-error-message
            :documentation "What is wrong, and what to do about it."))
  (:report (lambda (condition stream)
             (write-string (csrf-configuration-error-message condition) stream)))
  (:documentation "Signalled when the CSRF stage is configured in a way that cannot work.

   Signalled while the middleware stack is being built, i.e. during server
   startup, so that the server does not come up in a configuration in which
   every state-changing request is bound to fail."))


;;; ------------------------------------------------------------------
;;; Configuration check (M6-T7, checked configuration 1)
;;; ------------------------------------------------------------------

(defun check-csrf-configuration ()
  "Check that this stage's configuration can work, and refuse it if not.

   The only configuration checked here is \"CSRF enabled, session disabled\",
   and it is checked because it is unambiguous: lack/middleware/csrf signals
   an error when (getf env :lack.session) is missing, and clails has nothing
   in the stack that turns a condition into a response, so the error escapes
   the stack and the request dies as a server error.  Such a configuration is
   100% broken, with no false positive to worry about -- which is why it is
   allowed to abort startup.

   clails/environment:*STRICT-CONFIGURATION* decides what happens:

     T   (the framework default) -- signal CSRF-CONFIGURATION-ERROR, which
         aborts the stack build and therefore startup;
     NIL -- record a warning on the :audit logger and carry on.

   The warning goes to the :audit logger rather than the framework's own
   package logger: the generated logger configuration sets the package logger
   to :error in development and test, so an :warn record there would never be
   seen, while :audit is registered in all three environments.

   The check runs where the middleware stack is assembled -- this stage's own
   builder, which is the one place guaranteed to run exactly once per stack,
   whether the stack is the framework default, a default with stages added to
   it, or one an application assembled itself.  Nothing else has to call it,
   and the code paths that never build a stack (a task, the console) are
   therefore never affected by it.

   @return [null] nil
   @condition csrf-configuration-error When the session stage is disabled and *strict-configuration* is true
   "
  (unless *enable-session-middleware*
    (let ((message
            (concatenate
             'string
             "clails CSRF middleware is enabled but the session middleware is not. "
             "The CSRF token is kept in the session, and the verification stage signals "
             "an error on every state-changing request when (getf env :lack.session) is missing. "
             "Fix it in app/config/environment.lisp by enabling the session middleware "
             "((setf clails/middleware/session-middleware:*enable-session-middleware* t)), "
             "or by disabling the CSRF middleware "
             "((setf clails/middleware/csrf-middleware:*enable-csrf-middleware* nil)). "
             "To let a configuration like this one start up with a warning instead, "
             "set clails/environment:*strict-configuration* to nil.")))
      (if *strict-configuration*
          (error 'csrf-configuration-error :message message)
          (log-message-to :audit :warn message
                          :stage :csrf
                          :check :session-required))))
  nil)


;;; ------------------------------------------------------------------
;;; Comparing tokens (M6-T5)
;;; ------------------------------------------------------------------

(defun constant-time-string= (a b)
  "Compare the strings A and B without letting the comparison time say where
   they first differ.

   An ordinary string comparison returns as soon as it meets a differing
   character, so the time it takes reveals how long a prefix the caller got
   right -- enough, with many requests, to guess a token character by
   character.  This comparison instead folds every character difference into
   one accumulator and inspects it only at the end, so it looks at the same
   number of characters whatever the answer turns out to be.

   The lengths are folded into the same accumulator, and the shorter string is
   indexed modulo its own length, so that a wrong length does not shorten the
   loop either.  A non-string argument (a JSON token sent as a number, a
   missing parameter, NIL) is simply not equal to anything.

   @param a [t] First value; only a string can compare equal
   @param b [t] Second value; only a string can compare equal
   @return [boolean] T when both are strings with the same contents, NIL otherwise
   "
  (and (stringp a)
       (stringp b)
       (let* ((la (length a))
              (lb (length b))
              (difference (logxor la lb)))
         (if (or (zerop la) (zerop lb))
             ;; Nothing to walk: equal exactly when both are empty.
             (zerop difference)
             (progn
               (dotimes (i la)
                 (setf difference
                       (logior difference
                               (logxor (char-code (char a i))
                                       (char-code (char b (mod i lb)))))))
               (zerop difference))))))


;;; ------------------------------------------------------------------
;;; Reading the token out of a request
;;; ------------------------------------------------------------------

(defparameter +unsafe-methods+ '(:post :put :patch :delete)
  "The request methods whose requests are verified.

   The same set lack/middleware/csrf uses, spelled out here because the
   decision is taken in this stage: every method that is meant to change
   state, PATCH included.  Everything else (GET, HEAD, OPTIONS, ...) passes
   through unverified, and its body is not even looked at.")

(defun unsafe-method-p (request-method)
  "True when REQUEST-METHOD is one this stage verifies.

   @param request-method [keyword] The :request-method of the request environment
   @return [boolean] T for POST / PUT / PATCH / DELETE
   "
  (and (member request-method +unsafe-methods+ :test #'eq) t))

(defun parameter-alist-p (parameters)
  "True when PARAMETERS has the shape the token lookup needs: a list of
   conses whose CAR is a string.

   A parsed body is not guaranteed to have that shape.  A JSON body whose top
   level is an array of objects, for instance, parses into a list of alists --
   a list of conses whose CAR is itself a cons -- and looking a string up in
   it with ASSOC :test #'STRING= signals a type error.  Rather than guess what
   such a body meant, the stage treats it as unexpected and refuses the
   request.

   @param parameters [list] Parsed body parameters
   @return [boolean] T when every element is a cons with a string key
   "
  (every (lambda (entry)
           (and (consp entry)
                (stringp (car entry))))
         parameters))

(defun body-parameters-of (env)
  "Parse the body of ENV and return (values PARAMETERS STATUS).

   STATUS is :OK when PARAMETERS can be searched for a token, and :MALFORMED
   when the body could not be parsed at all or parsed into something that is
   not an alist of string keys.  PARAMETERS is NIL in the :MALFORMED case.

   This is the containment M6-T3 needs.  A state-changing request carrying a
   broken JSON body, a JSON array at the top level, an empty body or a body
   that disagrees with its declared Content-Type makes the body parser signal,
   and nothing in the clails stack turns a condition into a response -- so
   without this the condition escapes the whole stack and the request becomes
   an externally triggerable server failure (measured).  Catching it here
   converts it into an ordinary rejection.

   The scope of the catch is deliberately this one call and nothing else: the
   application below the stage keeps signalling its own errors the way it
   always did, and the missing-session error that lack raises for a
   misconfigured stack still escapes (that configuration is caught at startup
   instead; see CHECK-CSRF-CONFIGURATION).

   In the framework's own stack the parse error has normally been caught one
   stage further out already (*LACK-MIDDLEWARE-CLAILS-CSRF-BODY-GUARD*,
   which has to be outside the session stage because that stage parses the
   body too).  The catch is kept here as well, because this stage must refuse
   such a request on its own in a stack assembled by hand, and because the
   unusable-but-parseable shapes are only visible here.

   Parsing here costs nothing extra downstream: LACK/REQUEST:MAKE-REQUEST
   caches what it parsed in the request environment, so lack's CSRF stage and
   the controller stage reuse the result rather than parsing again.

   @param env [list] Clack/Lack request environment plist
   @return [(values list keyword)] Parsed parameters, and :OK or :MALFORMED
   "
  (handler-case
      (let ((parameters (request-body-parameters (make-request env))))
        (if (parameter-alist-p parameters)
            (values parameters :ok)
            (values nil :malformed)))
    (error () (values nil :malformed))))

(defun body-token (parameters form-token)
  "The token PARAMETERS carries under FORM-TOKEN, or NIL.

   A multipart body gives a parameter's value as a list (the value plus the
   part's metadata); as lack does, the first element is taken.

   @param parameters [list] Parsed body parameters
   @param form-token [string] Name of the parameter holding the token
   @return [t] The value found (normally a string), or NIL
   "
  (let ((value (cdr (assoc form-token parameters :test #'string=))))
    (if (listp value)
        (first value)
        value)))

(defun header-token (env header-name)
  "The token ENV's HEADER-NAME request header carries, or NIL.

   Header names are matched against the downcased name, which is how every
   Clack backend puts them into the environment; the name as written is tried
   as well, for an environment assembled by hand.

   @param env [list] Clack/Lack request environment plist
   @param header-name [string] Header to read
   @return [string or null] The header value when it is a string
   "
  (let ((headers (getf env :headers)))
    (when (hash-table-p headers)
      (let ((value (or (gethash (string-downcase header-name) headers)
                       (gethash header-name headers))))
        (when (stringp value)
          value)))))

(defun received-token (env parameters form-token header-name)
  "The token the request offers, or NIL.

   The body parameter is consulted first and the header only when the body
   carries no such parameter at all -- see *CSRF-HEADER-NAME* for why the
   body wins.

   @param env [list] Clack/Lack request environment plist
   @param parameters [list] Parsed body parameters
   @param form-token [string] Name of the body parameter holding the token
   @param header-name [string] Name of the header holding the token
   @return [t] The token offered by the request, or NIL
   "
  (let ((from-body (body-token parameters form-token)))
    (if (null from-body)
        (header-token env header-name)
        from-body)))


;;; ------------------------------------------------------------------
;;; Handing an accepted request on
;;; ------------------------------------------------------------------

(defun env-with-verified-token (env parameters form-token token)
  "Return the environment to hand to lack's CSRF stage for an accepted
   request.

   The body parameters are made to carry TOKEN -- the very string the session
   holds -- under FORM-TOKEN.  That does two things:

     * it is how a token sent in a header reaches lack's stage, which only
       looks at body parameters;
     * it keeps lack's EQUAL from being the comparison that decides anything.
       The value it compares is the session's own token, so it can only agree
       with the decision CONSTANT-TIME-STRING= has already taken -- except in
       the one race that matters, where a concurrent one-time submission has
       consumed the token in between, and lack correctly refuses.

   The value stored is a string equal to the token the request sent, so the
   application below reads the same token it would have read anyway (for a
   multipart body it reads the string rather than the one-element list the
   parser produced, which is if anything the more useful of the two).

   @param env [list] Clack/Lack request environment plist
   @param parameters [list] Parsed body parameters (possibly NIL)
   @param form-token [string] Name of the body parameter holding the token
   @param token [string] The token the session holds
   @return [list] The environment to pass on
   "
  (let ((entry (assoc form-token parameters :test #'string=)))
    (cond (entry
           ;; The alist is the one cached in ENV, so updating it in place is
           ;; what makes the value visible to the stages below.
           (setf (cdr entry) token)
           env)
          (t
           (setf (getf env :body-parameters)
                 (cons (cons form-token token) parameters))
           env))))

(defun env-with-marker (env)
  "Return ENV carrying the \"this request passed the CSRF stage\" marker.

   See +CSRF-MARKER-KEY+ for what reads it and why it exists.

   @param env [list] Clack/Lack request environment plist
   @return [list] The environment to pass on
   "
  (if (getf env +csrf-marker-key+)
      env
      (list* +csrf-marker-key+ t env)))

(defun make-passage-app (app &key session-key one-time message)
  "Wrap APP in the innermost half of this stage: consume a one-time token and
   place the marker.

   The wrapper sits between lack's CSRF stage and whatever comes next, so a
   request reaches it exactly when the verification let it through, and a
   rejected request never does.  Two things are done there:

   Consuming a one-time token.  lack can do this itself (its :one-time), and
   this stage deliberately does not let it: lack reads the token, compares it
   and only then removes it, so two concurrent submissions of the same token
   can both read it before either removes it and both be served -- which was
   measured to happen (about one round in twelve, with the two requests
   released together).  REMHASH, in contrast, reports whether the key WAS
   there, and the session container the clails session stage installs
   serialises its own accesses, so REMHASH is an atomic test-and-clear: of any
   number of concurrent requests, exactly one gets T and is served, and every
   other one is refused.  The loser is refused here when it got this far, or
   by lack's own check (which finds no token) when it had not.

   On an implementation whose hash-tables cannot be made to serialise their
   own accesses the container is left as it was (see
   clails/middleware/session/store/wrapper:+SYNCHRONIZED-SESSION-TABLES-P+)
   and the exclusion is only as good as the implementation's REMHASH -- no
   worse than leaving the consumption to lack, which is what it replaces.

   A safe method never consumes anything: it is not verified, and a GET that
   spent the token would invalidate the very form it just rendered.

   Placing the marker.  See +CSRF-MARKER-KEY+ for what reads it.

   @param app [function] The application below this stage
   @param session-key [string] Session key the token is stored under
   @param one-time [boolean] Whether a verified token is consumed
   @param message [string] Body of the 403 a loser gets
   @return [function] The wrapped application
   "
  (lambda (env)
    (let ((session (getf env :lack.session)))
      (if (and one-time
               (unsafe-method-p (getf env :request-method))
               (hash-table-p session)
               (not (remhash session-key session)))
          (reject env "CSRF check failed: one-time token already used" message)
          (funcall app (env-with-marker env))))))

(defun reject (env reason message)
  "Refuse the request: record it on the :audit logger and answer 403.

   @param env [list] Clack/Lack request environment plist
   @param reason [string] Why it was refused (the audit record's message)
   @param message [string] Response body
   @return [list] Lack response as (status headers body)
   "
  (reject-forbidden env
                    :reason reason
                    :message message
                    :context (list :stage :csrf)))

(defun make-block-app (message)
  "The application lack's CSRF stage rejects with (its :block-app).

   lack's default answers 400 and records nothing; this one answers the same
   403 as the rest of the stage and leaves the same audit record (M6-T2).

   It is reached in one situation only, because this stage has already taken
   the decision by the time lack's stage runs: a one-time token consumed by a
   concurrent request between this stage's comparison and lack's.  That is
   exactly the case that must be refused, which is why the hook is wired up
   rather than left at its default.  (The same race caught one step further
   in is MAKE-PASSAGE-APP's business.)

   @param message [string] Response body
   @return [function] A Lack application that refuses every request
   "
  (lambda (env)
    (reject env "CSRF token no longer valid" message)))


;;; ------------------------------------------------------------------
;;; The outer half of the stage: unparseable bodies (M6-T3)
;;; ------------------------------------------------------------------
;;;
;;; Why this is a stage of its own, OUTSIDE the session stage
;;; --------------------------------------------------------
;;; A request body is parsed by LACK/REQUEST:MAKE-REQUEST, and the first
;;; thing in the stack to call it is not the CSRF stage: it is the SESSION
;;; stage, whose cookie state calls MAKE-REQUEST to read the Cookie header
;;; (lack/middleware/session/state/cookie), and MAKE-REQUEST parses the body
;;; on the way.  A body that cannot be parsed therefore signals one stage
;;; further out than the CSRF stage, i.e. before any code of the CSRF stage
;;; runs -- measured, and the reason this containment cannot live inside the
;;; verification stage.
;;;
;;; Nothing in the clails stack turns a condition into a response, so such a
;;; condition escapes the whole stack: a request anybody can send kills the
;;; handling of that request, which is an availability defect (G10) rather
;;; than a bad request.  This stage is the wrapper that turns it into an
;;; ordinary 403.
;;;
;;; Scope
;;; -----
;;; Only a body that cannot be PARSED, and only while the CSRF stage is
;;; enabled:
;;;
;;;   * a body that parses into something unusable (a JSON array at the top
;;;     level, say) is not this stage's business -- it parses, so nothing
;;;     signals; the verification stage refuses it;
;;;   * every method is checked, not only the state-changing ones, because
;;;     the parse that signals is the session stage's and it does not care
;;;     about the method: a GET carrying a broken body would otherwise kill
;;;     the request just as reliably;
;;;   * a request that declares no body is never touched, and a body whose
;;;     Content-Type the parser does not know (text/plain,
;;;     application/octet-stream, ...) parses to no parameters without
;;;     signalling, so it passes;
;;;   * the error is only caught around that one parse, so the application's
;;;     own errors still propagate exactly as they did.
;;;
;;; The same defect exists with the CSRF stage disabled, since it is really
;;; the session stage's parse -- but closing it there would change how an
;;; existing project answers a malformed request without that project opting
;;; in to anything, so it is left to the goal that owns request-wide error
;;; handling.

(defun body-parseable-p (env)
  "True when the body of ENV can be parsed (or when there is none to parse).

   @param env [list] Clack/Lack request environment plist
   @return [boolean] NIL only when parsing the declared body signals
   "
  (handler-case (progn (make-request env) t)
    (error () nil)))

(defparameter *lack-middleware-clails-csrf-body-guard*
  (lambda (app)
    "Clails CSRF body guard: answers 403 instead of letting a body that
     cannot be parsed kill the request.

     The outer half of the CSRF stage.  It belongs outside the session stage
     and is a pass-through whenever *enable-csrf-middleware* is NIL; see the
     commentary above for why it cannot be part of the verification stage
     itself.
     "
    (if (not *enable-csrf-middleware*)
        app
        ;; Read once, when the stack is built -- the contract the session and
        ;; verification stages have.
        (let ((message *csrf-forbidden-message*))
          (lambda (env)
            (if (body-parseable-p env)
                (funcall app env)
                (reject env
                        "Request rejected: request body could not be parsed"
                        message))))))
  "Lack middleware function turning an unparseable request body into a 403.")


;;; ------------------------------------------------------------------
;;; The stage
;;; ------------------------------------------------------------------

(defparameter *lack-middleware-clails-csrf*
  (lambda (app)
    "Clails CSRF verification middleware.

     An opt-in stage around lack/middleware/csrf.  When
     *enable-csrf-middleware* is NIL (the default) the application is returned
     untouched: nothing is verified, no token is minted and no marker is
     placed.  When T, every POST / PUT / PATCH / DELETE must carry the
     session's CSRF token -- in the *csrf-form-token* body parameter or the
     *csrf-header-name* header -- and is answered with a 403 recorded on the
     :audit logger when it does not.

     The configuration variables are read once, here, when the stack is
     built -- the same contract the session stage has -- so a stack cannot be
     reconfigured after the fact, and every test builds its own.
     "
    ;; The enable decision is taken here, once, and a disabled stage returns
    ;; the application untouched.  A later goal that lets a project exempt
    ;; certain paths from verification puts that test in the same two places:
    ;; a path pattern list is read here, and the per-request lambda below
    ;; passes a matching request straight to INNER, exactly as a safe method
    ;; is passed.  Nothing else has to move.
    (if (not *enable-csrf-middleware*)
        app
        (let ((session-key *csrf-session-key*)
              (form-token *csrf-form-token*)
              (header-name *csrf-header-name*)
              (one-time *csrf-one-time-token*)
              (message *csrf-forbidden-message*))
          ;; Refuse (or report) a configuration that cannot work, before the
          ;; server comes up.  This is the one place that runs exactly once
          ;; per middleware stack.
          (check-csrf-configuration)
          (when (log-level-enabled-p :trace)
            (log-package.trace "CSRF middleware: enabled"))
          (let ((inner (funcall *lack-middleware-csrf*
                                (make-passage-app app
                                                  :session-key session-key
                                                  :one-time one-time
                                                  :message message)
                                :block-app (make-block-app message)
                                :session-key session-key
                                :form-token form-token
                                ;; NOT lack's :one-time -- the consumption is
                                ;; done by the passage app, atomically.  See
                                ;; MAKE-PASSAGE-APP.
                                :one-time nil)))
            (lambda (env)
              (let ((session (getf env :lack.session)))
                (cond
                  ;; A safe method is not verified and its body is not even
                  ;; parsed: whatever it carries is the business of whoever
                  ;; reads it further down.
                  ((not (unsafe-method-p (getf env :request-method)))
                   (funcall inner env))
                  ;; No session: hand the request to lack so that its own
                  ;; explicit error happens.  Silently refusing or silently
                  ;; serving such a request would hide a broken configuration
                  ;; that CHECK-CSRF-CONFIGURATION reports at startup.
                  ((null session)
                   (funcall inner env))
                  (t
                   (multiple-value-bind (parameters status) (body-parameters-of env)
                     (if (eq status :malformed)
                         (reject env "CSRF check failed: request body could not be parsed" message)
                         (let ((stored (gethash session-key session))
                               (received (received-token env parameters form-token header-name)))
                           (cond
                             ((null stored)
                              (reject env "CSRF check failed: no token in session" message))
                             ((null received)
                              (reject env "CSRF check failed: token missing from request" message))
                             ((constant-time-string= stored received)
                              (funcall inner (env-with-verified-token env parameters
                                                                      form-token stored)))
                             (t
                              (reject env "CSRF check failed: token mismatch" message))))))))))))))
  "Lack middleware function for opt-in CSRF verification.")
