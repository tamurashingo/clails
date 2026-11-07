(in-package #:cl-user)
(defpackage #:clails/middleware/method-override-middleware
  (:use #:cl)
  (:import-from #:lack/request
                #:make-request
                #:request-body-parameters)
  (:import-from #:clails/environment
                #:*strict-configuration*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:+csrf-marker-key+)
  (:import-from #:clails/middleware/rejection
                #:reject-forbidden)
  ;; LOG-MESSAGE-TO is the function the log.audit macro expands into; it is
  ;; used directly because the context plist is assembled at run time.  The
  ;; configuration warning is written at :warn, which the :audit logger (level
  ;; :info in all three generated environments) still passes on -- the
  ;; framework's own package logger would not, as it is configured at :error
  ;; in development and test.
  (:import-from #:clails/logger
                #:log-message-to
                #:log-level-enabled-p
                #:log-package.trace)
  (:export #:*enable-method-override*
           #:*method-override-parameter*
           #:*method-override-forbidden-message*
           #:+method-override-methods+
           #:+method-override-default-parameter+
           #:method-override-value
           #:method-override-warning-issued-p
           #:reset-method-override-warning
           #:*lack-middleware-clails-method-override*))
(in-package #:clails/middleware/method-override-middleware)

;;; ------------------------------------------------------------------
;;; The request-method override stage (M9)
;;; ------------------------------------------------------------------
;;;
;;; What this stage is for
;;; ----------------------
;;; An HTML form can only be sent with GET or POST.  A resource's update and
;;; destroy routes are reached with PUT / PATCH / DELETE, so a form that wants
;;; to update or delete has to say which method it meant in a hidden field --
;;; the "_method" convention every framework of this shape uses.
;;;
;;; clails already honoured that field, but only in one place: the legacy
;;; HTTP-method dispatch branch of the controller stage
;;; (clails/middleware/clails-middleware), which reads it AFTER the route has
;;; already been matched.  Route matching looks at the real request method and
;;; never at the field, so with :action-based routing -- the routes RESOURCES
;;; generates, among them -- the forgery had no effect at all: a form posting
;;; to /todos/7 with _method=put matched no route and the request became a
;;; 404.  "A form cannot reach the update and destroy routes of a resource"
;;; was exactly that.
;;;
;;; This stage closes it by rewriting the DECLARED request method in the
;;; request environment, which is the one value both route matching and the
;;; legacy dispatch branch read.  Rewriting it therefore fixes both at once,
;;; and no line of the route resolution or dispatch code has to change.
;;;
;;; Where it sits in the stack, and why that is a safety requirement
;;; ---------------------------------------------------------------
;;; INSIDE the CSRF verification stage and OUTSIDE the controller (dispatch)
;;; stage.  That is the order clails/middleware/core:*clails-middleware-stack*
;;; declares, and it is not a matter of taste:
;;;
;;;   * outside the CSRF stage, the forgery would be honoured BEFORE the token
;;;     is checked.  A POST rewritten to a safe method would then be let
;;;     through unverified -- the "_method=GET and no token" bypass, which was
;;;     measured to work when the stage is placed there.  Inside, the token
;;;     has already been verified against the request's real method (POST), so
;;;     a request that forges anything at all has had to carry a valid token
;;;     first;
;;;   * outside the dispatch stage, because the rewritten method has to be in
;;;     place before the route is matched -- which is the whole point.
;;;
;;; The stage cannot see where it was put, so it asks the CSRF stage: that
;;; stage marks every request it lets through (+CSRF-MARKER-KEY+), and this
;;; one refuses to work on a request that carries no mark while CSRF is
;;; enabled.  See CHECK-STAGE-ORDER.
;;;
;;; Four conditions that keep the mechanism from becoming a hole
;;; -----------------------------------------------------------
;;;   1. POST only.  A GET carrying the field is left alone.  Without this,
;;;      appending ?_method=delete to any link would delete the resource it
;;;      points at -- measured, and the reason the restriction is spelled out
;;;      rather than assumed.
;;;   2. An explicit allow-list of state-changing methods
;;;      (+METHOD-OVERRIDE-METHODS+).  A request can never forge its way into
;;;      a method that is exempt from CSRF verification (GET, HEAD,
;;;      OPTIONS, ...), whatever it puts in the field.
;;;   3. The BODY only.  The field is read from the parsed request body and
;;;      never from the query string -- the same rule the CSRF token follows.
;;;      Note that a request's "parameters" are the query string and the body
;;;      added together, so reading parameters would have read the query
;;;      string too, which is how the pre-M9 legacy branch behaved.
;;;   4. An unknown value is ignored, not refused.  An empty field, a
;;;      misspelled method, a field whose value is not a string at all: the
;;;      request simply keeps its real method.  Nothing signals, and nothing
;;;      is rejected on account of the field alone.
;;;
;;; What changes for an existing project
;;; ------------------------------------
;;; Only a POST that actually carries the field, and only when the routing
;;; table has a route for the forged method on the same path.  Routes
;;; generated by `clails generate' declare no :method and match on the path
;;; alone, so they are unaffected; the legacy dispatch branch reaches the same
;;; receiver as before (it tests for "is this a PUT", which a rewritten
;;; request now is).  A hand-written table that declares both a POST route and
;;; a PUT (or DELETE) route on one path does change: such a POST now reaches
;;; the PUT route instead of the POST one.  That is the intended effect -- it
;;; is what lets a form reach an update route -- and it is pinned by a test
;;; (clails-test/middleware/method-override-roundtrip).

(defparameter +method-override-methods+ '(:put :patch :delete)
  "The methods a POST may be rewritten to: the allow-list, in full.

   These are the state-changing methods, which is what makes the allow-list a
   safety property rather than a convenience: every method NOT in this list
   is either unsafe-but-meaningless to forge or, worse, exempt from CSRF
   verification.  Letting a request forge GET / HEAD / OPTIONS would hand it a
   way to be served without a token, so they are not here, and nothing adds
   to this list at run time.

   PATCH is part of it (M9-T2): the token verification has always covered the
   four state-changing methods, so leaving PATCH out of the override -- and
   out of the routes RESOURCES generates -- left one method that was verified
   but unreachable.

   This is a public constant so that the form helper (M7) can render exactly
   the values this stage accepts, instead of keeping a second list that could
   drift from this one.  The comparison this list is used with is
   case-insensitive; see METHOD-OVERRIDE-VALUE.")

(defparameter +method-override-default-parameter+ "_method"
  "The default name of the body parameter carrying the forged method.

   Kept as a name of its own so that the convention is stated in one place:
   the legacy dispatch branch of the controller stage reads the same field
   name, and the form helper (M7) renders a hidden input of that name.")

(defvar *enable-method-override* t
  "Whether a POST may declare, in its body, that it means another method.

   T (the framework default) rewrites a POST carrying *method-override-parameter*
   into the method that field names, when that method is in
   +METHOD-OVERRIDE-METHODS+.  NIL makes the stage a pass-through: nothing is
   parsed, nothing is rewritten, and no configuration check is run -- a POST
   then stays a POST, and a form can no longer reach an update or destroy
   route of a resource.

   Why the default is T, when the session and CSRF stages default to NIL:
   those two change how every request is answered (a Set-Cookie on all of
   them, a 403 on unverified ones).  This one changes a request only when the
   request itself asks for it -- a POST that carries the field -- and the
   thing it then does is the documented purpose of that field.  A project that
   does not use the convention has nothing to opt into, and one that does
   would have to opt in just to get the behaviour its forms already assume.

   The one case where an existing project does change is a hand-written
   routing table that declares both a POST route and a PUT / PATCH / DELETE
   route on the same path; see the commentary at the top of this file.  That
   is what this setting is for:

     (setf clails/middleware/method-override-middleware:*enable-method-override* nil)

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *method-override-parameter* +method-override-default-parameter+
  "Name of the body parameter (hidden form field) the forged method is read
   from.

   Defaults to \"_method\", which is the name the legacy dispatch branch of
   the controller stage has always read and the name the form helper renders.
   Changing it here does NOT change the legacy branch's own fallback, so
   change it only if the application's forms are generated by something that
   insists on another name.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *method-override-forbidden-message*
  "Forbidden: the request-method override stage is running outside CSRF verification. Fix the middleware stack order."
  "Body of the 403 a request gets when the stack order is wrong and
   *strict-configuration* is true.

   It follows the body format M2 decided for a rejection -- one plain-text
   line, no markup -- and says what is wrong rather than what the client
   should do, because there is nothing the client can do: every request is
   refused until the configuration is fixed.  The reader of this line is the
   operator who just deployed the stack.

   This can be overridden in <project>/app/config/environment.lisp")


;;; ------------------------------------------------------------------
;;; Reading the forged method out of a request
;;; ------------------------------------------------------------------

(defun method-override-value (value)
  "The method VALUE names, or NIL when it names none of the allowed ones.

   The comparison is case-insensitive (\"put\", \"PUT\" and \"Put\" all name
   :PUT), which is the behaviour the legacy dispatch branch has always had --
   it compares with STRING-EQUAL -- and is kept so that a form written
   against the old behaviour keeps working.

   Anything else yields NIL, and NIL means \"leave the request alone\": an
   empty string, a misspelled method, a method outside the allow-list
   (\"get\"), a value that is not a string at all (a JSON number, a missing
   field).  No value of the field can make this function signal.

   @param value [t] The value the override field carried
   @return [keyword or null] The method to rewrite to, or NIL
   "
  (when (stringp value)
    (find value +method-override-methods+
          :test #'string-equal
          :key #'symbol-name)))

(defun body-override-value (env parameter)
  "The value ENV's BODY carries under PARAMETER, or NIL.

   The body and only the body: LACK/REQUEST:REQUEST-BODY-PARAMETERS is the
   body alone, whereas REQUEST-PARAMETERS is the query string and the body
   added together.  Reading the latter is what let a GET forge its method
   through the query string before M9, and it is also the rule the CSRF token
   follows, so the two stages read the same half of the request.

   A multipart body gives a parameter's value as a list (the value plus the
   part's metadata); as lack does elsewhere, the first element is taken.

   A body that cannot be parsed yields NIL rather than an error.  The stage
   must not turn an unparseable body into a server failure on its own: with
   CSRF enabled such a request has already been refused one stage further
   out (the CSRF body guard), and with CSRF disabled the body is nobody's
   business here -- the request simply keeps its real method.

   Parsing costs nothing extra downstream: LACK/REQUEST:MAKE-REQUEST caches
   what it parsed in the request environment, so the controller stage below
   reuses the result rather than parsing again (and when CSRF is enabled the
   parse has already happened there).

   @param env [list] Clack/Lack request environment plist
   @param parameter [string] Name of the body parameter to read
   @return [t] The value found (normally a string), or NIL
   "
  (handler-case
      (let* ((parameters (request-body-parameters (make-request env)))
             (entry (and (listp parameters)
                         (find-if (lambda (pair)
                                    (and (consp pair)
                                         (stringp (car pair))
                                         (string= (car pair) parameter)))
                                  parameters)))
             (value (cdr entry)))
        (if (listp value)
            (first value)
            value))
    (error () nil)))

(defun env-with-method (env method)
  "Return the environment ENV with its declared request method set to METHOD.

   The key is PREPENDED rather than assigned: GETF answers with the first
   occurrence it finds, so the new value shadows the old one and the
   environment the stages outside this one hold on to is left untouched.
   That is the same thing the CSRF stage does with its marker, and it keeps
   the rewrite from being visible anywhere but below this stage.

   Both route matching (CLAILS/CONTROLLER/BASE-CONTROLLER:PATH-CONTROLLER,
   via the controller stage) and LACK/REQUEST:MAKE-REQUEST read the method
   from this key, which is why rewriting this one value reaches the new-style
   route matching and the legacy dispatch branch at the same time.

   @param env [list] Clack/Lack request environment plist
   @param method [keyword] The method to declare
   @return [list] The environment to pass on
   "
  (list* :request-method method env))


;;; ------------------------------------------------------------------
;;; Checking the stack order (M9-T1, checked configuration 2)
;;; ------------------------------------------------------------------
;;;
;;; What is checked, and why it cannot be left to fail at run time
;;; -------------------------------------------------------------
;;; A stack whose stages are in the wrong order -- this stage outside the CSRF
;;; stage -- does not fail.  Nothing signals, every request is answered
;;; normally, and the only difference is that a forged method is honoured
;;; before the token is checked.  There is therefore no "let the request fail
;;; and the operator will see it" option: the misconfiguration is silent by
;;; nature, so it has to be detected deliberately.
;;;
;;; How it is detected: the marker, not the structure
;;; ------------------------------------------------
;;; The CSRF stage marks every request it lets through (+CSRF-MARKER-KEY+) and
;;; this stage looks for the mark.  No mark, with CSRF enabled, means the CSRF
;;; stage has not run outside this one.
;;;
;;; Walking the middleware list instead -- "is the CSRF stage before me?" --
;;; was rejected: the list is a sequence of already-built closures, and the
;;; public documentation itself shows two configurations that such a check
;;; would misjudge (wrapping a stage, and replacing the whole list).  The
;;; marker reports the order the request actually took, so it cannot
;;; misjudge either.
;;;
;;; What happens then is *STRICT-CONFIGURATION*'s decision; see
;;; CHECK-STAGE-ORDER.

(defvar *method-override-warning-issued* nil
  "Whether the stack-order warning has already been recorded in this process.

   The check runs per request, so without this the warning would be written
   once per request and drown the log it is meant to be noticed in.  It is
   deliberately per process rather than per stack: the point is that a human
   reads the line, and one line is what gets read.

   RESET-METHOD-OVERRIDE-WARNING puts it back, which is what lets a test
   assert \"once per process\" more than once.")

(defvar *method-override-warning-lock*
  (bt:make-lock "clails method override configuration warning")
  "Guards *METHOD-OVERRIDE-WARNING-ISSUED*.

   Requests are served on concurrent threads, so the test-and-set that makes
   the warning happen once has to be atomic; without the lock a burst of
   concurrent requests through a misconfigured stack could each find the flag
   unset and each write the line.")

(defun method-override-warning-issued-p ()
  "True when the stack-order warning has already been recorded in this process.

   @return [boolean] T once the warning has been written
   "
  (bt:with-lock-held (*method-override-warning-lock*)
    *method-override-warning-issued*))

(defun reset-method-override-warning ()
  "Forget that the stack-order warning has been recorded.

   Only tests need this: it is what lets one process assert both \"the
   warning is written\" and \"it is written once\" without the two
   assertions seeing each other's state.

   @return [null] nil
   "
  (bt:with-lock-held (*method-override-warning-lock*)
    (setf *method-override-warning-issued* nil))
  nil)

(defun claim-warning ()
  "Atomically take the right to write the stack-order warning.

   @return [boolean] T for the first caller in this process, NIL afterwards
   "
  (bt:with-lock-held (*method-override-warning-lock*)
    (if *method-override-warning-issued*
        nil
        (setf *method-override-warning-issued* t))))

(defparameter +stage-order-message+
  (concatenate
   'string
   "clails request-method override middleware is running OUTSIDE the CSRF verification stage "
   "(the request carries no CSRF marker, see clails/middleware/csrf-middleware:+csrf-marker-key+). "
   "In that order a request could forge its method before its token is verified. "
   "Fix the order in app/config/environment.lisp: the override stage must come AFTER "
   "clails/middleware/csrf-middleware:*lack-middleware-clails-csrf* in "
   "clails/middleware/core:*clails-middleware-stack* (the framework's default stack has it there), "
   "or disable one of the two "
   "((setf clails/middleware/method-override-middleware:*enable-method-override* nil)). "
   "While *strict-configuration* is nil the request is served, but WITHOUT the method override.")
  "What a misordered stack is told, in both the rejection and the warning.

   One text for the two outcomes on purpose: strict or not, the mistake and
   its fix are the same, and only the consequence for this request differs.")

(defun check-stage-order (env csrf-enabled)
  "Decide what to do about ENV when the stages may be in the wrong order.

   Returns one of three answers:

     :OK         -- the order is right, or there is nothing to check
                    (CSRF disabled), so carry on and rewrite;
     :REJECT     -- the order is wrong and *STRICT-CONFIGURATION* is true:
                    the caller must answer 403 and must NOT rewrite;
     :PASS-THROUGH -- the order is wrong and *STRICT-CONFIGURATION* is nil:
                    the caller serves the request, but without rewriting,
                    and a warning has been recorded (once per process).

   Why :PASS-THROUGH does not rewrite.  The decided non-strict behaviour is
   \"warn and carry on\", and carrying on means the request is served -- it is
   a development environment, and refusing every request there would be
   indistinguishable from the strict behaviour.  What it does NOT do is honour
   the forgery, because honouring it is the one thing the order makes unsafe.
   A form that needs the override therefore stops working, loudly (a 404 from
   the route that was not matched) rather than silently, and the warning says
   why.

   The check is made on EVERY request, not only on a POST carrying the field:
   the configuration is wrong for all of them, and a GET that is answered
   normally would hide it until the first form was submitted.

   CSRF-ENABLED is passed in rather than read here, because the stage reads
   its configuration once when the stack is built -- the same contract the
   session and CSRF stages have.  A project that does not use the CSRF stage
   is never checked: there is no marker to expect, and a user who has not
   opted into CSRF must not be told about the order of stages that cannot
   harm them.

   *STRICT-CONFIGURATION*, by contrast, is read per request: the check itself
   is a per-request one, and an operator who flips the setting in a running
   image gets the new behaviour without rebuilding the stack.  The warning is
   written once per process (see *METHOD-OVERRIDE-WARNING-ISSUED*), whereas a
   REJECTION is recorded every time, like every other 403 -- a refused
   request is an event, not a configuration notice.

   @param env [list] Clack/Lack request environment plist
   @param csrf-enabled [boolean] Whether the CSRF stage was enabled when the stack was built
   @return [keyword] :OK, :REJECT or :PASS-THROUGH
   "
  (cond
    ((not csrf-enabled) :ok)
    ((getf env +csrf-marker-key+) :ok)
    (*strict-configuration* :reject)
    (t
     (when (claim-warning)
       (log-message-to :audit :warn +stage-order-message+
                       :stage :method-override
                       :check :csrf-marker))
     :pass-through)))


;;; ------------------------------------------------------------------
;;; The stage
;;; ------------------------------------------------------------------

(defun reject (env)
  "Refuse the request: record it on the :audit logger and answer 403.

   The rejection is M2's (clails/middleware/rejection), so a request refused
   here is recorded and shaped exactly like one refused by the CSRF stage.

   @param env [list] Clack/Lack request environment plist
   @return [list] Lack response as (status headers body)
   "
  (reject-forbidden env
                    :reason "Request rejected: method override stage is outside the CSRF stage"
                    :message *method-override-forbidden-message*
                    :context (list :stage :method-override
                                   :check :csrf-marker)))

(defparameter *lack-middleware-clails-method-override*
  (lambda (app)
    "Clails request-method override middleware.

     Rewrites a POST that carries *method-override-parameter* in its body
     into the method that field names, so that an HTML form can reach a
     route declared for PUT, PATCH or DELETE.  Only a POST, only from the
     body, only into +METHOD-OVERRIDE-METHODS+, and only when the CSRF stage
     (if enabled) has already verified the request.

     Must sit inside the CSRF verification stage and outside the controller
     stage; that is where clails/middleware/core:*clails-middleware-stack*
     puts it, and a request that arrives without the CSRF stage's marker is
     refused or served unrewritten rather than trusted (see
     CHECK-STAGE-ORDER).

     The enabling flag, the field name and whether CSRF is in use are read
     once, here, when the stack is built -- the same contract the session and
     CSRF stages have -- so a stack cannot be reconfigured after the fact,
     and every test builds its own.  (*STRICT-CONFIGURATION* is the one
     setting read per request; see CHECK-STAGE-ORDER for why.)
     "
    (if (not *enable-method-override*)
        app
        (let ((parameter *method-override-parameter*)
              (csrf-enabled *enable-csrf-middleware*))
          (when (log-level-enabled-p :trace)
            (log-package.trace "Method override middleware: enabled"))
          (lambda (env)
            (let ((order (check-stage-order env csrf-enabled)))
              (cond
                ((eq order :reject)
                 (reject env))
                ;; Anything but a POST keeps its method, whatever it carries:
                ;; condition 1 of the four, and the one that keeps
                ;; "?_method=delete" on a link from deleting anything.
                ((not (eq (getf env :request-method) :post))
                 (funcall app env))
                ;; The order is wrong but strict is off: serve the request,
                ;; without honouring the forgery.
                ((eq order :pass-through)
                 (funcall app env))
                (t
                 (let ((method (method-override-value (body-override-value env parameter))))
                   (cond
                     (method
                      (when (log-level-enabled-p :trace)
                        (log-package.trace
                         (format nil "Method override: POST rewritten to ~A" method)))
                      (funcall app (env-with-method env method)))
                     ;; No field, an empty one, or a value outside the
                     ;; allow-list: condition 4 of the four.  Nothing
                     ;; signals and nothing is refused.
                     (t
                      (funcall app env)))))))))))
  "Lack middleware function rewriting a POST into the method its body declares.")
