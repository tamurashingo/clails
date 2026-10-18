(in-package #:cl-user)
(defpackage #:clails/middleware/session/state/cookie
  (:use #:cl)
  ;; NOTE: lack-middleware-session is one ordinary (non-package-inferred) ASDF
  ;; system that defines several packages (lack/middleware/session,
  ;; lack/middleware/session/state, .../state/cookie, ...).  Only its
  ;; top-level package is aliased as a system (lack.asd declares the system
  ;; "lack/middleware/session"), so this is the only one of those names that
  ;; may appear in a clause here: an :import-from for one of the sub-packages
  ;; would make clails' package-inferred-system try (and fail) to find an ASDF
  ;; component named after it.  The state protocol this file derives from and
  ;; specialises lives in LACK/MIDDLEWARE/SESSION/STATE and is therefore
  ;; written with fully-qualified symbols below; loading it is guaranteed by
  ;; this (symbol-less) dependency on the system that contains it.  The same
  ;; trap is explained at length in clails/middleware/session/store.
  (:import-from #:lack/middleware/session)
  ;; lack/request and lack/response, in contrast, DO have forwarding systems
  ;; of their own in lack.asd, so they may be named in a clause.
  (:import-from #:lack/request
                #:make-request
                #:request-cookies)
  (:import-from #:lack/response
                #:make-response
                #:finalize-response
                #:response-set-cookies)
  (:export #:clails-cookie-state
           #:clails-cookie-state-p
           #:make-clails-cookie-state
           #:clails-cookie-state-path
           #:clails-cookie-state-domain
           #:clails-cookie-state-secure
           #:clails-cookie-state-httponly
           #:clails-cookie-state-cookie-key
           #:clails-cookie-state-samesite
           #:+default-session-cookie-name+
           #:+cookie-deletion-time+))
(in-package #:clails/middleware/session/state/cookie)


;;; ------------------------------------------------------------------
;;; The session cookie clails emits (M5-T4)
;;; ------------------------------------------------------------------
;;;
;;; The session stage needs a "state": the object that knows how a session id
;;; travels between the server and the client.  The underlying library ships
;;; one (lack/middleware/session/state/cookie), and clails used it unchanged
;;; until this milestone.  Three of its defaults had to change -- and one of
;;; them could not be changed through its constructor at all, which is why
;;; this file exists.
;;;
;;; What changed, and why
;;; ---------------------
;;;   (1) HttpOnly is now ON by default.  A session cookie that scripts can
;;;       read is a session id one cross-site script can steal.  Nothing in a
;;;       browser needs to read it: only the server does.
;;;   (2) SameSite is stated explicitly (:LAX).  Leaving it out is not
;;;       "unspecified": the response builder underneath defaults the
;;;       attribute to None AND warns that "Samesite=None cookies require
;;;       Secure", so an unspecified value both weakens the cookie and makes
;;;       the framework warn on every response.
;;;   (3) The cookie name no longer names the library that implements the
;;;       session.  "lack.session" tells an attacker which stack is running
;;;       and therefore which bugs to try; +DEFAULT-SESSION-COOKIE-NAME+ says
;;;       nothing about the implementation.
;;;   (4) NO EXPIRY DATE IS EMITTED.  This is the one that needed a state of
;;;       clails' own, and it is the heart of this file -- see below.
;;;
;;; Secure is NOT turned on here.  It is read from a configuration variable
;;; (clails/middleware/session-middleware:*SESSION-COOKIE-SECURE*) whose
;;; framework default is false, because a cookie marked Secure is not sent
;;; over plain HTTP at all: defaulting it to true would break every
;;; development environment that runs without TLS, i.e. every developer who
;;; has not configured anything.  Putting the true value in for production is
;;; the generated project's job (M8-T1), and the framework never inspects the
;;; environment it is running in.
;;;
;;; Why a cookie with no expiry date, and why a state of our own
;;; -----------------------------------------------------------
;;; clails expires sessions by IDLE TIME: a session dies 30 minutes (by
;;; default) after its last use, and every request pushes that deadline
;;; forward.  The cookie cannot express that policy, for a measured reason:
;;; the session stage attaches Set-Cookie ONLY when the session is created,
;;; when its id changes, or when it is destroyed.  An ordinary continuing
;;; request carries no Set-Cookie at all, so an expiry date put on the cookie
;;; is never refreshed -- it would turn into an ABSOLUTE deadline counted
;;; from the moment the session was created, which is a different policy from
;;; the one the server enforces.  A user actively working would be logged out
;;; 30 minutes after logging in.
;;;
;;; So the cookie carries no expiry date: it is a session cookie in the
;;; browser's sense (gone when the browser closes), and the server's idle
;;; timeout is the only deadline.  Two ways of saying that were tried first
;;; and are deliberately NOT used:
;;;
;;;   * passing NIL as the expiry.  The library's own cookie state declares
;;;     the slot as (OR INTEGER), so NIL fails at construction time -- a
;;;     type error, not a cookie without an expiry date;
;;;   * passing 0.  That does not mean "no expiry": the state adds the
;;;     current time to whatever it is given, so 0 means "expires now",
;;;     i.e. a cookie the browser deletes immediately.  (That IS what we
;;;     want for a destroyed session, and it is how EXPIRE-STATE below
;;;     works -- but as the deliberate deletion of a cookie, never as the
;;;     default.)
;;;
;;; Since the attribute cannot be left out through the constructor, the state
;;; itself is defined here: a structure including the library's STATE (so the
;;; id generator and the 40-hex-character id validator are inherited) with
;;; its own FINALIZE-STATE, which simply never puts an expiry date in the
;;; cookie it bakes.  The response builder omits the attribute when it is
;;; given NIL, so "no expiry date" is expressible once the state is ours.
;;;
;;; As a side effect this file also fixes a defect of the default: the
;;; library's state initialises its expiry slot to GET-UNIVERSAL-TIME at the
;;; moment the state is created and then emits (+ (GET-UNIVERSAL-TIME) that),
;;; which is roughly twice the current universal time -- a cookie that
;;; expires in the year 2153.  No clails response carries that date any more.
;;;
;;; An application that really wants a persistent cookie (one that survives
;;; the browser being closed) can still put the library's own state in
;;; *SESSION-STATE*; nothing here prevents that, and the idle timeout keeps
;;; applying on the server side.


(defparameter +default-session-cookie-name+ "_session_id"
  "The name of the cookie the session id travels in, by default.

   Deliberately says nothing about the implementation.  The previous default
   came from the underlying library and was called \"lack.session\", which
   told anyone looking at a response which library -- and therefore which
   version-specific weaknesses -- to try.  An application that wants a name
   of its own passes :COOKIE-KEY to MAKE-CLAILS-COOKIE-STATE.")

(defparameter +cookie-deletion-time+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "The expiry date put on a cookie that is being deleted: the Unix epoch, i.e.
   the conventional \"Thu, 01 Jan 1970 00:00:00 GMT\" of a Set-Cookie that
   asks the client to forget the cookie.

   Used only by the destroyed-session path (EXPIRE-STATE); an ordinary
   response carries no expiry date at all.")


;;; The default keyword constructor (MAKE-CLAILS-COOKIE-STATE) is what the
;;; session stage and applications use.  Nothing is spelled out about the two
;;; inherited slots -- the id generator and the id validator -- on purpose:
;;; including the library's STATE brings both in with their own defaults, so
;;; clails keeps the 40-hexadecimal-character ids and their validation
;;; without restating either.  Note the absence of an EXPIRES slot: there is
;;; no way to ask this state for a cookie with an expiry date, which is the
;;; whole point (see the file header).

(defstruct (clails-cookie-state
            (:include lack/middleware/session/state:state))
  (path "/" :type string)
  (domain nil :type (or string null))
  (secure nil :type boolean)
  (httponly t :type boolean)
  (cookie-key +default-session-cookie-name+ :type string)
  (samesite :lax :type keyword))


(defmethod lack/middleware/session/state:extract-sid ((state clails-cookie-state) env)
  "Read the session id out of the request's cookies.

   The id is still validated before it is used, by the :AROUND method the
   included STATE provides: a value that is not 40 hexadecimal characters is
   discarded here and the stage mints a fresh session instead.  (A
   well-formed id that no longer names a session is a different case, and is
   handled by the expiry stage -- see clails/middleware/session/expiry.)

   @param state [clails-cookie-state] Cookie state
   @param env [list] Request environment
   @return [(or string null)] The session id the client sent, or NIL
   "
  (let ((request (make-request env)))
    (cdr (assoc (clails-cookie-state-cookie-key state)
                (request-cookies request)
                :test #'string=))))

(defmethod lack/middleware/session/state:expire-state ((state clails-cookie-state) sid res options)
  "Answer RES with a Set-Cookie that deletes the session cookie.

   Called by the session stage when the session was destroyed (:EXPIRE in the
   session options, which is what SESSION-DESTROY sets).  The baking below
   reads the same :EXPIRE flag out of OPTIONS, so this method only has to
   delegate.

   @param state [clails-cookie-state] Cookie state
   @param sid [string] Session id being retired
   @param res [t] The response, or a function for a streamed response
   @param options [list] The session options plist
   @return [t] The response
   "
  (lack/middleware/session/state:finalize-state state sid res options))

(defmethod lack/middleware/session/state:finalize-state ((state clails-cookie-state) sid (res function) options)
  "Streamed-response variant: defer to the response the application finally
   produces.

   @param state [clails-cookie-state] Cookie state
   @param sid [string] Session id
   @param res [function] A streamed response
   @param options [list] The session options plist
   @return [function] A streamed response
   "
  (lambda (responder)
    (funcall res
             (lambda (actual-response)
               (funcall responder
                        (lack/middleware/session/state:finalize-state
                         state sid actual-response options))))))

(defmethod lack/middleware/session/state:finalize-state ((state clails-cookie-state) sid (res list) options)
  "Attach the session cookie to RES, when the response needs one.

   A response needs one only when the session was just created, when its id
   changed, or when it was destroyed; an ordinary continuing request is
   answered untouched (which is why a cookie expiry date could never be
   refreshed, see the file header).

   The baked cookie carries path, HttpOnly, SameSite and -- when configured
   -- domain and Secure, and NO expiry date, unless the session is being
   destroyed, in which case the date is the epoch so that the client deletes
   the cookie.

   @param state [clails-cookie-state] Cookie state
   @param sid [string] Session id to put in the cookie
   @param res [list] The response triple
   @param options [list] The session options plist
   @return [list] The response triple, with Set-Cookie when one is due
   "
  (destructuring-bind (&key no-store new-session change-id expire &allow-other-keys)
      options
    (if (or no-store
            (not (or new-session change-id expire)))
        res
        (let ((response (apply #'make-response res)))
          (setf (getf (response-set-cookies response)
                      (clails-cookie-state-cookie-key state))
                (list :value sid
                      :path (clails-cookie-state-path state)
                      :domain (clails-cookie-state-domain state)
                      :secure (clails-cookie-state-secure state)
                      :httponly (clails-cookie-state-httponly state)
                      :samesite (clails-cookie-state-samesite state)
                      ;; NIL here is what keeps the attribute out of the
                      ;; header entirely: the response builder emits
                      ;; "expires=" only for a non-NIL value.  A destroyed
                      ;; session is the one exception -- there the date is
                      ;; the point, because it is what makes the client drop
                      ;; the cookie.
                      :expires (when expire +cookie-deletion-time+)))
          (finalize-response response)))))
