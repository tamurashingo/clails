(in-package #:cl-user)
(defpackage #:clails/middleware/session/expiry
  (:use #:cl)
  ;; NOTE: only lack-middleware-session's top-level package name may appear in
  ;; a clause here (see the long explanation in
  ;; clails/middleware/session/store); the state protocol used below --
  ;; LACK/MIDDLEWARE/SESSION/STATE:EXTRACT-SID -- is written fully qualified.
  (:import-from #:lack/middleware/session)
  (:import-from #:clails/middleware/session/clock
                #:session-clock-now
                #:session-idle-seconds)
  (:import-from #:clails/middleware/session/store
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/lifecycle
                #:clear-session-table
                #:session-regenerate-id)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log-package.debug
                #:log-package.trace)
  (:export #:*session-idle-timeout*
           #:session-idle-expired-p
           #:session-expiry-reason
           #:+session-expiry-env-key+
           #:wrap-session-expiry))
(in-package #:clails/middleware/session/expiry)


;;; ------------------------------------------------------------------
;;; Idle-timeout expiry (M5-T3, G11)
;;; ------------------------------------------------------------------
;;;
;;; The policy (decision 6): a session dies when it has not been USED for
;;; *SESSION-IDLE-TIMEOUT* seconds -- 30 minutes by default.  Every request
;;; counts as a use and pushes the deadline forward, so a user who keeps
;;; working is never logged out, and a user who walks away is.  That is the
;;; relative, "idle time" policy, not an absolute lifetime counted from the
;;; moment the session was created.
;;;
;;; Three things have to happen for a request whose session is past the
;;; limit, and all three are here:
;;;
;;;   (1) the session must come out EMPTY.  The request still has to work --
;;;       it is an ordinary request that happens to arrive late -- so it gets
;;;       a usable session with nothing in it, not an error.
;;;   (2) the session id must CHANGE.  This is the part that is easy to get
;;;       wrong, and getting it wrong is a security hole; see "Why the id has
;;;       to change" below.
;;;   (3) the dead session must LEAVE THE STORE, right now, on the way in.
;;;       That is the "lazy deletion" half of decision 6; the periodic
;;;       sweeper (clails/middleware/session/sweeper) is the other half, for
;;;       sessions nobody ever comes back for.
;;;
;;; Where the decision is made, and why it cannot be made in the store
;;; ------------------------------------------------------------------
;;; The obvious-looking implementation is to teach the store to answer "no
;;; such session" for an entry that is too old -- expiry entirely inside
;;; FETCH-SESSION, with no stage involved.  THAT IS NOT WHAT THIS DOES, and
;;; the reason was measured rather than reasoned:
;;;
;;;   the session stage reuses the id the client presented when the store
;;;   returns nothing.  It extracts the id, asks the store, gets NIL, and
;;;   then opens a NEW, EMPTY session UNDER THAT SAME ID.  An id that leaked
;;;   -- the whole reason for having an idle timeout -- therefore stays valid
;;;   for ever: every expiry hands the attacker's id straight back, and the
;;;   victim's next login happens under the id the attacker is holding.  A
;;;   "pretend it is not there" store turns an expiry mechanism into a
;;;   session-fixation mechanism.
;;;
;;; So the decision is made HERE, one stage INSIDE the session stage, where
;;; both halves of the session are visible: its contents (to empty them) and
;;; its options (to ask for a new id).  A stage outside the session stage
;;; could not do it -- outside, there is no session yet.  This is the same
;;; position WRAP-SESSION-CONTAINER occupies, and for the same reason.
;;;
;;; Why the id has to change, in both cases
;;; ---------------------------------------
;;; Two different situations arrive at the same door, and both are handled
;;; the same way -- a new id -- because in both the id the client presented
;;; must not be allowed to carry on:
;;;
;;;   * a session that WAS there and has been idle too long.  Continuing
;;;     under the presented id would mean a stolen id never stops working.
;;;   * an id that could NOT BE RESOLVED at all: a session the sweeper
;;;     already removed, an id from a process that has restarted, a forged
;;;     id that happens to look well-formed.  The stage has already decided
;;;     to open a fresh session for it -- under the presented id, as
;;;     described above -- so this stage takes the id away again.
;;;
;;; (An id that is not even well-formed never gets this far: the state's
;;; validator rejects anything that is not 40 hexadecimal characters, and the
;;; stage mints a fresh id before this stage runs.)
;;;
;;; Asking for a new id is all this stage does about it; the mechanics are the
;;; session stage's: at the end of the request it mints an id, stores the
;;; session under it, removes the old id from the store, and the response
;;; carries the new id in its cookie.  So the swap costs one extra store
;;; operation and nothing else.
;;;
;;; Where the last-access time lives
;;; --------------------------------
;;; In the store, never in the session value (decision 11).  The value is the
;;; table the application owns; writing a timestamp into it would make every
;;; request -- including the concurrent static-asset requests one page load
;;; fans out -- a writer of the session, which is precisely the concurrency
;;; hazard M4 exists to remove.  The time is therefore recorded with the
;;; store protocol's fourth operation, TOUCH-SESSION, which updates nothing
;;; but the time (one index entry for the in-memory store, one column for the
;;; database store) and leaves the session value untouched, contents and
;;; identity.
;;;
;;; And it is not a repurposed updated_at: the store owns a column of its own
;;; and writes it on every store and every touch, because a column maintained
;;; by something else is not written when a request only reads -- which under
;;; an idle-timeout policy would expire the users who are actively browsing.

(defvar *session-idle-timeout* 1800
  "How long a session may stay unused before it expires, in seconds.

   30 minutes by default.  Every request that carries a live session pushes
   the deadline forward, so this is an IDLE time, not a session lifetime: a
   user who keeps working is never logged out.

   Set it to NIL to switch server-side expiry off entirely -- sessions then
   live until they are destroyed or the store loses them.  Doing so also
   leaves the periodic sweeper with nothing to remove.

     (setf clails/middleware/session-middleware:*session-idle-timeout*
           (* 60 15))   ; 15 minutes

   Note that the cookie carries no expiry date, by design: a cookie is only
   re-sent when the session is created, renamed or destroyed, so a date on it
   would become an absolute deadline and contradict this setting.  See
   clails/middleware/session/state/cookie.

   This can be overridden in <project>/app/config/environment.lisp")

(defparameter +session-expiry-env-key+ :clails.session.expired
  "Request-environment key the stage sets, to the reason, when it has just
   expired or replaced the session of this request.

   Two values: :IDLE-TIMEOUT when a live session was found to be past the
   limit, and :UNKNOWN-ID when the id the client presented named no session
   at all.  Nothing in the framework reads it; it is there so that an
   application (or a test) can tell \"this request's session was reset\" from
   \"this request started a session\", which the session options alone cannot
   express.")


(defun session-idle-expired-p (last-access-time
                               &optional (idle-timeout *session-idle-timeout*)
                                         (now (session-clock-now)))
  "True when a session last used at LAST-ACCESS-TIME has been idle for longer
   than IDLE-TIMEOUT seconds.

   The predicate the whole feature turns on, and the one place the comparison
   is written:

     * a NIL IDLE-TIMEOUT means expiry is switched off -- nothing is ever
       expired;
     * a NIL LAST-ACCESS-TIME means the store does not know when the session
       was last used, which is not evidence that it is old: it is treated as
       not expired, so that a store without a last-access time keeps working
       the way it did before this feature existed;
     * exactly IDLE-TIMEOUT seconds of idleness is NOT expired; one second
       more is.

   @param last-access-time [(or integer null)] Universal time of the last use
   @param idle-timeout [(or integer null)] Limit in seconds, or NIL for none
   @param now [integer] Universal time to measure against
   @return [boolean] T when the session is past the limit
   "
  (and idle-timeout
       last-access-time
       (> (session-idle-seconds last-access-time now) idle-timeout)
       t))


;;; ------------------------------------------------------------------
;;; The stage
;;; ------------------------------------------------------------------

(defun %presented-session-id (state env)
  "The session id the client sent, as the state reads it, or NIL.

   Asked through the state's own EXTRACT-SID so that this works for any state
   -- a cookie state, or something else an application configured -- and so
   that the id is validated the same way the session stage validated it.  The
   cookies have already been parsed and cached in the environment by the
   stage, so this costs no re-parsing."
  (handler-case
      (lack/middleware/session/state:extract-sid state env)
    (error ()
      ;; A state that cannot read this request tells us nothing about the id;
      ;; treating that as "no id was presented" is the conservative answer,
      ;; because the only thing the answer is used for is to decide whether
      ;; to take an id away.
      nil)))

(defun session-expiry-reason (env)
  "Why the session of the request ENV describes was reset, or NIL.

   @param env [list] Request environment
   @return [(or keyword null)] :IDLE-TIMEOUT, :UNKNOWN-ID or NIL
   "
  (getf env +session-expiry-env-key+))

(defun %env-put (env key value)
  "Put KEY -> VALUE into the request environment ENV, IN PLACE.

   (SETF (GETF env key)) is wrong here and the reason is easy to miss: when
   the key is absent, GETF's expansion conses a new plist and assigns it to
   the VARIABLE -- so a function that does it changes only its own argument,
   and the environment its caller (and the session stage, which reads the
   same plist back when it finalises the response) is holding is untouched.
   Appending to the last cons of the existing list mutates the structure
   everybody shares, which is what the layer below does for the same reason."
  (let ((cell (loop for tail on env by #'cddr
                    when (eq (car tail) key) return tail)))
    (if cell
        (setf (second cell) value)
        (setf (cdr (last env)) (list key value)))
    value))

(defun %mark-expired (env reason)
  (%env-put env +session-expiry-env-key+ reason))

(defun apply-session-idle-policy (env store state)
  "Decide what this request's session is worth, and act on it.

   Called once per request, before the application runs, with the session
   already in ENV.  One of four things happens:

     * the id the client presented named no session: the id is taken away
       (a new one is minted for the response);
     * the session is past the idle limit: it is emptied, removed from the
       store, and the id is taken away;
     * the session is live: the access is recorded with the store's fourth
       operation, which pushes the deadline forward;
     * the request carries no session id at all (a first visit): nothing to
       decide.

   @param env [list] Request environment
   @param store [t] The store the session stage was configured with
   @param state [t] The state the session stage was configured with
   @return [(or keyword null)] The expiry reason, or NIL when nothing happened
   "
  (let* ((options (getf env :lack.session.options))
         (sid (getf options :id))
         (session (getf env :lack.session))
         (new-session-p (getf options :new-session))
         (now (session-clock-now)))
    (cond
      ;; Not a session at all: nothing in the environment to judge.
      ((or (null sid) (null options)) nil)

      ;; (1) The client presented an id the stage could not resolve.  The
      ;;     stage has already decided to open a fresh session under that
      ;;     very id, which is what must not happen -- see the file header.
      ((and new-session-p (%presented-session-id state env))
       (session-regenerate-id env)
       (%mark-expired env :unknown-id)
       (when (log-level-enabled-p :debug)
         (log-package.debug "Session middleware: unresolved session id presented; a new id will be issued"))
       :unknown-id)

      ;; A session the stage has just created, with no id presented: ordinary
      ;; first visit.  There is nothing in the store yet to touch -- the
      ;; stage stores it at the end of the request, and storing counts as an
      ;; access.
      (new-session-p nil)

      ;; (2) A continuing session that has been idle for too long.
      ((session-idle-expired-p (session-last-access-time store sid)
                               *session-idle-timeout*
                               now)
       (when (hash-table-p session)
         ;; Emptied, not replaced: the container is the one the stage and the
         ;; application share, and it is already safe for concurrent access.
         (clear-session-table session))
       ;; Lazy deletion (G12): the dead session leaves the store at the
       ;; moment it is accessed.  The stage would remove this id anyway when
       ;; it renames the session, but doing it here is what makes "access
       ;; expires it" true even if the response never gets that far.
       (remove-session store sid)
       (session-regenerate-id env)
       (%mark-expired env :idle-timeout)
       (when (log-level-enabled-p :debug)
         (log-package.debug "Session middleware: session expired after idle timeout; a new id will be issued"))
       :idle-timeout)

      ;; (3) A live session: record the access.  TOUCH-SESSION writes nothing
      ;;     but the time -- see the file header on why that matters.
      (t
       (touch-session store sid now)
       (when (log-level-enabled-p :trace)
         (log-package.trace "Session middleware: session access recorded"))
       nil))))

(defun wrap-session-expiry (app store state)
  "Wrap APP in the stage that enforces the server-side idle timeout.

   Belongs INSIDE the session stage -- it needs the session and its options,
   which only exist there -- and outside the application, which must see the
   session the policy decided on.  STORE and STATE are the ones the session
   stage was configured with.

   @param app [function] The application to wrap
   @param store [t] The session store in use
   @param state [t] The session state in use
   @return [function] An application
   "
  (lambda (env)
    (apply-session-idle-policy env store state)
    (funcall app env)))
