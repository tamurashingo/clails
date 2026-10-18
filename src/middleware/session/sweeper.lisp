(in-package #:cl-user)
(defpackage #:clails/middleware/session/sweeper
  (:use #:cl)
  (:import-from #:clails/environment
                #:add-startup-hook
                #:add-shutdown-hook)
  (:import-from #:clails/periodic
                #:make-periodic-waiter
                #:reset-periodic-waiter
                #:signal-periodic-waiter
                #:run-periodic-loop)
  (:import-from #:clails/middleware/session/clock
                #:session-clock-now)
  (:import-from #:clails/middleware/session/store
                #:remove-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/wrapper
                #:<session-store-wrapper>
                #:session-store-wrapper-index-entries
                #:session-store-wrapper-index-count)
  (:import-from #:clails/middleware/session/expiry
                #:*session-idle-timeout*
                #:session-idle-expired-p)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log-package.debug
                #:log-package.info)
  (:export #:*session-sweep-enabled*
           #:*session-sweep-interval*
           #:*session-sweeper-thread*
           #:*session-sweeper-stop-requested*
           #:register-session-sweep-store
           #:forget-session-sweep-stores
           #:session-sweep-stores
           #:sweep-idle-sessions
           #:sweep-all-registered-stores
           #:run-session-sweeper-loop
           #:start-session-sweeper
           #:stop-session-sweeper
           #:session-sweeper-running-p))
(in-package #:clails/middleware/session/sweeper)


;;; ------------------------------------------------------------------
;;; The periodic sweeper (M5-T3, G12)
;;; ------------------------------------------------------------------
;;;
;;; Decision 6 asks for BOTH halves of session cleanup:
;;;
;;;   lazy deletion   an expired session is removed the moment somebody
;;;                   presents its id (clails/middleware/session/expiry);
;;;   periodic sweep  expired sessions are removed even when nobody ever
;;;                   comes back for them -- which is the normal case, since
;;;                   a user who abandons a session by definition sends no
;;;                   further request.  Without this half, the in-memory
;;;                   store grows for the lifetime of the process and the
;;;                   session table grows for ever.
;;;
;;; Where the list of sessions comes from
;;; -------------------------------------
;;; Neither the clails store protocol nor lack's has an "enumerate every
;;; session" operation, on purpose -- it is the operation a key/value store
;;; is least likely to be able to provide.  So the sweeper cannot ask a
;;; store what it holds.  What it can do is read the index kept by the
;;; wrapping store the session stage installs around the configured store:
;;; that object sees every store, touch and remove go past, so it knows which
;;; ids are live.  The index is the candidate list; the last-access time to
;;; compare against is read from the store itself
;;; (SESSION-LAST-ACCESS-TIME), because the time is store-side state
;;; (decision 11) and the index must not become a second truth.
;;;
;;; Why not the job queue
;;; ---------------------
;;; clails has a job queue (clails/job), and "run something every five
;;; minutes" sounds exactly like a job.  It cannot be one, for three
;;; independent reasons, each of which is fatal on its own:
;;;
;;;   (1) THE WORKER IS A DIFFERENT PROCESS.  Jobs are claimed from a table
;;;       by `clails job:work', which runs outside the web server.  The
;;;       default session store keeps its sessions in the MEMORY OF THE
;;;       SERVER PROCESS, and no amount of work in another process can free
;;;       them.  A sweeper has to run inside the image that owns the sessions.
;;;   (2) THERE IS NO RECURRING SCHEDULE.  The queue runs a job once, at or
;;;       after a given time; it has no notion of "every N seconds".  A job
;;;       would have to re-enqueue itself, and a single lost enqueue -- a
;;;       crash, a failed insert -- silently ends cleanup for ever.
;;;   (3) IT REQUIRES A DATABASE TABLE.  The queue is a table, so sessions
;;;       could only be swept by applications that have a database and have
;;;       migrated it.  Session expiry has to work for the default
;;;       configuration, which has neither.
;;;
;;; Hence a background thread, started from the framework's startup hook and
;;; stopped from its shutdown hook, with its loop mechanics in
;;; clails/periodic so that the next periodic job in clails does not have to
;;; reinvent them.
;;;
;;; A hard kill runs no hooks
;;; -------------------------
;;; SIGKILL, a power cut, or a crash take the process without running the
;;; shutdown hook, so the sweeper thread is simply gone with everything else.
;;; That is accounted for rather than handled:
;;;
;;;   * with the in-memory store the sessions die with the process, so there
;;;     is nothing left to clean up;
;;;   * with a persistent store the rows stay behind -- and are removed by
;;;     the next process's sweeper, or by the lazy deletion if someone
;;;     presents one of those ids.  Nothing accumulates permanently and no
;;;     expired session is ever honoured, because the expiry decision is made
;;;     on every request from the stored timestamp, never from "the sweeper
;;;     has not got to it yet".
;;;
;;; So no crash-recovery machinery is needed, and none is written.

(defvar *session-sweep-enabled* t
  "Whether the periodic session sweeper runs.

   True by default: the server's startup hook starts the sweeper thread, and
   its shutdown hook stops it.  Set it to NIL to leave cleanup entirely to
   the lazy deletion that happens when an expired session is accessed --
   correct, but it never frees a session nobody comes back for.

     (setf clails/middleware/session-middleware:*session-sweep-enabled* nil)

   The sweeper also does nothing when the session stage is disabled, because
   it only sweeps stores the stage registered, and a disabled stage registers
   none.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-sweep-interval* 300
  "How often the periodic sweeper looks for expired sessions, in seconds.

   Five minutes by default.  The interval is a trade-off and not a deadline:
   an expired session is never honoured, whatever the interval -- the expiry
   decision is made on every request -- so a longer interval only means
   expired sessions occupy memory (or rows) a little longer, and a shorter
   one means more passes over the index.

   Stopping the sweeper does not wait for the current interval to elapse (see
   clails/periodic), so a long interval costs nothing at shutdown.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *session-sweeper-thread* nil
  "The bordeaux-threads thread started by START-SESSION-SWEEPER, or NIL when
   the sweeper is not running.")

(defvar *session-sweeper-stop-requested* nil
  "When true, the sweeper loop exits at the start of its next iteration --
   and, because the loop waits on a condition variable rather than sleeping,
   it notices immediately rather than at the end of the interval.")

(defvar *session-sweeper-waiter* (make-periodic-waiter :name "clails session sweeper")
  "What the sweeper loop waits on between passes.  Signalling it is what makes
   a stop request take effect at once.")

(defvar *session-sweep-stores* '()
  "The stores the sweeper walks: the wrapping stores the session stage has
   built, newest first.

   Registered by the session stage when a middleware stack is built, which is
   the only moment the wrapping store exists and the only place that knows
   which store was configured.  A list rather than a single store because a
   process may build more than one stack (a test suite does this constantly,
   and an application may mount several).")

(defvar *session-sweep-stores-lock*
  (bt:make-lock "clails session sweep stores")
  "Guards *SESSION-SWEEP-STORES*.")


;;; ------------------------------------------------------------------
;;; Which stores to sweep
;;; ------------------------------------------------------------------

(defun register-session-sweep-store (store)
  "Remember STORE as one of the stores the periodic sweeper should walk.

   Called by the session stage when the middleware stack is built.  Only a
   wrapping store can be swept -- it is the one that knows which session ids
   exist -- so anything else is ignored rather than refused: a store the
   sweeper cannot walk is a store whose sessions are cleaned up by the lazy
   deletion alone, which is a weaker guarantee but not an error.

   Registering the same store twice does nothing.

   @param store [t] A session store, normally a <session-store-wrapper>
   @return [t] STORE when it was registered, NIL otherwise
   "
  (when (typep store '<session-store-wrapper>)
    (bt:with-lock-held (*session-sweep-stores-lock*)
      (unless (member store *session-sweep-stores*)
        (push store *session-sweep-stores*)))
    store))

(defun session-sweep-stores ()
  "The stores the periodic sweeper currently walks.

   @return [list] Registered wrapping stores
   "
  (bt:with-lock-held (*session-sweep-stores-lock*)
    (copy-list *session-sweep-stores*)))

(defun forget-session-sweep-stores ()
  "Forget every registered store.

   For a test that builds many stacks and does not want the sweeper to hold
   on to the stores (and the sessions) of the ones it has finished with.

   @return [null] NIL
   "
  (bt:with-lock-held (*session-sweep-stores-lock*)
    (setf *session-sweep-stores* '()))
  nil)


;;; ------------------------------------------------------------------
;;; One pass
;;; ------------------------------------------------------------------

(defun sweep-idle-sessions (store &key (idle-timeout *session-idle-timeout*)
                                       (now (session-clock-now)))
  "Remove from STORE every session that has been idle past IDLE-TIMEOUT.

   One pass.  The ids come from STORE's own index; the last-access time of
   each comes from the store, with the index' record as a fallback for a
   store that keeps no time of its own.  A session whose age cannot be
   established is left alone -- a sweeper must never remove a session it is
   not sure about.

   An IDLE-TIMEOUT of NIL means expiry is switched off and nothing is
   removed.

   @param store [<session-store-wrapper>] The wrapping store to sweep
   @param idle-timeout [(or integer null)] Limit in seconds, or NIL for none
   @param now [integer] Universal time to measure against
   @return [list] The session ids that were removed
   "
  (if (null idle-timeout)
      '()
      (let ((removed '()))
        (dolist (entry (session-store-wrapper-index-entries store))
          (let* ((sid (car entry))
                 ;; The store is the authority on when the session was last
                 ;; used (decision 11); the index' own record stands in only
                 ;; for a store that keeps none.
                 (last-access (or (session-last-access-time store sid)
                                  (cdr entry))))
            (when (session-idle-expired-p last-access idle-timeout now)
              (remove-session store sid)
              (push sid removed))))
        (when (and removed (log-level-enabled-p :debug))
          (log-package.debug "Session sweeper: removed idle sessions"
                             :count (length removed)
                             :remaining (session-store-wrapper-index-count store)))
        (nreverse removed))))

(defun sweep-all-registered-stores (&key (idle-timeout *session-idle-timeout*)
                                         (now (session-clock-now)))
  "Sweep every registered store once.

   The task the periodic loop runs.

   @param idle-timeout [(or integer null)] Limit in seconds, or NIL for none
   @param now [integer] Universal time to measure against
   @return [integer] How many sessions were removed in total
   "
  (let ((total 0))
    (dolist (store (session-sweep-stores))
      (incf total (length (sweep-idle-sessions store
                                               :idle-timeout idle-timeout
                                               :now now))))
    total))


;;; ------------------------------------------------------------------
;;; The thread
;;; ------------------------------------------------------------------

(defun run-session-sweeper-loop (&key max-iterations
                                      (interval *session-sweep-interval*))
  "Sweep every registered store, every INTERVAL seconds, until stopped.

   Runs on the sweeper thread; a test can also run it directly with a small
   MAX-ITERATIONS to keep the run bounded.  Errors from one pass are logged
   and the loop continues -- a background thread that is meant to live as
   long as the process must not die of a transient store failure.

   @param max-iterations [(or integer null)] Stop after this many passes; NIL
     runs until STOP-SESSION-SWEEPER is called
   @param interval [number] Seconds between passes
   @return [integer] How many passes ran
   "
  (run-periodic-loop
   :task (lambda () (sweep-all-registered-stores))
   :interval interval
   :waiter *session-sweeper-waiter*
   :stop-requested-p (lambda () *session-sweeper-stop-requested*)
   :max-iterations max-iterations
   :on-error (lambda (condition)
               (log-package.info "Session sweeper: a pass failed"
                                 :error (format nil "~A" condition)))))

(defun session-sweeper-running-p ()
  "@return [boolean] T when a sweeper thread started by START-SESSION-SWEEPER
     is alive"
  (and *session-sweeper-thread*
       (bt:thread-alive-p *session-sweeper-thread*)
       t))

(defun start-session-sweeper ()
  "Start the periodic session sweeper on its own background thread.

   Registered as a framework startup hook (see the bottom of this file), so a
   server started with `clails server' sweeps sessions without the
   application configuring anything.

   Does nothing when sweeping is switched off (*SESSION-SWEEP-ENABLED*), when
   a sweeper is already running, or when no store has been registered -- the
   last of which is how a process without a session stage (a task, the
   console) ends up with no sweeper thread.

   @return [(or t null)] The thread that was started, or NIL
   "
  (cond ((not *session-sweep-enabled*) nil)
        ((session-sweeper-running-p) *session-sweeper-thread*)
        ((null (session-sweep-stores)) nil)
        (t
         (setf *session-sweeper-stop-requested* nil)
         (reset-periodic-waiter *session-sweeper-waiter*)
         (setf *session-sweeper-thread*
               (bt:make-thread (lambda () (run-session-sweeper-loop))
                               :name "clails-session-sweeper"))
         (log-package.info "Session sweeper started"
                           :interval *session-sweep-interval*
                           :idle-timeout *session-idle-timeout*)
         *session-sweeper-thread*)))

(defun stop-session-sweeper ()
  "Ask the sweeper thread to stop and wait for it to exit.

   Registered as a framework shutdown hook.  Returns as soon as the thread
   has gone, which is a matter of milliseconds and not of the sweep interval:
   the flag is raised AND the loop's waiter is signalled, so a thread waiting
   out a five-minute interval wakes at once.  (That is the whole reason the
   loop does not simply SLEEP -- see clails/periodic.)

   @return [null] NIL
   "
  (setf *session-sweeper-stop-requested* t)
  (signal-periodic-waiter *session-sweeper-waiter*)
  (when *session-sweeper-thread*
    (ignore-errors (bt:join-thread *session-sweeper-thread*))
    (setf *session-sweeper-thread* nil)
    (log-package.info "Session sweeper stopped"))
  nil)


;;; ------------------------------------------------------------------
;;; Registration with the framework's hooks
;;; ------------------------------------------------------------------
;;;
;;; ADD-STARTUP-HOOK / ADD-SHUTDOWN-HOOK, never PUSH: the hook lists already
;;; contain the framework's own entries (the connection pool is started and
;;; stopped by exactly this mechanism) and PUSH would put the sweeper BEFORE
;;; them, i.e. before the pool the database session store needs.  The
;;; registration functions append, so the sweeper starts after everything the
;;; framework set up and stops before any of it is torn down.
;;;
;;; The hooks are named as strings rather than function objects so that
;;; re-loading this file does not leave a stale closure in the list, and the
;;; registration is guarded so that re-loading does not register them twice
;;; (which would start the sweeper, then find it already running, then try to
;;; stop it twice -- harmless, but noise in the startup log).

(defparameter +startup-hook-name+ "clails/middleware/session/sweeper:start-session-sweeper")
(defparameter +shutdown-hook-name+ "clails/middleware/session/sweeper:stop-session-sweeper")

(unless (member +startup-hook-name+ clails/environment:*startup-hooks* :test #'equal)
  (add-startup-hook +startup-hook-name+))
(unless (member +shutdown-hook-name+ clails/environment:*shutdown-hooks* :test #'equal)
  (add-shutdown-hook +shutdown-hook-name+))
