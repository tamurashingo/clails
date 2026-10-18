(in-package #:cl-user)
(defpackage #:clails/middleware/session/store/wrapper
  (:use #:cl)
  ;; NOTE: lack-middleware-session is one ordinary (non-package-inferred) ASDF
  ;; system that defines several packages (lack/middleware/session,
  ;; lack/middleware/session/store, ...).  Only its top-level package is
  ;; aliased as a system, so this is the only package name that may appear in
  ;; a clause here; the three generic functions of lack's own store protocol
  ;; are written as fully-qualified symbols below.  The same trap is explained
  ;; at length in clails/middleware/session/store.
  (:import-from #:lack/middleware/session)
  (:import-from #:clails/middleware/session/clock
                #:session-clock-now)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:export #:<session-store-wrapper>
           #:make-session-store-wrapper
           #:ensure-session-store-wrapper
           #:session-store-wrapper-inner
           #:session-lock
           #:call-with-session-lock
           #:with-session-lock
           #:session-lock-env
           #:note-session-stored
           #:note-session-removed
           #:note-session-touched
           #:wrap-session-container
           #:session-table-synchronized-p
           #:+synchronized-session-tables-p+
           #:+session-store-env-key+
           ;; The sweep index (M5-T3)
           #:session-store-wrapper-index-entries
           #:session-store-wrapper-indexed-session-ids
           #:session-store-wrapper-index-time
           #:session-store-wrapper-index-count
           #:session-store-tracks-access-time-p))
(in-package #:clails/middleware/session/store/wrapper)


;;; ------------------------------------------------------------------
;;; Serialising one session (M4)
;;; ------------------------------------------------------------------
;;;
;;; This file is the wrapping store the design note in
;;; clails/middleware/session/store describes: a <session-store> derivation
;;; that holds one inner store, forwards the four operations to it, and is the
;;; place where cross-request concerns live.  M4 put the serialisation of one
;;; session here; M5-T3 added the sweeper's id index to the same object (see
;;; "The sweep index" below).
;;;
;;; What actually breaks without this layer
;;; ---------------------------------------
;;; The default application server runs one thread per request, and a browser
;;; fans one page load out into several concurrent requests that all carry the
;;; SAME session cookie.  Every one of those requests gets the same session
;;; hash-table -- by reference, not as a copy -- and the application code then
;;; writes into it with (SETF (GETHASH ...)).  Two threads doing that at the
;;; same instant do not merely lose one of the two values: they corrupt the
;;; table.  Measured on SBCL, 98 of 100 concurrent requests signalled "Unsafe
;;; concurrent operations on #<HASH-TABLE ...> detected" and the table was
;;; left with more entries than it could iterate over.
;;;
;;; The important part is WHERE that happens: not inside FETCH-SESSION or
;;; STORE-SESSION, but in between them, while the application is running.
;;; Taking a lock around the store operations therefore fixes nothing by
;;; itself -- the window stays exactly as wide as it was.  What has to change
;;; is the CONTAINER: the table the application writes into must be one that
;;; tolerates concurrent access.
;;;
;;; Two mechanisms, and why the split
;;; ---------------------------------
;;;   (1) WRAP-SESSION-CONTAINER (below) replaces the session container with a
;;;       synchronized hash-table before the application ever sees it.  It is
;;;       a stage INSIDE lack's session stage, because the container for a new
;;;       session is created by lack itself -- (make-hash-table :test 'equal)
;;;       in lack/middleware/session -- and a store cannot intervene there:
;;;       for a request that carries no cookie yet, lack never calls
;;;       FETCH-SESSION at all.  Doing the replacement at the store layer
;;;       instead (converting the table handed to STORE-SESSION) was measured
;;;       and does stop the corruption, but the stored table is then a
;;;       different object from the one the application wrote into, which
;;;       breaks the protocol's promise that FETCH-SESSION returns the session
;;;       itself and not a copy.  Replacing it before the application runs
;;;       keeps one object: env, store and application all share it.
;;;
;;;   (2) <SESSION-STORE-WRAPPER> owns one lock PER SESSION ID and takes it
;;;       around each store operation (decision 10: momentary exclusion -- the
;;;       lock is held for the read or the write, never for a whole request,
;;;       so two concurrent requests on one session still overlap).  The same
;;;       lock is what WITH-SESSION-LOCK exposes to applications.
;;;
;;; What is guaranteed, and what is not
;;; -----------------------------------
;;; A synchronized container guarantees that the session SURVIVES concurrent
;;; access: no corruption, no signalled condition, and the session stays
;;; readable and writable afterwards.  It does NOT make a read-modify-write
;;; atomic: two requests that both read "counter", add one and write it back
;;; can still produce one increment instead of two.  That is a deliberate line
;;; (the same one Rails and Django draw): a LOST UPDATE is specified behaviour
;;; here, and WITH-SESSION-LOCK is how an application that cannot live with it
;;; asks for atomicity.
;;;
;;; The sweep index (M5-T3)
;;; -----------------------
;;; The protocol has no "enumerate every session" operation, so a periodic
;;; sweeper needs a layer that watches the operations go past.  M4 left three
;;; hooks for it -- NOTE-SESSION-STORED / NOTE-SESSION-REMOVED /
;;; NOTE-SESSION-TOUCHED, called by the three mutating operations -- and M5-T3
;;; fills them in: this object now keeps an index from session id to
;;; last-access time, which is what clails/middleware/session/sweeper walks.
;;; The hooks remain the extension point they were, so a subclass can still
;;; add bookkeeping of its own on top.
;;;
;;; The index is a CANDIDATE LIST, not a second truth.  The sweeper reads the
;;; authoritative time with SESSION-LAST-ACCESS-TIME (decision 11: the time
;;; is store-side state) and falls back to the index only for a store that
;;; keeps no time of its own.  What the index really provides is the one thing
;;; no store can: the set of ids to ask about.
;;;
;;; Scope of the index: it holds the sessions THIS PROCESS has seen.  With the
;;; in-memory store that is every session there is.  With a persistent store
;;; shared by several processes, each process sweeps the sessions it served,
;;; and a session whose process has since stopped is removed the next time
;;; someone presents its id (the lazy deletion in the expiry stage).  A
;;; store-side sweep -- one DELETE over the whole table -- would cover those
;;; too and is a natural later addition to the database derivation; nothing
;;; here prevents it.
;;;
;;; Pruning: an id leaves the index as soon as its session is removed, so the
;;; index does not grow without bound.  The LOCKS table below is deliberately
;;; NOT pruned with it; the reason is in its own documentation.

(defparameter +session-store-env-key+ :clails.session.store
  "Request-environment key under which the session stage publishes the store
   it is using.

   WITH-SESSION-LOCK needs two things: the store that owns the per-session
   locks, and the session id.  The id is already in the environment -- lack
   puts it in (GETF (GETF env :lack.session.options) :id) -- and this key
   carries the store, so an application can ask for the lock with nothing but
   the request environment (or a controller, which knows its environment).")


;;; ------------------------------------------------------------------
;;; The container: a hash-table that tolerates concurrent access
;;; ------------------------------------------------------------------
;;;
;;; PORTABILITY: a hash-table that guards itself is NOT standard Common Lisp.
;;; SBCL provides it as the :SYNCHRONIZED argument of MAKE-HASH-TABLE plus the
;;; SB-EXT:HASH-TABLE-SYNCHRONIZED-P predicate, and SBCL is the implementation
;;; clails is built and tested on (see Dockerfile).  Other implementations
;;; have their own spellings of the same idea (CCL's :SHARED, and others), but
;;; none of them is exercised here, so nothing is claimed about them: on an
;;; implementation this file does not know, +SYNCHRONIZED-SESSION-TABLES-P+ is
;;; NIL, the container is left exactly as the layer below created it, and the
;;; session stage behaves as it did before this file existed.  Adding support
;;; for another implementation means adding a branch to the two functions
;;; below and nothing else.

(defparameter +synchronized-session-tables-p+
  #+sbcl t
  #-sbcl nil
  "True when this implementation can create a hash-table that serialises its
   own accesses, i.e. when the session container can be made safe for
   concurrent requests.  See the portability note above.")

(defun session-table-synchronized-p (table)
  "True when TABLE guards itself against concurrent access.

   Always NIL on an implementation without synchronized hash-tables, which is
   what keeps WRAP-SESSION-CONTAINER from replacing a container it cannot
   improve.

   @param table [hash-table] A session container
   @return [boolean] T when TABLE serialises its own accesses
   "
  #+sbcl (sb-ext:hash-table-synchronized-p table)
  #-sbcl (progn table nil))

(defun make-synchronized-session-table (&key (test 'equal) (size 16))
  "Create a session container that serialises its own accesses.

   Falls back to an ordinary hash-table where the implementation offers
   nothing better (see the portability note above).

   @param test [symbol] Hash-table test, defaulting to EQUAL as lack's own
     session container does
   @param size [integer] Initial size
   @return [hash-table] A fresh container
   "
  #+sbcl (make-hash-table :test test :size size :synchronized t)
  #-sbcl (make-hash-table :test test :size size))

(defun synchronized-session-table (table)
  "Return a synchronized container holding the same entries as TABLE.

   TABLE itself is returned when it already guards itself, or when this
   implementation has nothing to upgrade it to -- so calling this on every
   request costs one predicate once the session has been through it.

   @param table [hash-table] A session container
   @return [hash-table] TABLE, or a synchronized copy of it
   "
  (if (or (not +synchronized-session-tables-p+)
          (session-table-synchronized-p table))
      table
      (let ((new (make-synchronized-session-table
                  :test (hash-table-test table)
                  :size (max 16 (hash-table-count table)))))
        (maphash (lambda (key value) (setf (gethash key new) value)) table)
        new)))


;;; ------------------------------------------------------------------
;;; The stage that installs the container
;;; ------------------------------------------------------------------

(defun wrap-session-container (app store)
  "Wrap APP so that it runs with a session container safe for concurrent
   requests, and with STORE published in the request environment.

   This is a stage that belongs INSIDE lack's session stage: lack creates the
   container for a new session itself and then calls the application, so this
   is the first and only moment at which the container can be exchanged
   before any application code touches it.  Two cases:

     * the container lack just created for a NEW session is an ordinary
       hash-table; it is replaced here.  Nothing is lost by the replacement:
       the request that opens a session carries no cookie yet, so no other
       request can be sharing the container at this point;
     * the container FETCH-SESSION returned for a continuing session has
       already been through here on the request that created it (a store that
       hands back the object it was given), so it is already synchronized and
       is left alone -- which is what makes every concurrent request share
       ONE container rather than each getting a private copy.

   A store that rebuilds the session from a serialised form on every fetch (a
   database store will) hands back a fresh ordinary table per request, which
   is replaced here as well.  Such requests do not share a container at all,
   so they cannot corrupt one; what they can do is lose a whole write-back
   against each other, which is the documented last-writer-wins behaviour of
   a shared persistent store.

   @param app [function] The application to wrap
   @param store [t] The session store the stage was configured with
   @return [function] An application
   "
  (lambda (env)
    (let ((session (getf env :lack.session)))
      (when (hash-table-p session)
        ;; (SETF (GETF env :lack.session)) on a key that is already present
        ;; modifies the plist in place, so lack -- which reads the session
        ;; back out of the same environment when it commits the request --
        ;; stores the container the application actually wrote into.
        (setf (getf env :lack.session) (synchronized-session-table session))))
    (setf (getf env +session-store-env-key+) store)
    (funcall app env)))


;;; ------------------------------------------------------------------
;;; The wrapping store
;;; ------------------------------------------------------------------

(defclass <session-store-wrapper> (<session-store>)
  ((inner
    :initarg :inner
    :reader session-store-wrapper-inner
    :documentation "The store the four operations are forwarded to.  Either a
                    <session-store> derivation or a lack session store
                    structure; both are supported, because *session-store*
                    accepts both.")
   (locks
    :initform (make-hash-table :test 'equal)
    :reader session-store-wrapper-locks
    :documentation "Session id -> recursive lock.

                    The locks are RECURSIVE on purpose: WITH-SESSION-LOCK
                    nests (an action that takes the lock may call a function
                    that takes it again), and a store operation performed
                    inside a WITH-SESSION-LOCK body takes the very same lock.
                    Both are ordinary things to write, and neither may
                    deadlock.

                    Entries are never dropped, not even by REMOVE-SESSION: a
                    thread that is waiting for the lock of a session another
                    thread is removing must end up with the SAME lock object,
                    which cannot be guaranteed once the entry can disappear
                    while someone is using it.  The table therefore grows with
                    the number of distinct session ids this process has seen.

                    M5-T3 (which added the sweep index below, and with it the
                    knowledge of which ids are dead) looked at pruning this
                    table along with the index and decided against it: a swept
                    id can still have a request in flight, and giving that
                    request a DIFFERENT lock object from the one the sweeper
                    holds removes the very exclusion this table exists for.
                    The index is pruned instead -- it is the one that would
                    otherwise grow per session rather than per id -- and
                    bounding this table needs weak references, not the
                    sweeper.")
   (locks-lock
    :initform (bt:make-lock "clails session store wrapper locks")
    :reader session-store-wrapper-locks-lock
    :documentation "Guards LOCKS.")
   (index
    :initform (make-hash-table :test 'equal)
    :reader session-store-wrapper-index
    :documentation "Session id -> last-access time (a universal time), for
                    every session this process has put into the inner store
                    and not removed since.

                    The sweep index (M5-T3).  Neither the clails store
                    protocol nor lack's has an \"enumerate every session\"
                    operation, so this is the only place the periodic sweeper
                    can learn WHICH ids to consider; the time it compares
                    against comes from the store itself whenever the store
                    keeps one (see SESSION-LAST-ACCESS-TIME).  Maintained by
                    the three NOTE-SESSION-* hooks, under the session's own
                    lock.")
   (index-lock
    :initform (bt:make-lock "clails session store wrapper index")
    :reader session-store-wrapper-index-lock
    :documentation "Guards INDEX."))
  (:documentation "A session store that forwards to another store and
   serialises access to one session.

   Created by the session stage around whatever *session-store* holds (see
   clails/middleware/session-middleware), so applications do not have to know
   it exists.  Two things live here:

     * one lock per session id, taken around each store operation and exposed
       to applications as WITH-SESSION-LOCK;
     * the index of live session ids the periodic sweeper walks, kept by the
       three NOTE-SESSION-* hooks (M5-T3).

   Protecting the session CONTAINER is not done here but in
   WRAP-SESSION-CONTAINER; the file header explains why it cannot be."))


(defun make-session-store-wrapper (inner)
  "Wrap INNER in a store that serialises access per session id.

   @param inner [t] A <session-store> derivation or a lack session store
   @return [<session-store-wrapper>] The wrapping store
   "
  (make-instance '<session-store-wrapper> :inner inner))

(defun ensure-session-store-wrapper (store)
  "Return STORE wrapped in a <session-store-wrapper>, or STORE itself when it
   already is one.

   Idempotent so that the session stage can call it unconditionally and an
   application that built its own wrapper (or nests one) does not end up with
   two layers of locks.

   @param store [t] A session store of any kind
   @return [<session-store-wrapper>] A wrapping store
   "
  (if (typep store '<session-store-wrapper>)
      store
      (make-session-store-wrapper store)))

(defun session-lock (store sid)
  "Return the lock that serialises access to the session SID in STORE,
   creating it on first use.

   The same SID always yields the same (recursive) lock object, and different
   session ids yield different ones -- concurrent requests on DIFFERENT
   sessions never wait for each other.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [t] A recursive lock
   "
  (bt:with-lock-held ((session-store-wrapper-locks-lock store))
    (or (gethash sid (session-store-wrapper-locks store))
        (setf (gethash sid (session-store-wrapper-locks store))
              (bt:make-recursive-lock (format nil "clails session ~A" sid))))))

(defmacro with-session-id-lock ((store sid) &body body)
  "Run BODY holding the lock of the session SID in STORE."
  `(bt:with-recursive-lock-held ((session-lock ,store ,sid))
     ,@body))


;;; ------------------------------------------------------------------
;;; The sweep index, kept by three hooks (M5-T3)
;;; ------------------------------------------------------------------
;;;
;;; The three operations that change something call one of these, under the
;;; session's own lock, so that the index can follow the set of live sessions
;;; without the operations themselves knowing what it is for.  They are still
;;; extension points: a subclass that needs bookkeeping of its own adds
;;; methods and calls CALL-NEXT-METHOD to keep the index working.

(defun %index-session (store sid time)
  (bt:with-lock-held ((session-store-wrapper-index-lock store))
    (setf (gethash sid (session-store-wrapper-index store)) time)))

(defun %unindex-session (store sid)
  (bt:with-lock-held ((session-store-wrapper-index-lock store))
    (remhash sid (session-store-wrapper-index store))))

(defun session-store-wrapper-index-entries (store)
  "Return a snapshot of the sweep index as a list of (SID . LAST-ACCESS-TIME).

   A snapshot, not the table: the sweeper walks it while other threads keep
   serving requests, and removing a session while MAPHASH runs over the index
   is not defined behaviour.

   @param store [<session-store-wrapper>] Wrapping store
   @return [list] (SID . universal-time) pairs, in no particular order
   "
  (bt:with-lock-held ((session-store-wrapper-index-lock store))
    (loop for sid being the hash-keys of (session-store-wrapper-index store)
            using (hash-value time)
          collect (cons sid time))))

(defun session-store-wrapper-indexed-session-ids (store)
  "Return the session ids currently in STORE's sweep index.

   @param store [<session-store-wrapper>] Wrapping store
   @return [list] Session ids
   "
  (mapcar #'car (session-store-wrapper-index-entries store)))

(defun session-store-wrapper-index-count (store)
  "How many sessions STORE's sweep index currently holds.

   @param store [<session-store-wrapper>] Wrapping store
   @return [integer] Number of indexed sessions
   "
  (bt:with-lock-held ((session-store-wrapper-index-lock store))
    (hash-table-count (session-store-wrapper-index store))))

(defun session-store-wrapper-index-time (store sid)
  "The last-access time STORE's sweep index holds for SID, or NIL.

   The index' own record, which is what the sweeper falls back to for an
   inner store that keeps no last-access time of its own.  The authoritative
   answer is SESSION-LAST-ACCESS-TIME.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL when SID is not indexed
   "
  (bt:with-lock-held ((session-store-wrapper-index-lock store))
    (gethash sid (session-store-wrapper-index store))))


(defgeneric note-session-stored (store sid session)
  (:documentation "Called inside STORE-SESSION, under the session's lock,
   after the inner store accepted the value.

   The wrapper's own method adds SID to the sweep index with the current time:
   storing a session is an access to it.  An extension point as well -- a
   subclass may add a method and call CALL-NEXT-METHOD.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param session [hash-table] The value that was stored
   @return [t] Ignored
   "))

(defmethod note-session-stored ((store <session-store-wrapper>) sid session)
  (declare (ignore session))
  (%index-session store sid (session-clock-now)))

(defgeneric note-session-removed (store sid)
  (:documentation "Called inside REMOVE-SESSION, under the session's lock,
   after the inner store dropped the id.

   The wrapper's own method drops SID from the sweep index, which is what
   keeps the index from growing for the lifetime of the process.  See
   NOTE-SESSION-STORED about extending it.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [t] Ignored
   "))

(defmethod note-session-removed ((store <session-store-wrapper>) sid)
  (%unindex-session store sid))

(defgeneric note-session-touched (store sid access-time)
  (:documentation "Called inside TOUCH-SESSION, under the session's lock, with
   the time that was recorded (NIL when the id was unknown).

   The wrapper's own method refreshes SID's entry in the sweep index, so that
   a session being used does not look idle to the sweeper.  Nothing is
   indexed for an unknown id.  See NOTE-SESSION-STORED about extending it.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param access-time [(or integer null)] The recorded time, or NIL
   @return [t] Ignored
   "))

(defmethod note-session-touched ((store <session-store-wrapper>) sid access-time)
  (when access-time
    (%index-session store sid access-time)))


;;; ------------------------------------------------------------------
;;; Forwarding the four operations
;;; ------------------------------------------------------------------
;;;
;;; The inner store may be either kind of store *session-store* accepts, so
;;; every forward picks the protocol that fits: clails' generic functions for
;;; a <session-store> derivation, lack's three for a lack store structure
;;; (which cannot inherit from a CLOS class and therefore has no methods on
;;; the clails ones).  lack has no fourth operation, so for a lack store the
;;; base class' default TOUCH-SESSION is used instead -- CALL-NEXT-METHOD,
;;; which reaches the naive fetch-change-store default through this very
;;; wrapper and so stays serialised.

(defun %clails-store-p (store)
  (typep store '<session-store>))


;;; Does the inner store implement the fourth operation ITSELF?
;;;
;;; This question has to be asked before a touch is forwarded, and the answer
;;; decides where the last-access time of a session lives.
;;;
;;; The base class carries a default TOUCH-SESSION so that a store which
;;; implements only the three operations still expires correctly, and that
;;; default is deliberately naive: fetch the session, then write the whole
;;; thing back.  Called once by hand that is fine.  Called by the session
;;; stage on EVERY request it is not: it would turn every request -- the
;;; static-asset requests a single page load fans out included -- into a
;;; whole-value write-back of the session, which is exactly the cost (and
;;; exactly the concurrency hazard) that giving the protocol a fourth
;;; operation exists to avoid.
;;;
;;; So the wrapper forwards the touch only to a store that can do it cheaply,
;;; i.e. one that has a TOUCH-SESSION method of its own (the in-memory
;;; derivation replaces one index entry, the database derivation issues one
;;; UPDATE of one column, a future Redis derivation extends a key's
;;; lifetime).  For any other store -- a minimal third-party derivation, or a
;;; lack store structure, which cannot inherit a CLOS default at all -- the
;;; wrapper's own index is where the time is recorded, and
;;; SESSION-LAST-ACCESS-TIME below reads it back from there.  Expiry works
;;; either way, which is what keeps the store abstraction substitutable (G2):
;;; no store is excluded from expiry, and none pays a write-back for it.
;;;
;;; COMPUTE-APPLICABLE-METHODS is standard Common Lisp and asks precisely the
;;; question above: it returns the methods that would run, most specific
;;; first, so "the most specific one is the base class' default" means "this
;;; store has none of its own".  The answer depends only on the store's class
;;; and is cached per class; a store class that gains a TOUCH-SESSION method
;;; after the cache was filled (interactive development) needs the image
;;; restarted, or the entry cleared.

(defvar *access-time-tracking-cache* (make-hash-table :test 'eq)
  "Class -> boolean, the cached answer of SESSION-STORE-TRACKS-ACCESS-TIME-P.")

(defvar *access-time-tracking-cache-lock*
  (bt:make-lock "clails session store access-time tracking cache")
  "Guards *ACCESS-TIME-TRACKING-CACHE*.")

(defun %compute-tracks-access-time-p (store)
  (and (%clails-store-p store)
       (let ((default (find-method #'touch-session nil
                                   (list (find-class '<session-store>)
                                         (find-class t))
                                   nil))
             (applicable (compute-applicable-methods #'touch-session
                                                     (list store "" nil))))
         (and applicable
              (not (eq (first applicable) default))
              t))))

(defun session-store-tracks-access-time-p (store)
  "True when STORE keeps a last-access time of its own, i.e. implements the
   fourth operation (TOUCH-SESSION) rather than inheriting the base class'
   write-back default.

   The wrapper uses it to decide whether to forward a touch to the store it
   wraps or to record the time in its own sweep index instead; see the
   commentary above.

   @param store [t] A session store of any kind
   @return [boolean] T when STORE can record an access without a write-back
   "
  (let ((class (class-of store)))
    (bt:with-lock-held (*access-time-tracking-cache-lock*)
      (multiple-value-bind (cached present-p)
          (gethash class *access-time-tracking-cache*)
        (if present-p
            cached
            (setf (gethash class *access-time-tracking-cache*)
                  (%compute-tracks-access-time-p store)))))))

(defun %inner-fetch (inner sid)
  (if (%clails-store-p inner)
      (fetch-session inner sid)
      (lack/middleware/session/store:fetch-session inner sid)))

(defun %inner-store (inner sid session)
  (if (%clails-store-p inner)
      (store-session inner sid session)
      (lack/middleware/session/store:store-session inner sid session)))

(defun %inner-remove (inner sid)
  (if (%clails-store-p inner)
      (remove-session inner sid)
      (lack/middleware/session/store:remove-session inner sid)))

(defun %touch-in-index (store sid access-time)
  "Record an access to SID in STORE's own index, for an inner store that keeps
   no last-access time of its own.  Returns the recorded time, or NIL when
   SID holds no session."
  (let ((known (or (session-store-wrapper-index-time store sid)
                   ;; Not in the index: either the id is unknown, or the
                   ;; session was put into the inner store behind this
                   ;; wrapper's back (a pre-populated store, a second process
                   ;; sharing a persistent one).  Asking the inner store is
                   ;; the only way to tell the two apart, and it is still
                   ;; cheaper than a write-back -- and it happens at most once
                   ;; per id, because the answer is indexed below.
                   (and (%inner-fetch (session-store-wrapper-inner store) sid)
                        t))))
    (when known
      (let ((time (or access-time (session-clock-now))))
        (%index-session store sid time)
        time))))

(defmethod fetch-session ((store <session-store-wrapper>) sid)
  "Forward to the inner store, holding the session's lock while it runs.

   The lock is released before the value is handed back: the application then
   works on the container without holding anything (decision 10).  It is the
   container itself that keeps concurrent writes safe.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [(or hash-table null)] The stored session, or NIL
   "
  (with-session-id-lock (store sid)
    (%inner-fetch (session-store-wrapper-inner store) sid)))

(defmethod store-session ((store <session-store-wrapper>) sid session)
  "Forward to the inner store, holding the session's lock while it runs.

   The value is passed through untouched -- the container was made safe before
   the application ran (WRAP-SESSION-CONTAINER), and replacing it here would
   mean the stored session is a different object from the one the application
   wrote into.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param session [hash-table] Session contents to store
   @return [t] Whatever the inner store returned
   "
  (with-session-id-lock (store sid)
    (let ((result (%inner-store (session-store-wrapper-inner store) sid session)))
      (note-session-stored store sid session)
      result)))

(defmethod remove-session ((store <session-store-wrapper>) sid)
  "Forward to the inner store, holding the session's lock while it runs.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [t] Whatever the inner store returned
   "
  (with-session-id-lock (store sid)
    (let ((result (%inner-remove (session-store-wrapper-inner store) sid)))
      (note-session-removed store sid)
      result)))

(defmethod touch-session ((store <session-store-wrapper>) sid &optional access-time)
  "Record an access to SID, holding the session's lock while it runs.

   Forwarded to the inner store when that store implements the fourth
   operation itself; otherwise the time is recorded in the sweep index only,
   so that a store without a cheap partial update is not made to write the
   whole session back on every request.  The commentary above
   SESSION-STORE-TRACKS-ACCESS-TIME-P explains the choice; the session value
   is untouched either way.

   An id that holds no session is not recorded, and answers NIL: the stage
   reads NIL as \"this id is not a session\".

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "
  (with-session-id-lock (store sid)
    (let* ((inner (session-store-wrapper-inner store))
           (time (if (session-store-tracks-access-time-p inner)
                     (touch-session inner sid access-time)
                     (%touch-in-index store sid access-time))))
      (note-session-touched store sid time)
      time)))

(defmethod session-last-access-time ((store <session-store-wrapper>) sid)
  "Read the last-access time of SID.

   From the inner store when it keeps one, otherwise from this wrapper's sweep
   index -- the same split TOUCH-SESSION makes, so that the time is read back
   from wherever it was written.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL
   "
  (with-session-id-lock (store sid)
    (let ((inner (session-store-wrapper-inner store)))
      (if (session-store-tracks-access-time-p inner)
          (session-last-access-time inner sid)
          (session-store-wrapper-index-time store sid)))))


;;; ------------------------------------------------------------------
;;; The public macro: an atomic read-modify-write on one session
;;; ------------------------------------------------------------------

(defgeneric call-with-session-lock (store sid function)
  (:documentation "Call FUNCTION with the session SID in STORE locked against
   other requests on the same session.

   The extension point behind WITH-SESSION-LOCK: a store that can serialise
   sessions in its own medium (a future Redis derivation with a distributed
   lock, say) implements this instead of being wrapped.

   @param store [t] Session store instance
   @param sid [string] Session id
   @param function [function] Called with no arguments while the lock is held
   @return [t] Whatever FUNCTION returned
   "))

(defmethod call-with-session-lock (store sid function)
  (declare (ignore sid function))
  (error "The session store ~S provides no per-session lock, so ~
          WITH-SESSION-LOCK cannot make this body atomic.  The session stage ~
          normally wraps the configured store in a ~
          CLAILS/MIDDLEWARE/SESSION/STORE/WRAPPER:<SESSION-STORE-WRAPPER>, ~
          which does; a store reached outside that stage has to be wrapped by ~
          hand with ENSURE-SESSION-STORE-WRAPPER."
         (type-of store)))

(defmethod call-with-session-lock ((store <session-store-wrapper>) sid function)
  (with-session-id-lock (store sid)
    (funcall function)))

(defgeneric session-lock-env (source)
  (:documentation "Return the request environment WITH-SESSION-LOCK should
   read the session store and session id out of.

   WITH-SESSION-LOCK accepts a request environment directly (a plist, which is
   what a middleware stage holds).  Anything else is passed through this
   generic function, which is how a controller can be given instead: the
   method lives in clails/controller/base-controller, so that the session
   layer does not have to know about controllers.

   @param source [t] Whatever was handed to WITH-SESSION-LOCK
   @return [list] A request environment plist
   "))

(defmethod session-lock-env (source)
  (error "~S is neither a request environment nor an object that knows one, ~
          so WITH-SESSION-LOCK cannot find the session to lock."
         source))

(defun %session-lock-env (source)
  (if (listp source)
      source
      (session-lock-env source)))

(defun %env-session-store (env)
  (or (getf env +session-store-env-key+)
      (error "No session store in the request environment.  WITH-SESSION-LOCK ~
              needs the clails session stage to have run, which it does only ~
              when CLAILS/MIDDLEWARE/SESSION-MIDDLEWARE:*ENABLE-SESSION-MIDDLEWARE* ~
              is true (typically set in app/config/environment.lisp).")))

(defun %env-session-id (env)
  (or (getf (getf env :lack.session.options) :id)
      (error "No session id in the request environment, so there is no ~
              session for WITH-SESSION-LOCK to lock.")))

(defun call-with-locked-session (source function)
  "Run FUNCTION with the session of the request SOURCE describes locked.

   The function WITH-SESSION-LOCK expands into; see that macro.

   @param source [t] A request environment plist, or a controller
   @param function [function] Called with no arguments while the lock is held
   @return [t] Whatever FUNCTION returned
   "
  (let ((env (%session-lock-env source)))
    (call-with-session-lock (%env-session-store env)
                            (%env-session-id env)
                            function)))

(defmacro with-session-lock ((source) &body body)
  "Run BODY with this request's session locked against other requests on the
   same session.

   The session stage keeps the session container safe to write concurrently,
   but that is all it can do on its own: two requests that both read a value,
   change it and write it back can still end up applying only one of the two
   changes.  Wrapping the read AND the write in this macro makes the sequence
   atomic:

     (with-session-lock (controller)
       (let ((count (or (gethash \"count\" (session controller)) 0)))
         (setf (gethash \"count\" (session controller)) (1+ count))))

   SOURCE is the request environment (what a middleware stage has) or an
   object that knows it -- a controller, which is what an action has.

   Nesting is safe: the lock is recursive, so a body that takes the lock again
   (directly, or through a function that does, or through a store operation,
   which takes the same lock) proceeds instead of deadlocking.

   Three things to know before using it:

     * only requests in THIS process are held back.  The lock is a lock in
       this image; with a database or Redis store shared by several processes,
       a concurrent write-back from another process still wins or loses the
       last-writer-wins way;
     * other requests on the SAME session wait for the body to finish -- the
       stage itself only ever holds the lock for the instant of a single store
       operation, so a slow body is the one thing that can make a user's own
       requests queue up.  Keep the body to the read, the change and the
       write, and do the slow work (a query, an HTTP call) outside it;
     * requests on OTHER sessions are not affected at all; the lock is per
       session id.

   @param source [t] A request environment plist, or a controller
   @return [t] The value of the last form in BODY
   "
  `(call-with-locked-session ,source (lambda () ,@body)))
