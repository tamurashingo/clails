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
           #:+session-store-env-key+))
(in-package #:clails/middleware/session/store/wrapper)


;;; ------------------------------------------------------------------
;;; Serialising one session (M4)
;;; ------------------------------------------------------------------
;;;
;;; This file is the wrapping store the design note in
;;; clails/middleware/session/store describes: a <session-store> derivation
;;; that holds one inner store, forwards the four operations to it, and is the
;;; place where cross-request concerns live.  M4 puts the serialisation of one
;;; session here; M5-T3 will put the sweeper's id index in the same object
;;; (see "Room for the sweep index" below).
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
;;; Room for the sweep index (M5-T3)
;;; --------------------------------
;;; The protocol has no "enumerate every session" operation, so a periodic
;;; sweeper needs a layer that watches the operations go past.  Rather than
;;; have M5-T3 rewrite the methods below, the three mutating operations call
;;; NOTE-SESSION-STORED / NOTE-SESSION-REMOVED / NOTE-SESSION-TOUCHED, which
;;; do nothing by default.  The sweep index is then a subclass with a slot and
;;; three methods -- additive, with this file untouched.

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
                    M5-T3, which adds the sweep index and so becomes the owner
                    of session-id lifetime in this object, is where pruning
                    belongs.")
   (locks-lock
    :initform (bt:make-lock "clails session store wrapper locks")
    :reader session-store-wrapper-locks-lock
    :documentation "Guards LOCKS."))
  (:documentation "A session store that forwards to another store and
   serialises access to one session.

   Created by the session stage around whatever *session-store* holds (see
   clails/middleware/session-middleware), so applications do not have to know
   it exists.  Two things live here:

     * one lock per session id, taken around each store operation and exposed
       to applications as WITH-SESSION-LOCK;
     * the hooks a later milestone needs to keep an index of live session ids
       (NOTE-SESSION-STORED and friends).

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
;;; Hooks for the sweep index (M5-T3)
;;; ------------------------------------------------------------------

(defgeneric note-session-stored (store sid session)
  (:documentation "Called inside STORE-SESSION, under the session's lock,
   after the inner store accepted the value.

   An extension point and nothing else: the default does nothing.  M5-T3 adds
   the sweeper's index of live session ids by subclassing the wrapper and
   adding methods here, without changing the operations themselves.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param session [hash-table] The value that was stored
   @return [t] Ignored
   "))

(defmethod note-session-stored ((store <session-store-wrapper>) sid session)
  (declare (ignore sid session))
  nil)

(defgeneric note-session-removed (store sid)
  (:documentation "Called inside REMOVE-SESSION, under the session's lock,
   after the inner store dropped the id.  Does nothing by default; see
   NOTE-SESSION-STORED.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [t] Ignored
   "))

(defmethod note-session-removed ((store <session-store-wrapper>) sid)
  (declare (ignore sid))
  nil)

(defgeneric note-session-touched (store sid access-time)
  (:documentation "Called inside TOUCH-SESSION, under the session's lock, with
   the time the inner store recorded (NIL when the id was unknown).  Does
   nothing by default; see NOTE-SESSION-STORED.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param access-time [(or integer null)] The recorded time, or NIL
   @return [t] Ignored
   "))

(defmethod note-session-touched ((store <session-store-wrapper>) sid access-time)
  (declare (ignore sid access-time))
  nil)


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
  "Forward the fourth operation to the inner store, holding the session's lock.

   A lack store structure has no fourth operation, so for such an inner store
   the base class' default (fetch, change, store) is used instead.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "
  (with-session-id-lock (store sid)
    (let* ((inner (session-store-wrapper-inner store))
           (time (if (%clails-store-p inner)
                     (touch-session inner sid access-time)
                     (call-next-method))))
      (note-session-touched store sid time)
      time)))

(defmethod session-last-access-time ((store <session-store-wrapper>) sid)
  "Read the last-access time from the inner store, or -- for a lack store
   structure, which keeps none -- from the base class' own bookkeeping.

   @param store [<session-store-wrapper>] Wrapping store
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL
   "
  (with-session-id-lock (store sid)
    (let ((inner (session-store-wrapper-inner store)))
      (if (%clails-store-p inner)
          (session-last-access-time inner sid)
          (call-next-method)))))


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
