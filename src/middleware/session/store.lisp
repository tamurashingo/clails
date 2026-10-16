(in-package #:cl-user)
(defpackage #:clails/middleware/session/store
  (:use #:cl)
  ;; NOTE: lack-middleware-session is one ordinary (non-package-inferred) ASDF
  ;; system that defines several packages (lack/middleware/session,
  ;; lack/middleware/session/store, .../store/memory, .../state/cookie, ...).
  ;; Only its top-level package is aliased as a system (lack.asd declares the
  ;; system "lack/middleware/session"), so this is the only package name that
  ;; may appear in a clause here: an :import-from for one of the sub-packages
  ;; would make clails' package-inferred-system try (and fail) to find an ASDF
  ;; component named after it.  The three generic functions this file attaches
  ;; bridge methods to live in LACK/MIDDLEWARE/SESSION/STORE and are therefore
  ;; written as fully-qualified symbols below; loading them is guaranteed by
  ;; this (symbol-less) dependency on the system that contains them.
  (:import-from #:lack/middleware/session)
  (:export #:<session-store>
           #:fetch-session
           #:store-session
           #:remove-session
           #:touch-session
           #:session-last-access-time))
(in-package #:clails/middleware/session/store)


;;; ------------------------------------------------------------------
;;; The clails session store protocol
;;; ------------------------------------------------------------------
;;;
;;; A session store answers four questions about one session id:
;;;
;;;   fetch-session             what is stored under this id?
;;;   store-session             store this value under this id
;;;   remove-session            forget this id
;;;   touch-session             this id was used just now
;;;
;;; The first three mirror the abstraction of the underlying library (lack):
;;; lack/middleware/session calls exactly those three generic functions on
;;; whatever object was handed to it as :store.  The fourth is clails' own and
;;; is explained under "The fourth operation" below.
;;;
;;; Why a CLOS class and not a structure
;;; ------------------------------------
;;; lack's own abstraction is structure based -- (defstruct store) plus three
;;; generic functions -- and its derivations are written as
;;; (defstruct (memory-store (:include lack/session/store:store))).  clails
;;; defines a CLOS class hierarchy instead.  That was measured before it was
;;; decided, because the two styles do not mix in one direction:
;;;
;;;   * (defclass x (lack/session/store:store) ()) is IMPOSSIBLE: SBCL signals
;;;     SB-MOP:INVALID-SUPERCLASS, because STRUCTURE-CLASS and STANDARD-CLASS
;;;     are incompatible metaclasses.  A clails store therefore cannot be a
;;;     subtype of lack's store structure.
;;;   * It does not need to be.  lack's session middleware never checks the
;;;     type of its :store argument; it only calls the three generic functions
;;;     on it.  A plain CLOS instance that has methods on those generic
;;;     functions was driven through two real requests of the lack session
;;;     middleware (new session, then the same session id again) and the value
;;;     carried over correctly, with fetch / store observed in the expected
;;;     order.
;;;
;;; So <session-store> is an ordinary CLOS class with no lack superclass, and
;;; the bridge at the bottom of this file -- three methods on lack's generic
;;; functions, specialised on <session-store> -- is what makes every clails
;;; store acceptable to the lack session stage.
;;;
;;; Compatibility of *session-store*
;;; --------------------------------
;;; clails/middleware/session-middleware:*session-store* is documented as
;;; "a lack session store instance" and is passed straight through to lack.
;;; Nothing about that changes: lack dispatches generically, so a lack
;;; structure store (its memory store, its DBI store, its Redis store) that an
;;; application already puts there keeps working exactly as before.  What is
;;; new is that a <session-store> instance is now equally acceptable, and that
;;; the default when the variable is NIL is clails' own memory derivation.
;;; Only the fourth operation is unavailable on a bare lack store, because a
;;; lack structure store cannot inherit the default implementation below; an
;;; application that wants idle-timeout expiry has to use a store that derives
;;; from <session-store>.
;;;
;;; The fourth operation: "this id was used just now"
;;; -------------------------------------------------
;;; lack's abstraction has only fetch / store / remove and no notion of a
;;; PARTIAL update.  With those three alone, recording a last-access time can
;;; only be done as fetch -> change -> store, i.e. a WHOLE-VALUE WRITE-BACK of
;;; the session on every single request -- including requests that only read.
;;; That is expensive for a persistent store (the entire serialised session is
;;; rewritten) and, worse, it turns every request into a writer of the session
;;; container, which is exactly the concurrency hazard M4 exists to remove.
;;;
;;; TOUCH-SESSION is therefore a fourth generic function, so that each
;;; derivation can express the update in the cheapest way its medium allows:
;;;
;;;   memory store : replace the entry in its own index (no value write-back)
;;;   DB store     : one statement that updates the timestamp column only
;;;   Redis store  : extend the key's remaining lifetime
;;;
;;; The base class still carries the naive default (fetch -> change -> store),
;;; so a derivation that implements only the three operations still gets a
;;; working last-access time -- a new store is never silently excluded from
;;; expiry just because its author did not know about the fourth operation.
;;;
;;; The timestamp is NOT part of the session value.  It is store-side state:
;;; the value returned by FETCH-SESSION is the table the application reads and
;;; writes, and TOUCH-SESSION must leave it untouched.  Keeping the timestamp
;;; inside the value would mean every request -- including the concurrent
;;; static-asset requests a single page load fans out -- writes the session
;;; table, so an ordinary page view would pay the concurrency risk.  The base
;;; class keeps that bookkeeping in a slot of its own (see ACCESS-TIMES).
;;;
;;; Design note: the wrapping store (implemented by M4, extended by M5-T3)
;;; ---------------------------------------------------------------------
;;; Two later pieces of work both need to sit *between* the session stage and
;;; the configured store, and they will share one object: a store that derives
;;; from <session-store>, holds another store as its inner store, and forwards
;;; the four operations to it.  Writing it down here, because the shape has to
;;; be fixed before either piece is built:
;;;
;;;   (a) M4 -- serialising one session.  Concurrent requests carrying the same
;;;       session id currently read and write one container with nothing
;;;       between them.  The wrapper owns a per-session-id lock and takes it
;;;       around each store operation (momentary exclusion, decision 10: the
;;;       lock is held for the read or the write, never for a whole request).
;;;       The same per-id lock is what the public WITH-SESSION-LOCK macro
;;;       (M4-T2) exposes, so applications that need a read-modify-write to be
;;;       atomic can ask for it; without it, a lost update is specified
;;;       behaviour rather than a defect.
;;;
;;;   (b) M5-T3 -- the sweep index.  The protocol above deliberately has NO
;;;       "enumerate every session" operation, and neither does lack's.  A
;;;       periodic sweeper cannot be written against the four operations, so
;;;       the only place an index of live session ids can live is a wrapper
;;;       that watches the operations go past: store-session adds an id,
;;;       remove-session drops it, touch-session refreshes its timestamp.  The
;;;       sweeper then walks that index and calls remove-session for the ids
;;;       that are idle past the limit.
;;;
;;; Consequences that this milestone is responsible for keeping true:
;;;
;;;   * the four operations are generic functions on the store INSTANCE, so a
;;;     wrapper is substitutable for the store it wraps, and a wrapper can be
;;;     inserted (or removed) without any other code changing;
;;;   * nothing in the protocol enumerates sessions, so adding the index later
;;;     is additive -- it is a slot plus the bookkeeping inside the three
;;;     mutating methods of the wrapper, not a change to this protocol;
;;;   * last-access time is readable through SESSION-LAST-ACCESS-TIME, so the
;;;     sweeper can ask the store rather than keeping a second truth;
;;;   * TOUCH-SESSION takes the time as an optional argument, so the wrapper
;;;     and the sweeper can record a known instant instead of re-reading the
;;;     clock at each layer.
;;;
;;; The wrapper itself is NOT implemented here: it is
;;; clails/middleware/session/store/wrapper:<session-store-wrapper> (M4),
;;; which the session stage puts around whatever *session-store* holds.  One
;;; thing that design note got wrong is written down there: taking the
;;; per-session lock around the store operations is NOT by itself enough to
;;; stop concurrent requests from damaging the session, because the damage
;;; happens between the operations, while the application writes into the
;;; container -- so the container itself is replaced by one that tolerates
;;; concurrent access.  M3-T2's swap tests exercise a delegating store that
;;; keeps its own id index, to prove that inserting such a layer does not
;;; disturb the round trip.


(defclass <session-store> ()
  ((access-times
    :initform (make-hash-table :test 'equal)
    :reader session-store-access-times
    :documentation "Last-access time per session id, as universal times.

                    This is the bookkeeping behind the DEFAULT implementation
                    of TOUCH-SESSION / SESSION-LAST-ACCESS-TIME.  A derivation
                    that records the time in its own medium (the memory
                    derivation's index, a timestamp column, a key lifetime)
                    overrides both methods and leaves this table empty.")
   (access-times-lock
    :initform (bt:make-lock "clails session store access times")
    :reader session-store-access-times-lock
    :documentation "Guards ACCESS-TIMES."))
  (:documentation "Base class for clails session stores.

   Derive from this class and implement FETCH-SESSION, STORE-SESSION and
   REMOVE-SESSION to plug a new backing store (a database, Redis, ...) into
   the session stage; TOUCH-SESSION is optional and has a working default.
   An instance is what clails/middleware/session-middleware:*session-store*
   holds, and what the lack session stage receives as its :store."))


;;; ------------------------------------------------------------------
;;; Access-time bookkeeping used by the base-class defaults
;;; ------------------------------------------------------------------

(defun %record-access-time (store sid time)
  (bt:with-lock-held ((session-store-access-times-lock store))
    (setf (gethash sid (session-store-access-times store)) time)))

(defun %access-time (store sid)
  (bt:with-lock-held ((session-store-access-times-lock store))
    (gethash sid (session-store-access-times store))))

(defun %forget-access-time (store sid)
  (bt:with-lock-held ((session-store-access-times-lock store))
    (remhash sid (session-store-access-times store))))


;;; ------------------------------------------------------------------
;;; The three operations lack also knows about
;;; ------------------------------------------------------------------

(defgeneric fetch-session (store sid)
  (:documentation "Return the session stored under SID, or NIL when there is none.

   The returned object is the session itself -- the hash-table the application
   reads and writes -- not a copy, for stores that can hand it back directly.

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @return [(or hash-table null)] The stored session, or NIL
   "))

(defmethod fetch-session ((store <session-store>) sid)
  (declare (ignore sid))
  (error "fetch-session is not implemented for ~S." (type-of store)))

(defgeneric store-session (store sid session)
  (:documentation "Store SESSION under SID, replacing whatever was there.

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @param session [hash-table] Session contents to store
   @return [t] Implementation defined
   "))

(defmethod store-session ((store <session-store>) sid session)
  (declare (ignore sid session))
  (error "store-session is not implemented for ~S." (type-of store)))

(defgeneric remove-session (store sid)
  (:documentation "Forget the session stored under SID.

   Removing an id that is not stored is not an error.

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @return [t] Implementation defined
   "))

(defmethod remove-session ((store <session-store>) sid)
  (declare (ignore sid))
  (error "remove-session is not implemented for ~S." (type-of store)))

(defmethod remove-session :after ((store <session-store>) sid)
  "Drop the base-class last-access bookkeeping for a removed session.

   Harmless for derivations that keep the timestamp elsewhere; without it, the
   ACCESS-TIMES table of a store using the default implementation would grow
   for the lifetime of the process."
  (%forget-access-time store sid))


;;; ------------------------------------------------------------------
;;; The fourth operation: update only the last-access time
;;; ------------------------------------------------------------------

(defgeneric touch-session (store sid &optional access-time)
  (:documentation "Record that the session under SID was used at ACCESS-TIME.

   This must NOT change the session value: the table the application reads and
   writes is left exactly as it is, contents and identity.  The time is
   store-side state, read back with SESSION-LAST-ACCESS-TIME.

   ACCESS-TIME defaults to now; callers that already know the instant (the
   session stage, the periodic sweeper, a test) pass it explicitly so that
   every layer records the same value.

   Touching an id that holds no session does nothing.

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "))

(defmethod touch-session ((store <session-store>) sid &optional access-time)
  "Default implementation: fetch, change, store.

   Deliberately naive, because it has to work with nothing but the three
   operations every derivation implements.  The cost is a whole-value
   write-back of the session on every touch -- which is precisely why
   TOUCH-SESSION exists as a generic function, so that a derivation can
   replace this with an update of its own index, column or key lifetime.  The
   point of the default is that a derivation which implements only the three
   operations still expires correctly rather than never expiring at all.

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "
  (let ((session (fetch-session store sid)))
    (when session
      (let ((time (or access-time (get-universal-time))))
        ;; The session value itself is handed back unchanged; what changes is
        ;; the store-side timestamp.
        (store-session store sid session)
        (%record-access-time store sid time)
        time))))

(defgeneric session-last-access-time (store sid)
  (:documentation "Return when the session under SID was last used, as a universal
   time, or NIL when that is not known (unknown id, or never touched).

   @param store [<session-store>] Session store instance
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL
   "))

(defmethod session-last-access-time ((store <session-store>) sid)
  "Default implementation: read the base class' own bookkeeping, which is what
   the default TOUCH-SESSION writes."
  (%access-time store sid))


;;; ------------------------------------------------------------------
;;; Bridge to the lack session stage
;;; ------------------------------------------------------------------
;;;
;;; lack/middleware/session calls lack/session/store:{fetch,store,remove}-session
;;; on the object it was given as :store.  These three methods are the whole
;;; reason a CLOS store works there: they translate lack's protocol into the
;;; clails protocol above, so a derivation only ever implements the clails
;;; generic functions.  lack has no fourth operation to bridge.

(defmethod lack/middleware/session/store:fetch-session ((store <session-store>) sid)
  (fetch-session store sid))

(defmethod lack/middleware/session/store:store-session ((store <session-store>) sid session)
  (store-session store sid session))

(defmethod lack/middleware/session/store:remove-session ((store <session-store>) sid)
  (remove-session store sid))
