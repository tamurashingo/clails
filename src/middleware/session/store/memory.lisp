(in-package #:cl-user)
(defpackage #:clails/middleware/session/store/memory
  (:use #:cl)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:export #:<memory-session-store>
           #:make-memory-session-store))
(in-package #:clails/middleware/session/store/memory)


;;; ------------------------------------------------------------------
;;; In-memory session store (the default)
;;; ------------------------------------------------------------------
;;;
;;; What clails uses when *session-store* is NIL: one process-local index from
;;; session id to entry, guarded by a lock.  Sessions live as long as the
;;; process, which is why this is the development / single-process default and
;;; a persistent derivation is the recommendation for production.
;;;
;;; An entry is a small structure holding the session value and the time the
;;; session was last used.  Two reasons for not storing the raw session:
;;;
;;;   * the last-access time must not live inside the session value, because
;;;     the value is the table the application owns (see the protocol notes in
;;;     clails/middleware/session/store);
;;;   * it makes the fourth operation cheap: TOUCH-SESSION REPLACES the index
;;;     entry with one that carries a new timestamp and the SAME value object.
;;;     No session is fetched, serialised or written back, and the value the
;;;     application holds stays identical (EQ), which is what makes touching a
;;;     session on every request affordable and harmless.
;;;
;;; The entry is replaced rather than mutated in place so that a reader that
;;; already holds an entry keeps reading a consistent pair.

(defstruct (entry (:constructor %make-entry))
  (value nil)
  (last-access-time nil :type (or null integer)))


(defclass <memory-session-store> (<session-store>)
  ((index
    :initform (make-hash-table :test 'equal)
    :reader memory-session-store-index
    :documentation "Session id -> ENTRY.")
   (lock
    :initform (bt:make-lock "clails memory session store")
    :reader memory-session-store-lock
    :documentation "Guards INDEX."))
  (:documentation "Session store that keeps sessions in the memory of this process.

   The default store: created by clails/middleware/session-middleware when
   *session-store* is NIL.  Sessions are lost when the process stops and are
   not shared between processes."))


(defun make-memory-session-store ()
  "Create a fresh in-memory session store.

   @return [<memory-session-store>] A store with no sessions in it
   "
  (make-instance '<memory-session-store>))


(defmethod fetch-session ((store <memory-session-store>) sid)
  "Return the session stored under SID, or NIL.

   Reading does not update the last-access time: recording an access is the
   job of TOUCH-SESSION, so that the session stage stays in control of when a
   request counts as activity.

   @param store [<memory-session-store>] Session store instance
   @param sid [string] Session id
   @return [(or hash-table null)] The stored session, or NIL
   "
  (bt:with-lock-held ((memory-session-store-lock store))
    (let ((entry (gethash sid (memory-session-store-index store))))
      (and entry (entry-value entry)))))

(defmethod store-session ((store <memory-session-store>) sid session)
  "Store SESSION under SID and count that as an access.

   @param store [<memory-session-store>] Session store instance
   @param sid [string] Session id
   @param session [hash-table] Session contents to store
   @return [hash-table] SESSION
   "
  (bt:with-lock-held ((memory-session-store-lock store))
    (setf (gethash sid (memory-session-store-index store))
          (%make-entry :value session
                       :last-access-time (get-universal-time))))
  session)

(defmethod remove-session ((store <memory-session-store>) sid)
  "Forget the session stored under SID.

   @param store [<memory-session-store>] Session store instance
   @param sid [string] Session id
   @return [boolean] T when an entry was removed
   "
  (bt:with-lock-held ((memory-session-store-lock store))
    (remhash sid (memory-session-store-index store))))

(defmethod touch-session ((store <memory-session-store>) sid &optional access-time)
  "Record an access to SID by replacing its index entry.

   This is the whole point of the fourth operation: the base class' default
   would fetch the session and write it back again, while here only the index
   entry is exchanged.  STORE-SESSION is not called, the session value is
   neither read nor copied, and the object the application holds stays EQ.

   @param store [<memory-session-store>] Session store instance
   @param sid [string] Session id
   @param access-time [(or integer null)] Universal time, or NIL for now
   @return [(or integer null)] The recorded time, or NIL when SID is unknown
   "
  (let ((time (or access-time (get-universal-time))))
    (bt:with-lock-held ((memory-session-store-lock store))
      (let ((entry (gethash sid (memory-session-store-index store))))
        (when entry
          (setf (gethash sid (memory-session-store-index store))
                (%make-entry :value (entry-value entry)
                             :last-access-time time))
          time)))))

(defmethod session-last-access-time ((store <memory-session-store>) sid)
  "Return when SID was last stored or touched, or NIL when it is unknown.

   @param store [<memory-session-store>] Session store instance
   @param sid [string] Session id
   @return [(or integer null)] Universal time, or NIL
   "
  (bt:with-lock-held ((memory-session-store-lock store))
    (let ((entry (gethash sid (memory-session-store-index store))))
      (and entry (entry-last-access-time entry)))))
