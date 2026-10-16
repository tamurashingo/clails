(in-package #:cl-user)
(defpackage #:clails-test/support/session-store
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:export ;; a minimal derivation: the three operations and nothing else
           #:<recording-session-store>
           #:make-recording-session-store
           #:store-calls
           #:store-call-count
           #:clear-store-calls
           ;; a derivation that wraps another store and keeps its own index
           #:<wrapping-session-store>
           #:make-wrapping-session-store
           #:wrapping-store-inner
           #:wrapping-store-ids
           ;; the shared round-trip body, run against any store
           #:run-session-store-roundtrip))
(in-package #:clails-test/support/session-store)


;;; Stores written for the tests, and the one round-trip body that every
;;; store has to satisfy.
;;;
;;; The point of this file is M3-T2: the guarantee that the session stage
;;; works with ANY store deriving from clails/middleware/session/store, not
;;; only with the one clails ships.  RUN-SESSION-STORE-ROUNDTRIP therefore
;;; takes the store as an argument and makes no assumption about its class --
;;; which is also what lets M4 hand it a store wrapped in the serialising
;;; layer and get the same verdict.


;;; ------------------------------------------------------------------
;;; A minimal derivation
;;; ------------------------------------------------------------------

(defclass <recording-session-store> (<session-store>)
  ((sessions
    :initform (make-hash-table :test 'equal)
    :reader recording-store-sessions)
   (calls
    :initform '()
    :accessor recording-store-calls
    :documentation "Operations seen so far, newest first, as (OP . SID).")
   (lock
    :initform (bt:make-lock "clails-test recording session store")
    :reader recording-store-lock))
  (:documentation "The smallest store a third party could write: a hash-table
   plus the three operations of the abstraction.

   It deliberately does NOT implement TOUCH-SESSION, so it also stands for
   \"a store whose author never heard of the fourth operation\" -- the case
   the base class' default implementation has to carry.  Every operation is
   recorded, which is how a test shows that the session stage really drives
   the store it was given."))

(defun make-recording-session-store ()
  (make-instance '<recording-session-store>))

(defun record-call (store op sid)
  (bt:with-lock-held ((recording-store-lock store))
    (push (cons op sid) (recording-store-calls store))))

(defun store-calls (store)
  "Operations seen by STORE, oldest first, as a list of (OP . SID)."
  (bt:with-lock-held ((recording-store-lock store))
    (reverse (recording-store-calls store))))

(defun store-call-count (store op)
  "How many times OP (:FETCH, :STORE or :REMOVE) was called on STORE."
  (count op (store-calls store) :key #'car))

(defun clear-store-calls (store)
  (bt:with-lock-held ((recording-store-lock store))
    (setf (recording-store-calls store) '())))

(defmethod fetch-session ((store <recording-session-store>) sid)
  (record-call store :fetch sid)
  (gethash sid (recording-store-sessions store)))

(defmethod store-session ((store <recording-session-store>) sid session)
  (record-call store :store sid)
  (setf (gethash sid (recording-store-sessions store)) session))

(defmethod remove-session ((store <recording-session-store>) sid)
  (record-call store :remove sid)
  (remhash sid (recording-store-sessions store)))


;;; ------------------------------------------------------------------
;;; A derivation that wraps another store
;;; ------------------------------------------------------------------

(defclass <wrapping-session-store> (<session-store>)
  ((inner
    :initarg :inner
    :reader wrapping-store-inner)
   (ids
    :initform (make-hash-table :test 'equal)
    :reader wrapping-store-id-index)
   (lock
    :initform (bt:make-lock "clails-test wrapping session store")
    :reader wrapping-store-lock))
  (:documentation "A store that forwards the four operations to another store
   and keeps its own index of the session ids it has seen.

   This is a stand-in for the wrapping store M4 implements -- see the design
   note in clails/middleware/session/store -- and it exists here for one
   reason: to prove, now, that inserting such a layer between the session
   stage and the configured store does not disturb anything, and that an
   index of live ids can be added later without changing the protocol.  The
   real wrapper additionally serialises access per session id (M4) and is the
   home of the sweeper's index (M5-T3); neither is attempted here."))

(defun make-wrapping-session-store (inner)
  (make-instance '<wrapping-session-store> :inner inner))

(defun wrapping-store-ids (store)
  "The session ids currently in STORE's own index, sorted."
  (sort (loop for sid being the hash-keys of (wrapping-store-id-index store)
              collect sid)
        #'string<))

(defmethod fetch-session ((store <wrapping-session-store>) sid)
  (fetch-session (wrapping-store-inner store) sid))

(defmethod store-session ((store <wrapping-session-store>) sid session)
  (bt:with-lock-held ((wrapping-store-lock store))
    (setf (gethash sid (wrapping-store-id-index store)) t))
  (store-session (wrapping-store-inner store) sid session))

(defmethod remove-session ((store <wrapping-session-store>) sid)
  (bt:with-lock-held ((wrapping-store-lock store))
    (remhash sid (wrapping-store-id-index store)))
  (remove-session (wrapping-store-inner store) sid))

(defmethod touch-session ((store <wrapping-session-store>) sid &optional access-time)
  (touch-session (wrapping-store-inner store) sid access-time))

(defmethod session-last-access-time ((store <wrapping-session-store>) sid)
  (session-last-access-time (wrapping-store-inner store) sid))


;;; ------------------------------------------------------------------
;;; The shared round-trip body
;;; ------------------------------------------------------------------

(defun run-session-store-roundtrip (store &key (label nil))
  "Drive a full HTTP round trip over the clails session stage backed by STORE
   and assert the behaviour every store owes, whatever its class.

   STORE may be a clails <session-store> derivation, a store wrapping another
   store, or a lack session store structure: nothing below reaches past the
   session protocol.  LABEL names the store in the assertion descriptions, so
   one failing line says which store failed.

   Returns the session id that was used."
  (let ((label (or label (string (type-of store)))))
    (flet ((note (text) (format nil "~A: ~A" label text)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/"))
               (sid (response-session-id first-response)))
          ;; 1. the stage answers, mints a session id and runs the application
          (ok (= (response-status first-response) 200)
              (note "the first request is answered"))
          (ok (and sid (= (length sid) 40))
              (note "the first response carries a 40 character session id"))
          (ok (string= (response-body-string first-response) "1")
              (note "the application saw an empty session"))

          ;; 2. the value the application wrote is in the store we handed in
          (let ((stored (fetch-stored-session store sid)))
            (ok (hash-table-p stored)
                (note "the session is in the store, under that id"))
            (ok (eql (gethash "counter" stored) 1)
                (note "the stored session holds what the application wrote")))

          ;; 3. the value is carried over to later requests with that cookie
          (ok (string= (response-body-string (client-request client "/")) "2")
              (note "the second request sees the first request's value"))
          (ok (string= (response-body-string (client-request client "/")) "3")
              (note "and so does the third"))
          (ok (eql (gethash "counter" (fetch-stored-session store sid)) 3)
              (note "the store holds the latest value"))

          ;; 4. a client that sends no cookie gets its own session
          (ok (string= (response-body-string (call-app app "/")) "1")
              (note "a cookie-less request gets a fresh session"))

          ;; 5. dropping the session from the store ends it
          (remove-stored-session store sid)
          (ok (null (fetch-stored-session store sid))
              (note "the session is gone from the store"))
          (ok (string= (response-body-string (client-request client "/")) "1")
              (note "the same cookie now opens an empty session"))
          sid)))))
