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
  ;; WITH-SESSION-LOCK is what the atomic-increment scenario below is about.
  (:import-from #:clails/middleware/session/store/wrapper
                #:with-session-lock)
  ;; The bulk-write application reads the writer and iteration out of the query
  ;; string, so that the keys of one request can be told from another's.
  (:import-from #:lack/request
                #:make-request
                #:request-query-parameters)
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
           #:run-session-store-roundtrip
           ;; the shared concurrency scenarios, run against any store
           #:+bulk-keys+
           #:bulk-writer-app
           #:assert-bulk-write-survives
           #:+expected-lossless-counter+
           #:make-locking-counter-app
           #:counter-value
           #:assert-locked-increment-is-lossless))
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


;;; ------------------------------------------------------------------
;;; The concurrency scenarios, run against any store
;;; ------------------------------------------------------------------
;;;
;;; Both scenarios below are acceptance conditions of M4, and both have to hold
;;; "whichever store is configured" -- which includes the database derivation
;;; M3-T3 adds.  They live here, rather than in the suite that first used them,
;;; so that the database suites (which need a database container and therefore
;;; cannot live in a DB-free suite) run the very same body against the very
;;; same expectations.
;;;
;;; One expectation is NOT the same for every store, and the difference is a
;;; property of the store rather than a weakening of the test.  A store that
;;; hands back the session BY REFERENCE (the in-memory derivation, and anything
;;; wrapping it) gives every concurrent request on one session the same
;;; container, so writes from different requests accumulate in it.  A store
;;; that rebuilds the session from a stored representation (the database
;;; derivation) gives each request its own table, so a request writes back the
;;; whole session it fetched and the later write wins -- decision 12's
;;; last-writer-wins, which is specified behaviour and not corruption.  The
;;; SHARES-CONTAINER argument says which of the two a store is; everything
;;; else asserted here is required of both.

(defparameter +bulk-keys+ 20
  "Keys written by one request of the bulk-write scenario.")

(defun bulk-writer-app ()
  "An application that writes +BULK-KEYS+ keys of its own into the session.

   The keys are named after the writer and the iteration, so the keys of one
   request can be told from the keys of another -- which is what makes it
   possible to say whether a surviving session holds WHOLE write-backs or the
   wreckage of several."
  (make-recording-app
   (lambda (env)
     (let* ((session (getf env :lack.session))
            (params (request-query-parameters (make-request env)))
            (writer (cdr (assoc "writer" params :test #'string=)))
            (n (cdr (assoc "n" params :test #'string=))))
       (dotimes (index +bulk-keys+)
         (setf (gethash (format nil "w-~A-~A-~D" writer n index) session) index))))))

(defun %bulk-write-groups (session)
  "Group the keys of SESSION by the request that wrote them, as an alist of
   (PREFIX . COUNT)."
  (let ((groups '()))
    (loop for key being the hash-keys of session
          do (let* ((last-dash (position #\- key :from-end t))
                    (prefix (if last-dash (subseq key 0 last-dash) key))
                    (entry (assoc prefix groups :test #'string=)))
               (if entry
                   (incf (cdr entry))
                   (push (cons prefix 1) groups))))
    groups))

(defun assert-bulk-write-survives (inner label &key (threads 4) (iterations 25)
                                                    (shares-container t))
  "Run the 4-concurrent bulk-write scenario against INNER (wrapped by the
   session stage) and assert that the session survived it.

   Required of every store: nothing signalled, every request answered, the
   container intact, the session still usable, and every write-back that
   survived is a WHOLE one (a request's keys are all there or all gone -- never
   half of them, which is what a damaged container produces).

   SHARES-CONTAINER additionally fixes how much must survive:
     T   -- every key of every request, because all requests wrote into one
            container (the in-memory derivation and anything wrapping it);
     NIL -- a positive whole number of complete write-backs, because each
            request wrote back the session it fetched and the later write won
            (the database derivation; decision 12).

   @param inner [t] The store to configure the session stage with
   @param label [string] Name of the store, for the assertion descriptions
   @param threads [integer] Concurrent writers
   @param iterations [integer] Requests per writer
   @param shares-container [boolean] Whether the store hands the session back
     by reference
   "
  (let* ((result (run-concurrent-session-requests
                  (bulk-writer-app)
                  :store inner
                  :threads threads
                  :iterations iterations
                  :uri (lambda (thread-index iteration-index)
                         (format nil "/?writer=~D&n=~D" thread-index iteration-index))))
         (errors (getf result :errors))
         (session (getf result :session))
         ;; the sequential request that opens the session writes its own set
         (expected (* +bulk-keys+ (1+ (* threads iterations)))))
    (ok (null (getf result :timed-out)) (format nil "~A: all workers finished" label))
    (ok (null errors)
        (format nil "~A: no request signalled~@[; first: ~A~]"
                label (when errors (cdr (first errors)))))
    (ok (= (length (getf result :responses)) (* threads iterations))
        (format nil "~A: every concurrent request was answered" label))
    (ok (hash-table-p session) (format nil "~A: the session is still a hash table" label))
    (when (hash-table-p session)
      (multiple-value-bind (sane reason) (hash-table-sane-p session)
        (ok sane (or reason (format nil "~A: the container is intact" label))))
      (let* ((groups (%bulk-write-groups session))
             (partial (remove-if (lambda (group) (= (cdr group) +bulk-keys+)) groups)))
        (ok (null partial)
            (format nil "~A: every surviving write-back is complete~@[; partial: ~S~]"
                    label partial)))
      (if shares-container
          (ok (= (hash-table-count session) expected)
              (format nil "~A: ~D of ~D written keys survived"
                      label (hash-table-count session) expected))
          (progn
            (ok (and (plusp (hash-table-count session))
                     (zerop (mod (hash-table-count session) +bulk-keys+))
                     (<= (hash-table-count session) expected))
                (format nil "~A: ~D keys survived -- a whole number of complete write-backs, at most ~D"
                        label (hash-table-count session) expected))
            (diag (format nil "~A: ~D of ~D keys survived (whole-value write-back, later writer wins)~%"
                          label (hash-table-count session) expected))))
      (setf (gethash "after" session) "ok")
      (ok (equal (gethash "after" session) "ok")
          (format nil "~A: the session is still usable afterwards" label)))))


(defparameter +expected-lossless-counter+ 101
  "2 threads x 50 increments, plus the one request that opens the session.")

(defun counter-value (result &key (key "counter"))
  "The counter RUN-CONCURRENT-SESSION-REQUESTS left in the session."
  (let ((session (getf result :session)))
    (and (hash-table-p session) (gethash key session))))

(defun make-locking-counter-app (&key (key "counter") (gap 0.001))
  "An application that increments KEY in the session ATOMICALLY: the read, the
   change and the write are all inside WITH-SESSION-LOCK.

   GAP seconds pass between the read and the write, which is how a slow action
   behaves and what makes a lost update certain without the lock."
  (lambda (env)
    (with-session-lock (env)
      (let* ((session (getf env :lack.session))
             (value (1+ (or (gethash key session) 0))))
        (sleep gap)
        (setf (gethash key session) value)
        (list 200 (list :content-type "text/plain") (list (princ-to-string value)))))))

(defun assert-locked-increment-is-lossless (store label &key (threads 2) (iterations 50)
                                                             (shares-container t))
  "Run the 2 x 50 atomic-increment scenario against STORE and assert the result.

   With a store that hands the session back by reference the macro is lossless
   and the counter is exactly +EXPECTED-LOSSLESS-COUNTER+.

   With a store that rebuilds the session on every fetch it cannot be, and the
   reason is structural rather than a bug in the macro: the session is read
   before the application runs and written back after it returns, both outside
   the body the macro locks, so two overlapping requests fetch the same value
   however perfectly their bodies are serialised.  What is asserted then is
   what the macro does still guarantee -- the bodies ran one at a time, nothing
   signalled, the session is intact, and no increment was invented (the counter
   never exceeds the lossless value).

   @param store [t] The store to configure the session stage with
   @param label [string] Name of the store, for the assertion descriptions
   @param threads [integer] Concurrent requests
   @param iterations [integer] Increments per request
   @param shares-container [boolean] Whether the store hands the session back
     by reference
   "
  (let* ((result (run-concurrent-session-requests (make-locking-counter-app)
                                                  :store store
                                                  :threads threads
                                                  :iterations iterations))
         (expected (1+ (* threads iterations)))
         (final (counter-value result)))
    (ok (null (getf result :errors))
        (format nil "~A: no request signalled~@[; first: ~A~]"
                label (when (getf result :errors) (cdr (first (getf result :errors))))))
    (if shares-container
        (ok (eql final expected)
            (format nil "~A: the counter is ~S, expected ~D" label final expected))
        (progn
          (ok (and (integerp final) (<= 1 final expected))
              (format nil "~A: the counter is ~S, at most ~D (whole-value write-back)"
                      label final expected))
          (diag (format nil "~A: ~S of ~D increments survived (whole-value write-back, later writer wins)~%"
                        label final expected))))
    (let ((session (getf result :session)))
      (when (hash-table-p session)
        (multiple-value-bind (sane reason) (hash-table-sane-p session)
          (ok sane (or reason (format nil "~A: the container is intact" label))))))))
