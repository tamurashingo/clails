(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store-wrapper
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:clails-test/support/session-store
                #:make-recording-session-store
                #:make-wrapping-session-store
                #:store-calls
                #:store-call-count
                #:clear-store-calls
                #:run-session-store-roundtrip)
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/wrapper
                #:<session-store-wrapper>
                #:make-session-store-wrapper
                #:ensure-session-store-wrapper
                #:session-store-wrapper-inner
                #:session-lock
                #:note-session-stored
                #:note-session-removed
                #:note-session-touched
                #:session-table-synchronized-p
                #:+synchronized-session-tables-p+
                #:+session-store-env-key+)
  (:import-from #:lack/request
                #:make-request
                #:request-query-parameters))
(in-package #:clails-test/middleware/session-store-wrapper)


;;; M4-T1: the store the session stage wraps around the configured store.
;;;
;;; Two things are asserted here.  First the structure the design note in
;;; clails/middleware/session/store asks for: a <session-store> derivation
;;; that forwards all four operations to one inner store, holds one lock per
;;; session id, and leaves room for the sweep index M5-T3 adds.  Then the
;;; behaviour that made M4 necessary: the session survives concurrent
;;; requests, whichever kind of store is underneath (M4-T2's store-kind
;;; independence).
;;;
;;; The scenario is deliberately the same one the measurement in M1 ran, and
;;; the same one clails-test/middleware/session-concurrency runs against the
;;; default configuration; what changes from test to test here is only the
;;; store that is wrapped.


;;; ------------------------------------------------------------------
;;; Structure
;;; ------------------------------------------------------------------

(deftest session-store-wrapper-structure-test
  (testing "the wrapper derives from the store base class"
    (let ((wrapper (make-session-store-wrapper (make-memory-session-store))))
      (ok (typep wrapper '<session-store>)
          "so it is acceptable to the session stage wherever a store is")
      (ok (typep wrapper '<session-store-wrapper>))))

  (testing "the inner store is reachable and is the one that was handed in"
    (let* ((inner (make-memory-session-store))
           (wrapper (make-session-store-wrapper inner)))
      (ok (eq (session-store-wrapper-inner wrapper) inner))))

  (testing "wrapping is idempotent"
    (let* ((inner (make-memory-session-store))
           (wrapper (ensure-session-store-wrapper inner)))
      (ok (typep wrapper '<session-store-wrapper>) "a bare store is wrapped")
      (ok (eq (ensure-session-store-wrapper wrapper) wrapper)
          "a store that already is a wrapper is returned as it is")
      (ok (eq (session-store-wrapper-inner wrapper) inner)
          "and no second layer of locks appears")))

  (testing "one lock per session id, and the same lock every time"
    (let ((wrapper (make-session-store-wrapper (make-memory-session-store))))
      (ok (eq (session-lock wrapper "sid-1") (session-lock wrapper "sid-1"))
          "the same id always yields the same lock")
      (ok (not (eq (session-lock wrapper "sid-1") (session-lock wrapper "sid-2")))
          "different ids yield different locks, so they never wait on each other"))))


(deftest session-store-wrapper-forwarding-test
  (testing "all four operations reach the inner store"
    (let* ((inner (make-recording-session-store))
           (wrapper (make-session-store-wrapper inner))
           (session (make-hash-table :test 'equal)))
      (setf (gethash "k" session) "v")
      (store-session wrapper "sid" session)
      (ok (eq (fetch-session wrapper "sid") session)
          "the stored session comes back as the same object")
      (ok (equal (gethash "k" (fetch-session inner "sid")) "v")
          "and it is the inner store that holds it")
      ;; the fourth operation: the recording store implements only three, so
      ;; the base class' default has to carry it through the wrapper
      (ok (eql (touch-session wrapper "sid" 3000000000) 3000000000)
          "touch-session answers with the time it recorded")
      (ok (eql (session-last-access-time wrapper "sid") 3000000000)
          "and the time can be read back")
      (ok (remove-session wrapper "sid")
          "remove-session reaches the inner store")
      (ok (null (fetch-session inner "sid")) "which forgot the session")
      (ok (and (plusp (store-call-count inner :store))
               (plusp (store-call-count inner :fetch))
               (plusp (store-call-count inner :remove)))
          (format nil "the inner store saw every operation: ~S"
                  (mapcar #'car (store-calls inner))))))

  (testing "the fourth operation is passed through to a store that implements it"
    (let* ((inner (make-memory-session-store))
           (wrapper (make-session-store-wrapper inner))
           (session (make-hash-table :test 'equal)))
      (store-session wrapper "sid" session)
      (touch-session wrapper "sid" 3000000000)
      (ok (eql (session-last-access-time wrapper "sid") 3000000000))
      (ok (eql (session-last-access-time inner "sid") 3000000000)
          "the inner store is where the time was recorded")
      (ok (eq (fetch-session inner "sid") session)
          "and touching did not replace the session value")))

  (testing "a lack session store structure can be wrapped too"
    ;; *session-store* accepts a lack store structure, so the wrapper has to
    ;; forward through lack's protocol as well as through the clails one.
    (let* ((inner (make-lack-memory-session-store))
           (wrapper (make-session-store-wrapper inner))
           (session (make-hash-table :test 'equal)))
      (setf (gethash "k" session) "v")
      (store-session wrapper "sid" session)
      (ok (eq (fetch-session wrapper "sid") session))
      (ok (equal (gethash "k" (fetch-stored-session inner "sid")) "v")
          "the session is in the lack store")
      ;; lack has no fourth operation; the base class' default stands in
      (ok (eql (touch-session wrapper "sid" 3000000000) 3000000000))
      (ok (eql (session-last-access-time wrapper "sid") 3000000000))
      (remove-session wrapper "sid")
      (ok (null (fetch-stored-session inner "sid"))))))


;;; The sweep index M5-T3 adds goes into this same object.  Proving that it
;;; can be added *without touching the wrapper* is M4's job, so here is a
;;; subclass that does exactly what the sweeper will need -- an index of live
;;; session ids with their last-access time -- built out of nothing but the
;;; three hooks.

(defclass <indexing-session-store> (<session-store-wrapper>)
  ((ids :initform (make-hash-table :test 'equal) :reader indexed-ids)))

(defmethod note-session-stored ((store <indexing-session-store>) sid session)
  (declare (ignore session))
  (setf (gethash sid (indexed-ids store)) t))

(defmethod note-session-removed ((store <indexing-session-store>) sid)
  (remhash sid (indexed-ids store)))

(defmethod note-session-touched ((store <indexing-session-store>) sid access-time)
  (when access-time
    (setf (gethash sid (indexed-ids store)) access-time)))

(defun indexed-session-ids (store)
  (sort (loop for sid being the hash-keys of (indexed-ids store) collect sid)
        #'string<))


(deftest session-store-wrapper-index-room-test
  (testing "an index of live session ids can be added as a subclass (M5-T3)"
    (let* ((inner (make-memory-session-store))
           (store (make-instance '<indexing-session-store> :inner inner)))
      (ok (null (indexed-session-ids store)) "the index starts empty")
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let ((first-id (response-session-id (call-app app "/")))
              (second-id (response-session-id (call-app app "/"))))
          (ok (equal (indexed-session-ids store)
                     (sort (list first-id second-id) #'string<))
              "both sessions were noticed on their way into the store")
          (touch-session store first-id 3000000000)
          (ok (eql (gethash first-id (indexed-ids store)) 3000000000)
              "and touching refreshes the index entry")
          (remove-session store first-id)
          (ok (equal (indexed-session-ids store) (list second-id))
              "a removed session leaves the index")
          (ok (null (fetch-stored-session inner first-id))
              "and the inner store too"))))))


;;; ------------------------------------------------------------------
;;; The stage installs the wrapper by itself
;;; ------------------------------------------------------------------

(deftest session-stage-installs-wrapper-test
  (testing "the configured store is wrapped, and published in the environment"
    (let ((store (make-memory-session-store))
          (seen-store :unset)
          (seen-session :unset))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen-store (getf env +session-store-env-key+))
                                            (setf seen-session (getf env :lack.session))
                                            (setf (gethash "k" (getf env :lack.session)) "v"))))
        (let ((session-id (response-session-id (call-app app "/"))))
          (ok (typep seen-store '<session-store-wrapper>)
              "the application is handed the wrapping store")
          (ok (eq (session-store-wrapper-inner seen-store) store)
              "which wraps the store that was configured")
          ;; the session the application wrote into is the session the store
          ;; holds -- the wrapper does not hand out copies
          (ok (eq (fetch-stored-session store session-id) seen-session)
              "the stored session is the object the application wrote into")
          (ok (equal (gethash "k" (fetch-stored-session store session-id)) "v"))))))

  (testing "the container the application writes into tolerates concurrent access"
    (let ((containers '()))
      (with-middleware-stack (app :store (make-memory-session-store)
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (push (getf env :lack.session) containers)
                                            (setf (gethash "k" (getf env :lack.session)) "v"))))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/")))
          (declare (ignore first-response))
          (client-request client "/")
          (ok (= (length containers) 2) "two requests, two looks at the container")
          (ok (eq (first containers) (second containers))
              "the continuing request works on the very same container")
          (if +synchronized-session-tables-p+
              (ok (every #'session-table-synchronized-p containers)
                  "the container is a synchronized hash-table from the first request on")
              (ok t "this implementation has no synchronized hash-tables (see the portability note)")))))))


;;; ------------------------------------------------------------------
;;; The round trip is unchanged by the wrapper (G2)
;;; ------------------------------------------------------------------

(deftest session-store-wrapper-roundtrip-test
  (testing "the shared round-trip body succeeds through the wrapper, for every store"
    (dolist (entry (list (cons "wrapped clails memory store"
                               (make-memory-session-store))
                         (cons "wrapped minimal test store"
                               (make-recording-session-store))
                         (cons "wrapped lack memory store"
                               (make-lack-memory-session-store))
                         (cons "wrapped delegating test store"
                               (make-wrapping-session-store (make-memory-session-store)))))
      (destructuring-bind (label . inner) entry
        (let ((sid (run-session-store-roundtrip (make-session-store-wrapper inner)
                                                :label label)))
          (ok (and sid (= (length sid) 40))
              (format nil "~A: the body ran to the end" label)))))))


;;; ------------------------------------------------------------------
;;; The scenario that used to damage the session, per store kind (M4-T2)
;;; ------------------------------------------------------------------

(defparameter +bulk-keys+ 20
  "Keys written by one request of the bulk-write scenario.")

(defun bulk-writer-app ()
  (make-recording-app
   (lambda (env)
     (let* ((session (getf env :lack.session))
            (params (request-query-parameters (make-request env)))
            (writer (cdr (assoc "writer" params :test #'string=)))
            (n (cdr (assoc "n" params :test #'string=))))
       (dotimes (index +bulk-keys+)
         (setf (gethash (format nil "w-~A-~A-~D" writer n index) session) index))))))

(defun assert-bulk-write-survives (inner label &key (threads 4) (iterations 25))
  "Run the 4-concurrent bulk-write scenario against INNER and assert that the
   session survived it: nothing signalled, the container is intact, every
   written key is still there, and it can still be used."
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
      (ok (= (hash-table-count session) expected)
          (format nil "~A: ~D of ~D written keys survived"
                  label (hash-table-count session) expected))
      (setf (gethash "after" session) "ok")
      (ok (equal (gethash "after" session) "ok")
          (format nil "~A: the session is still usable afterwards" label)))))


(deftest session-store-wrapper-concurrency-test
  (testing "4 concurrent writers do not damage the session, whichever store is wrapped"
    (assert-bulk-write-survives (make-memory-session-store) "clails memory store")
    (assert-bulk-write-survives (make-recording-session-store) "minimal test store")
    (assert-bulk-write-survives (make-lack-memory-session-store) "lack memory store")
    (assert-bulk-write-survives (make-wrapping-session-store (make-memory-session-store))
                                "delegating test store")))
