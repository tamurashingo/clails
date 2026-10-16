(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store-swap
  (:use #:cl
        #:rove
        #:clails-test/support/session-store)
  (:import-from #:clails-test/support/http
                #:with-middleware-stack
                #:call-app
                #:make-session-client
                #:client-request
                #:make-session-counter-app
                #:response-body-string
                #:response-session-id
                #:make-memory-session-store
                #:make-lack-memory-session-store
                #:fetch-stored-session
                #:remove-stored-session)
  (:import-from #:clails/middleware/session/store
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time))
(in-package #:clails-test/middleware/session-store-swap)


;;; M3-T2: the guarantee that the session stage works with ANY store.
;;;
;;; One round-trip body -- clails-test/support/session-store:
;;; RUN-SESSION-STORE-ROUNDTRIP -- is run against every store below.  It takes
;;; the store as an argument and never mentions a class, which is the property
;;; M4 depends on: when the serialising wrapper exists it is handed to the
;;; same body, and the expected values do not change.


(defun named-stores ()
  "The stores the shared round-trip body is run against, freshly built.

   Each entry is (LABEL . STORE).  Adding a store here is all it takes to put
   it under the same guarantee."
  (let ((memory (make-memory-session-store))
        (recording (make-recording-session-store)))
    (list (cons "clails memory store" memory)
          (cons "minimal test store" recording)
          (cons "wrapper around the memory store"
                (make-wrapping-session-store (make-memory-session-store)))
          (cons "wrapper around the minimal test store"
                (make-wrapping-session-store (make-recording-session-store)))
          ;; A lack session store structure, which is what *session-store* was
          ;; documented to hold before the clails abstraction existed.  It has
          ;; to keep working unchanged.
          (cons "lack memory store (compatibility)"
                (make-lack-memory-session-store)))))


(deftest session-store-swap-test
  (testing "the same round trip succeeds against every store"
    (dolist (entry (named-stores))
      (destructuring-bind (label . store) entry
        (let ((sid (run-session-store-roundtrip store :label label)))
          (ok (and sid (= (length sid) 40))
              (format nil "~A: the body ran to the end" label)))))))


(deftest session-store-swap-invocation-test
  (testing "the stage really drives the store it was given"
    (let ((store (make-recording-session-store)))
      (run-session-store-roundtrip store :label "minimal test store")
      (ok (plusp (store-call-count store :fetch))
          "fetch-session was called on the test store")
      (ok (plusp (store-call-count store :store))
          "store-session was called on the test store")
      (ok (plusp (store-call-count store :remove))
          "remove-session was called on the test store")))

  (testing "a request that opens a session stores it; a continuing request fetches it"
    (let ((store (make-recording-session-store)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let ((client (make-session-client app)))
          (clear-store-calls store)
          (client-request client "/")
          (ok (equal (mapcar #'car (store-calls store)) (list :store))
              "the session-creating request only stored")
          (clear-store-calls store)
          (client-request client "/")
          (ok (equal (mapcar #'car (store-calls store)) (list :fetch :store))
              "a continuing request fetched the session and stored it back")))))

  (testing "no store is reached at all while the session stage is disabled"
    (let ((store (make-recording-session-store)))
      (with-middleware-stack (app :session nil :store store)
        (call-app app "/")
        (ok (null (store-calls store)))))))


(deftest session-store-swap-wrapping-test
  ;; The wrapping store is the shape M4 implements and M5-T3 extends.  What is
  ;; asserted here is only what M3 owes: inserting such a layer changes
  ;; nothing a caller can see, and a layer like it can keep its own index of
  ;; session ids -- the only way a sweeper can ever enumerate sessions, since
  ;; the protocol has no operation for that.
  (testing "a wrapper is substitutable for the store it wraps"
    (let* ((inner (make-recording-session-store))
           (wrapper (make-wrapping-session-store inner)))
      (run-session-store-roundtrip wrapper :label "wrapper")
      (ok (plusp (store-call-count inner :store))
          "the operations reached the inner store")
      (ok (plusp (store-call-count inner :fetch)))
      (ok (plusp (store-call-count inner :remove)))))

  (testing "the wrapper's own index follows the sessions"
    (let* ((inner (make-memory-session-store))
           (wrapper (make-wrapping-session-store inner)))
      (ok (null (wrapping-store-ids wrapper)) "the index starts empty")
      (with-middleware-stack (app :store wrapper :inner (make-session-counter-app))
        (let* ((first-id (response-session-id (call-app app "/")))
               (second-id (response-session-id (call-app app "/"))))
          (ok (equal (wrapping-store-ids wrapper)
                     (sort (list first-id second-id) #'string<))
              "both sessions are in the index")
          (remove-stored-session wrapper first-id)
          (ok (equal (wrapping-store-ids wrapper) (list second-id))
              "a removed session leaves the index")
          (ok (null (fetch-stored-session inner first-id))
              "and leaves the inner store too")))))

  (testing "the fourth operation passes through the wrapper to the inner store"
    (let* ((inner (make-memory-session-store))
           (wrapper (make-wrapping-session-store inner)))
      (store-session wrapper "sid" (make-hash-table :test 'equal))
      (touch-session wrapper "sid" 3000000000)
      (ok (eql (session-last-access-time wrapper "sid") 3000000000))
      (ok (eql (session-last-access-time inner "sid") 3000000000)
          "the inner store is where the time was recorded")))

  (testing "wrappers nest"
    (let ((store (make-wrapping-session-store
                  (make-wrapping-session-store (make-memory-session-store)))))
      (run-session-store-roundtrip store :label "nested wrappers"))))
