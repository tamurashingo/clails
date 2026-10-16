(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-lock
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:clails-test/support/session-store
                #:make-recording-session-store
                #:make-wrapping-session-store)
  (:import-from #:clails/middleware/session/store
                #:fetch-session)
  (:import-from #:clails/middleware/session/store/wrapper
                #:call-with-session-lock
                #:with-session-lock
                #:+session-store-env-key+)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:session))
(in-package #:clails-test/middleware/session-lock)


;;; M4-T2: WITH-SESSION-LOCK, the public way to make a read-modify-write on
;;; the session atomic.
;;;
;;; The session stage guarantees that concurrent requests cannot damage the
;;; session (see clails-test/middleware/session-concurrency), and that is all
;;; it guarantees: "read the counter, add one, write it back" is three steps,
;;; and two requests can interleave them and apply one increment instead of
;;; two.  That is specified behaviour -- the same position Rails and Django
;;; take -- and this macro is the way out for an application that cannot live
;;; with it.
;;;
;;; The acceptance condition is the scenario of the milestone: two threads,
;;; fifty increments each, on ONE session.  101 is the lossless result,
;;; because the sequential request that opens the session counts once too.
;;;
;;; Every counter application below reads the value, waits, and only then
;;; writes it back.  The wait is what makes the test mean something: with it,
;;; an unprotected read-modify-write loses increments on every run, so "101"
;;; cannot come out by luck of the scheduler.  The lossy variant is still NOT
;;; asserted to be lossy (losing an update is a race, and a test that demands
;;; a race is a flaky test); it is only reported, and bounded from above.


(defparameter +expected-lossless-counter+ 101
  "2 threads x 50 increments, plus the one request that opens the session.")

(defun counter-value (result)
  (let ((session (getf result :session)))
    (and (hash-table-p session) (gethash "counter" session))))

(defun make-locking-counter-app (&key (key "counter") (gap 0.001))
  "An application that increments KEY in the session ATOMICALLY: the read,
   the change and the write are all inside WITH-SESSION-LOCK.

   GAP seconds pass between the read and the write, which is how a slow
   action behaves and what makes a lost update certain without the lock."
  (lambda (env)
    (with-session-lock (env)
      (let* ((session (getf env :lack.session))
             (value (1+ (or (gethash key session) 0))))
        (sleep gap)
        (setf (gethash key session) value)
        (list 200 (list :content-type "text/plain") (list (princ-to-string value)))))))

(defun make-unlocked-counter-app (&key (key "counter") (gap 0.001))
  "The same application WITHOUT the lock: the shape of an ordinary action,
   and the behaviour the macro exists to fix."
  (lambda (env)
    (let* ((session (getf env :lack.session))
           (value (1+ (or (gethash key session) 0))))
      (sleep gap)
      (setf (gethash key session) value)
      (list 200 (list :content-type "text/plain") (list (princ-to-string value))))))


;;; ------------------------------------------------------------------
;;; The acceptance condition
;;; ------------------------------------------------------------------

(deftest session-lock-lossless-increment-test
  (testing "2 threads x 50 increments through the macro add up exactly"
    (let* ((result (run-concurrent-session-requests (make-locking-counter-app)
                                                    :threads 2
                                                    :iterations 50))
           (final (counter-value result)))
      (ok (null (getf result :errors)) "no request signalled")
      (ok (= (length (getf result :responses)) 100) "all 100 requests were answered")
      (ok (eql final +expected-lossless-counter+)
          (format nil "the counter is ~S, expected ~D"
                  final +expected-lossless-counter+))
      (let ((session (getf result :session)))
        (when (hash-table-p session)
          (multiple-value-bind (sane reason) (hash-table-sane-p session)
            (ok sane (or reason "the container is intact as well")))))))

  (testing "repeating the scenario keeps producing exactly the expected value"
    ;; Three runs here; the suite itself is run repeatedly to check stability
    ;; over more attempts than a test run can afford.
    (dotimes (attempt 3)
      (let ((final (counter-value
                    (run-concurrent-session-requests (make-locking-counter-app)
                                                     :threads 2
                                                     :iterations 50))))
        (ok (eql final +expected-lossless-counter+)
            (format nil "attempt ~D: ~S" attempt final))))))


(deftest session-lock-without-the-macro-test
  (testing "the same scenario without the macro may lose updates, but never breaks"
    ;; The specification, fixed as a test: an unprotected read-modify-write
    ;; can apply fewer increments than were requested (it is not asserted to,
    ;; because that is a race), and whatever it loses, the session itself is
    ;; still intact and usable.  The counter can never exceed the lossless
    ;; value: more increments than were requested would be a real defect.
    (let* ((result (run-concurrent-session-requests (make-unlocked-counter-app)
                                                    :threads 2
                                                    :iterations 50))
           (final (counter-value result)))
      (ok (null (getf result :errors)) "no request signalled")
      (ok (integerp final) "the counter is readable")
      (when (integerp final)
        (ok (<= 1 final +expected-lossless-counter+)
            (format nil "the counter is ~D, at most ~D"
                    final +expected-lossless-counter+))
        (diag (format nil "without the lock: ~D of ~D increments survived~%"
                      final +expected-lossless-counter+)))
      (let ((session (getf result :session)))
        (when (hash-table-p session)
          (multiple-value-bind (sane reason) (hash-table-sane-p session)
            (ok sane (or reason "the container is intact"))))))))


;;; ------------------------------------------------------------------
;;; Whatever store is configured
;;; ------------------------------------------------------------------

(deftest session-lock-store-kinds-test
  (testing "the macro is lossless with every kind of store underneath"
    (dolist (entry (list (cons "clails memory store" (make-memory-session-store))
                         (cons "minimal test store" (make-recording-session-store))
                         (cons "lack memory store" (make-lack-memory-session-store))
                         (cons "delegating test store"
                               (make-wrapping-session-store (make-memory-session-store)))))
      (destructuring-bind (label . store) entry
        (let ((final (counter-value
                      (run-concurrent-session-requests (make-locking-counter-app)
                                                       :store store
                                                       :threads 2
                                                       :iterations 50))))
          (ok (eql final +expected-lossless-counter+)
              (format nil "~A: the counter is ~S, expected ~D"
                      label final +expected-lossless-counter+)))))))


;;; ------------------------------------------------------------------
;;; Where the macro can be written
;;; ------------------------------------------------------------------

(defclass <lock-test-controller> (<web-controller>)
  ()
  (:documentation "Stands in for an application controller, so that the macro
   is exercised exactly as an action would write it."))

(defmethod bump-counter ((controller <lock-test-controller>))
  "An action: read the session, change it, write it back, atomically."
  (with-session-lock (controller)
    (let ((value (1+ (or (gethash "counter" (session controller)) 0))))
      (setf (gethash "counter" (session controller)) value)
      value)))

(defun make-controller-counter-app ()
  "An application that builds a controller over the request environment and
   calls an action on it, which is where WITH-SESSION-LOCK is meant to be
   written."
  (lambda (env)
    (let ((controller (make-instance '<lock-test-controller>)))
      (setf (slot-value controller 'clails/controller/base-controller::env) env)
      (list 200
            (list :content-type "text/plain")
            (list (princ-to-string (bump-counter controller)))))))


(deftest session-lock-callers-test
  (testing "an action can take the lock through its controller"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-controller-counter-app))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/"))
               (session-id (response-session-id first-response)))
          (ok (string= (response-body-string first-response) "1")
              "the action ran and answered")
          (ok (string= (response-body-string (client-request client "/")) "2")
              "and the value it wrote under the lock was kept")
          (ok (eql (gethash "counter" (fetch-stored-session store session-id)) 2)
              "the store holds what the action wrote")))))

  (testing "concurrent requests through the controller are lossless too"
    (let ((final (counter-value
                  (run-concurrent-session-requests (make-controller-counter-app)
                                                   :threads 2
                                                   :iterations 50))))
      (ok (eql final +expected-lossless-counter+)
          (format nil "the counter is ~S, expected ~D"
                  final +expected-lossless-counter+))))

  (testing "a stage holding the request environment can take the lock directly"
    (let ((inside nil))
      (with-middleware-stack (app :store (make-memory-session-store)
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (with-session-lock (env)
                                              (setf inside t)))))
        (call-app app "/")
        (ok inside "the body ran with the environment as the source"))))

  (testing "without the session stage there is nothing to lock, and that is an error"
    (ok (signals (with-session-lock ('(:path-info "/")) :never) 'error)
        "a request environment without the session stage is refused")
    (ok (signals (with-session-lock (:not-an-environment) :never) 'error)
        "and so is something that is not an environment at all"))

  (testing "a store with no per-session lock refuses to pretend"
    ;; The extension point: a store that can serialise sessions in its own
    ;; medium implements CALL-WITH-SESSION-LOCK; one that cannot must say so
    ;; rather than run the body unprotected.
    (ok (signals (call-with-session-lock (make-memory-session-store) "sid"
                                         (lambda () :never))
                 'error))))


;;; ------------------------------------------------------------------
;;; Nesting
;;; ------------------------------------------------------------------
;;;
;;; Decided behaviour: the lock is RECURSIVE, so nesting is allowed and does
;;; not deadlock.  Two shapes of nesting have to work, because both are
;;; ordinary things to write: the macro inside itself (directly or through a
;;; function that uses it), and a store operation inside the body -- store
;;; operations take the very same per-session lock.

(deftest session-lock-nesting-test
  (testing "the macro nested in itself does not deadlock"
    (let ((store (make-memory-session-store))
          (result :unset))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf result
                                                  (with-session-lock (env)
                                                    (setf (gethash "outer" (getf env :lack.session)) 1)
                                                    (with-session-lock (env)
                                                      (setf (gethash "inner" (getf env :lack.session)) 2)
                                                      (with-session-lock (env)
                                                        :three-deep)))))))
        (let ((session-id (response-session-id (call-app app "/"))))
          (ok (eq result :three-deep) "all three levels ran")
          (let ((stored (fetch-stored-session store session-id)))
            (ok (eql (gethash "outer" stored) 1) "the outer write is there")
            (ok (eql (gethash "inner" stored) 2) "and so is the inner one"))))))

  (testing "a store operation inside the body does not deadlock either"
    ;; Store operations take the very same per-session lock, so a body that
    ;; reaches the store is the second shape of nesting that must not
    ;; deadlock.  The lock is taken through the wrapping store the stage
    ;; published in the environment -- the same object the macro locked.
    (let ((store (make-memory-session-store))
          (seen :unset))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (with-session-lock (env)
                                              (let ((wrapper (getf env +session-store-env-key+))
                                                    (sid (getf (getf env :lack.session.options) :id)))
                                                (setf (gethash "k" (getf env :lack.session)) "v")
                                                (let ((stored (fetch-session wrapper sid)))
                                                  (setf seen
                                                        (and (hash-table-p stored)
                                                             (gethash "k" stored)))))))))
        (let ((client (make-session-client app)))
          ;; the first request is the one that creates the session, so the
          ;; store has nothing to hand back yet
          (client-request client "/")
          (ok (null seen) "the store operation inside the body completed")
          ;; the second request finds the session in the store, so the value
          ;; really does come back through the locked store operation
          (client-request client "/")
          (ok (equal seen "v")
              "and read the session back through the store that holds the lock"))))))
