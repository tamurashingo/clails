(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-concurrency
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:lack/request
                #:make-request
                #:request-query-parameters))
(in-package #:clails-test/middleware/session-concurrency)


;;; Several threads hitting ONE session, with no database, no server and no
;;; network.
;;;
;;; Built as a measurement ground in M1, when every one of these scenarios
;;; damaged the session: 98 of 100 concurrent requests signalled "Unsafe
;;; concurrent operations on #<HASH-TABLE ...> detected" and the container was
;;; left with more entries than it could iterate over.  M4 fixed that -- the
;;; session stage now runs the application on a container that tolerates
;;; concurrent access (clails/middleware/session/store/wrapper) -- so the
;;; numbers this file used to report with DIAG are assertions now: no request
;;; may signal, the container must stay intact, and every written key must
;;; still be there.
;;;
;;; What is still NOT asserted here is that concurrent updates are lossless.
;;; A read-modify-write in an application action is not atomic unless the
;;; action asks for it, so "50 + 50 increments make 100" belongs to the test
;;; of the public macro that provides atomicity
;;; (clails-test/middleware/session-lock), not here.  This file asserts the
;;; guarantee that holds unconditionally: the session survives.


(deftest concurrent-session-increment-test
  (testing "2 threads x 50 increments on one session: the final value is readable"
    (let* ((result (run-concurrent-session-requests (make-session-counter-app)
                                                    :threads 2
                                                    :iterations 50))
           (responses (getf result :responses))
           (session (getf result :session)))
      (ok (null (getf result :timed-out)) "all worker threads finished")
      (ok (eql (getf result :abandoned) 0) "no worker thread had to be destroyed")
      (ok (null (getf result :errors)) "no request signalled an error")
      (ok (= (length responses) 100) "100 concurrent requests were replayed")
      ;; EVERY is true of an empty list, so the non-empty check belongs in the
      ;; same assertion: without it, "all 100 requests failed" -- which does
      ;; happen under concurrent session writes -- would examine no response
      ;; at all and still report a pass.
      (ok (and responses
               (every (lambda (response) (= (response-status response) 200)) responses))
          (format nil "~D response(s) came back, all of them 200" (length responses)))
      (ok (= (length (getf result :session-id)) 40) "one session id for all of them")
      (ok (hash-table-p session) "the session survives as a hash table")
      ;; ROVE's OK does not short-circuit, so a failing check above does not
      ;; stop the ones below: every value has to be guarded where it is used,
      ;; or a missing session / missing key turns a readable assertion failure
      ;; into a TYPE-ERROR that aborts the rest of the test.
      (ok (null (getf result :errors)) "no request signalled, so nothing was lost that way")
      (let ((final (and (hash-table-p session) (gethash "counter" session))))
        (ok (integerp final) "the final counter value can be read")
        (when (integerp final)
          ;; The session-opening request counted once as well, so 101 is the
          ;; lossless result.  Only the range is asserted, deliberately: this
          ;; application action reads the counter and writes it back without
          ;; asking for atomicity, so an increment MAY be lost -- that is
          ;; specified behaviour, not a defect, and asserting either "101" or
          ;; "less than 101" here would be asserting a race.  The exact value
          ;; is a requirement where the action takes the session lock; see
          ;; clails-test/middleware/session-lock.
          (ok (<= 1 final 101))
          (diag (format nil "final counter without the lock: ~D (lossless would be 101)~%" final))))
      (if (hash-table-p session)
          (multiple-value-bind (sane reason) (hash-table-sane-p session)
            (ok sane (or reason "session container is intact")))
          (ok nil "there is a session container to check"))))

  (testing "repeating the scenario keeps producing a readable number"
    (dotimes (attempt 3)
      (let* ((result (run-concurrent-session-requests (make-session-counter-app)
                                                      :threads 2
                                                      :iterations 50))
             (session (getf result :session))
             (final (and (hash-table-p session) (gethash "counter" session))))
        (ok (integerp final)
            (format nil "attempt ~D produced a number: ~S" attempt final))))))


(defparameter +keys-per-request+ 20
  "How many keys one request of the bulk-write scenario writes.")

(defun bulk-write-key (writer n index)
  (format nil "w-~A-~A-~D" writer n index))

(defun bulk-write-app ()
  "An application that writes +KEYS-PER-REQUEST+ keys of its own into the
   session, named after the query parameters, so that every request of the
   concurrent scenario writes its own distinct set of keys."
  (make-recording-app
   (lambda (env)
     (let* ((session (getf env :lack.session))
            (req (make-request env))
            (params (request-query-parameters req))
            (writer (cdr (assoc "writer" params :test #'string=)))
            (n (cdr (assoc "n" params :test #'string=))))
       (dotimes (index +keys-per-request+)
         (setf (gethash (bulk-write-key writer n index) session) index))))))

(defun bulk-write-uri (thread-index iteration-index)
  (format nil "/?writer=~D&n=~D" thread-index iteration-index))

(defun expected-bulk-write-keys (threads iterations)
  "Every key the scenario writes, the session-opening request included.

   RUN-CONCURRENT-SESSION-REQUESTS opens the session with one sequential
   request, resolving the URI with thread and iteration -1, so that request
   writes a set of keys too."
  (let ((keys '()))
    (dotimes (index +keys-per-request+)
      (push (bulk-write-key "-1" "-1" index) keys))
    (dotimes (thread-index threads)
      (dotimes (iteration-index iterations)
        (dotimes (index +keys-per-request+)
          (push (bulk-write-key (princ-to-string thread-index)
                                (princ-to-string iteration-index)
                                index)
                keys))))
    keys))


(deftest concurrent-session-bulk-write-test
  (testing "4 threads writing many keys into one session"
    (let* ((threads 4)
           (iterations 25)
           (result (run-concurrent-session-requests
                    (bulk-write-app)
                    :threads threads
                    :iterations iterations
                    ;; the query string differs per request, so every request
                    ;; writes its own 20 keys
                    :uri #'bulk-write-uri))
           (responses (getf result :responses))
           (errors (getf result :errors))
           (session (getf result :session))
           (expected-keys (expected-bulk-write-keys threads iterations)))
      (ok (null (getf result :timed-out)) "all worker threads finished")
      (ok (eql (getf result :abandoned) 0) "no worker thread had to be destroyed")
      ;; This is the assertion M1 could not make: with 98 of 100 requests
      ;; signalling, the only honest thing to do was to report the number.
      (ok (null errors)
          (format nil "no request signalled~@[; first: ~A~]"
                  (when errors (cdr (first errors)))))
      (ok (= (length responses) (* threads iterations))
          (format nil "all ~D concurrent requests came back"
                  (* threads iterations)))
      (let ((answered-200 (count 200 responses :key #'response-status)))
        (ok (and responses (= answered-200 (length responses)))
            (format nil "~D of the ~D response(s) answered 200"
                    answered-200 (length responses))))
      (ok (hash-table-p session) "the session is still a hash table")
      ;; ROVE's OK does not short-circuit, so everything that would signal on
      ;; a non-table has to be guarded explicitly.
      (if (hash-table-p session)
          (progn
            ;; (i) the container is not damaged: iteration works, every
            ;; iterated key is reachable, and the count agrees with it
            (multiple-value-bind (sane reason) (hash-table-sane-p session)
              (ok sane (or reason "the session container is intact")))
            ;; (ii) nothing that was written went missing
            (let ((missing (count-if-not (lambda (key) (nth-value 1 (gethash key session)))
                                         expected-keys)))
              (ok (zerop missing)
                  (format nil "~D of the ~D written key(s) are missing"
                          missing (length expected-keys))))
            (ok (= (hash-table-count session) (length expected-keys))
                (format nil "the session holds ~D entries, ~D were written"
                        (hash-table-count session) (length expected-keys)))
            ;; (iii) the session is still usable afterwards
            (setf (gethash "after-the-storm" session) "ok")
            (ok (equal (gethash "after-the-storm" session) "ok")
                "the session can still be written and read after the scenario"))
          (ok nil "there is a session container to check")))))


;;; ------------------------------------------------------------------
;;; The exclusion is momentary, and it is per session
;;; ------------------------------------------------------------------
;;;
;;; Decision 10: the lock is taken for the instant of a store operation, never
;;; for a whole request.  Both tests below are rendezvous tests, because that
;;; is the only way to tell "we overlapped" from "we happened to be fast": the
;;; application does not answer until the OTHER request is inside it too.  If
;;; anything held a lock across the request, the two requests could not be
;;; inside the application at the same time and the rendezvous would time out.

(defparameter +rendezvous-timeout+ 5
  "Seconds a rendezvous request waits for its partner before giving up and
   reporting that it was alone.  A timeout rather than an endless wait, so a
   regression is a readable failure instead of a hung test run.")

(defun make-rendezvous-app (expected)
  "An application that answers \"overlapped\" only once EXPECTED requests are
   inside it at the same time, and \"alone\" when it waited in vain.

   A request carrying the query parameter \"solo\" skips the rendezvous and
   answers \"solo\" at once; that is for the sequential requests that merely
   open a session, which would otherwise each wait out the timeout.

   Each request also writes a key of its own into the session, so that the
   requests really are session writers and not mere readers."
  (let ((lock (bt:make-lock "clails-test rendezvous"))
        (arrived 0))
    (lambda (env)
      (let ((session (getf env :lack.session))
            (solo (assoc "solo" (request-query-parameters (make-request env))
                         :test #'string=))
            (mine (bt:with-lock-held (lock) (incf arrived))))
        (when (hash-table-p session)
          (setf (gethash (format nil "rendezvous-~D" mine) session) t))
        (if solo
            (list 200 (list :content-type "text/plain") (list "solo"))
            (let ((deadline (+ (get-universal-time) +rendezvous-timeout+))
                  (overlapped nil))
              (loop until (or (setf overlapped
                                    (>= (bt:with-lock-held (lock) arrived) expected))
                              (> (get-universal-time) deadline))
                    do (sleep 0.01))
              (list 200 (list :content-type "text/plain")
                    (list (if overlapped "overlapped" "alone")))))))))


(deftest concurrent-same-session-overlap-test
  (testing "two requests on ONE session are inside the application together"
    (let* ((result (run-concurrent-session-requests
                    (make-rendezvous-app 3)
                    :threads 2
                    :iterations 1
                    ;; the session-opening request does not take part in the
                    ;; rendezvous, but it does pass through the application,
                    ;; so the two concurrent requests are the 2nd and the 3rd
                    :uri (lambda (thread-index iteration-index)
                           (declare (ignore iteration-index))
                           (if (minusp thread-index) "/?solo=1" "/"))))
           (responses (getf result :responses))
           (bodies (mapcar #'response-body-string responses)))
      (ok (null (getf result :errors)) "neither request signalled")
      (ok (= (length responses) 2) "both requests came back")
      (ok (and bodies (every (lambda (body) (string= body "overlapped")) bodies))
          (format nil "both requests on one session overlapped: ~S" bodies)))))


(deftest concurrent-different-sessions-test
  (testing "requests on DIFFERENT sessions do not wait for each other"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-rendezvous-app 4))
        (let* ((client-a (make-session-client app))
               (client-b (make-session-client app))
               ;; The first session is opened WITHOUT the solo shortcut, so
               ;; this request waits for a partner that cannot come.  That is
               ;; the control case: it shows the rendezvous really can fail,
               ;; which is what makes the "overlapped" answers below mean
               ;; something.
               (setup-a (client-request client-a "/"))
               (setup-b (client-request client-b "/?solo=1"))
               (cookie-a (session-client-cookie client-a))
               (cookie-b (session-client-cookie client-b)))
          (ok (string= (response-body-string setup-a) "alone")
              "a request with no partner reports that it was alone")
          (ok (= (response-status setup-b) 200) "both sessions were opened")
          (ok (and cookie-a cookie-b (not (string= cookie-a cookie-b)))
              "the two clients hold two different sessions")
          (let* ((result (run-concurrently
                          (lambda (thread-index iteration-index)
                            (declare (ignore iteration-index))
                            (call-app app "/"
                                      :cookie (if (zerop thread-index) cookie-a cookie-b)))
                          :threads 2
                          :iterations 1
                          :timeout 30))
                 (responses (apply #'append (getf result :values)))
                 (bodies (mapcar #'response-body-string responses)))
            (ok (null (getf result :errors)) "neither request signalled")
            (ok (= (length responses) 2) "both requests came back")
            (ok (and bodies (every (lambda (body) (string= body "overlapped")) bodies))
                (format nil "the two sessions ran at the same time: ~S" bodies))))))))


;;; ------------------------------------------------------------------
;;; The contract of the concurrency helper itself
;;; ------------------------------------------------------------------
;;;
;;; The assertions above are only worth as much as RUN-CONCURRENTLY's promise
;;; that a failing worker turns up in :ERRORS.  A condition the worker loop
;;; does not catch kills the thread, and BT:JOIN-THREAD resignals it inside
;;; RUN-CONCURRENTLY -- so the test would die on the way out instead of
;;; reading a result, and "no request signalled" would never even be
;;; evaluated.  SERIOUS-CONDITION is the whole class that can do that.

(define-condition worker-trouble (serious-condition) ()
  (:documentation "A serious condition that is NOT an ERROR -- the shape of
   condition that used to escape RUN-CONCURRENTLY.  BT:TIMEOUT and
   STORAGE-CONDITION are real examples of it."))

(deftest run-concurrently-error-contract-test
  (testing "a worker signalling a non-ERROR serious condition is collected, not resignalled"
    (let* ((result (run-concurrently
                    (lambda (thread-index iteration-index)
                      (declare (ignore iteration-index))
                      (if (zerop thread-index)
                          (error 'worker-trouble)
                          :fine))
                    :threads 2
                    :iterations 1))
           (errors (getf result :errors)))
      (ok (= (length errors) 1) "the failing worker was collected")
      (ok (typep (cdr (first errors)) 'worker-trouble)
          "and the condition itself is readable")
      (ok (eql (car (first errors)) 0) "together with the thread that signalled it")
      (ok (equal (second (getf result :values)) (list :fine))
          "the other worker's results came back")
      (ok (null (getf result :timed-out)) "the wait ended normally")
      (ok (eql (getf result :abandoned) 0) "no thread had to be destroyed"))))
