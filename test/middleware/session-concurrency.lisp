(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-concurrency
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:lack/request
                #:make-request
                #:request-query-parameters))
(in-package #:clails-test/middleware/session-concurrency)


;;; Measurement ground for M4: several threads hitting ONE session, with no
;;; database, no server and no network.
;;;
;;; These tests deliberately do NOT assert that concurrent updates are
;;; lossless.  They cannot: a controller action reads the session table and
;;; writes it back, and nothing serialises that today.  M4 is the milestone
;;; that fixes it, and M4 is where "50 + 50 increments make 100" becomes an
;;; assertion.  What M1 has to deliver is the ability to run the scenario and
;;; read the number out.


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
      (let ((final (and (hash-table-p session) (gethash "counter" session))))
        (ok (integerp final) "the final counter value can be read")
        (when (integerp final)
          ;; The session-opening request counted once as well, so 101 would be
          ;; the lossless result.  Only the range is asserted; M4-T2 is where
          ;; the exact value becomes a requirement.
          (ok (<= 1 final 101))
          (diag (format nil "final counter: ~D (lossless would be 101)~%" final))))
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


(deftest concurrent-session-bulk-write-test
  (testing "4 threads writing many keys into one session"
    (let* ((keys-per-request 20)
           (app (make-recording-app
                 (lambda (env)
                   (let* ((session (getf env :lack.session))
                          (req (make-request env))
                          (params (request-query-parameters req))
                          (writer (cdr (assoc "writer" params :test #'string=)))
                          (n (cdr (assoc "n" params :test #'string=))))
                     (dotimes (index keys-per-request)
                       (setf (gethash (format nil "w-~A-~A-~D" writer n index) session)
                             index))))))
           (result (run-concurrent-session-requests
                    app
                    :threads 4
                    :iterations 25
                    ;; the query string differs per request, so every request
                    ;; writes its own 20 keys
                    :uri (lambda (thread-index iteration-index)
                           (format nil "/?writer=~D&n=~D" thread-index iteration-index))))
           (responses (getf result :responses))
           (session (getf result :session)))
      (ok (null (getf result :timed-out)) "all worker threads finished")
      (ok (eql (getf result :abandoned) 0) "no worker thread had to be destroyed")
      (ok (= (+ (length responses) (length (getf result :errors))) 100)
          "every one of the 100 concurrent requests was accounted for")
      (ok (hash-table-p session) "the session is still a hash table")
      ;; Unlike the test above, this scenario tolerates requests that signal
      ;; -- that is what it measures -- and under the current implementation
      ;; it is normal for nearly all 100 of them to signal, so a non-empty
      ;; requirement here would be flaky.  The claim is therefore phrased as
      ;; a count: EVERY would silently report "all responses answered 200"
      ;; after examining none of them, whereas "0 of 0" cannot be misread as
      ;; 100 successes.
      (let ((answered-200 (count 200 responses :key #'response-status)))
        (ok (= answered-200 (length responses))
            (format nil "~D of the ~D request(s) that returned a response answered 200"
                    answered-200 (length responses))))
      ;; Both failure modes of the current implementation are reported here
      ;; rather than asserted away: requests may signal, and the container
      ;; itself may end up inconsistent.  Neither is a test failure yet --
      ;; M4-T1 is the task that turns these numbers into assertions -- what
      ;; M1 owes is that both are observable.
      (let ((errors (getf result :errors)))
        (diag (format nil "~D of 100 requests signalled~@[; first: ~A~]~%"
                      (length errors)
                      (when errors (cdr (first errors))))))
      ;; HASH-TABLE-COUNT below only makes sense on a hash table, and ROVE's
      ;; OK does not short-circuit the check above, so the guard is explicit.
      (if (hash-table-p session)
          (multiple-value-bind (sane reason) (hash-table-sane-p session)
            (if sane
                (diag (format nil "session container intact; ~D of ~D written keys survived~%"
                              (hash-table-count session)
                              (* 100 keys-per-request)))
                (diag (format nil "session container damaged: ~A~%" reason)))
            (ok (or sane reason)
                "the helper reaches a verdict on the session container"))
          (ok nil "there is a session container to reach a verdict on")))))
