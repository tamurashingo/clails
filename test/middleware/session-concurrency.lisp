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
      (ok (null (getf result :errors)) "no request signalled an error")
      (ok (= (length responses) 100) "100 concurrent requests were replayed")
      (ok (every (lambda (response) (= (response-status response) 200)) responses))
      (ok (= (length (getf result :session-id)) 40) "one session id for all of them")
      (ok (hash-table-p session) "the session survives as a hash table")
      (let ((final (gethash "counter" session)))
        (ok (integerp final) "the final counter value can be read")
        ;; The session-opening request counted once as well, so 101 would be
        ;; the lossless result.  Only the range is asserted; M4-T2 is where
        ;; the exact value becomes a requirement.
        (ok (<= 1 final 101))
        (diag (format nil "final counter: ~D (lossless would be 101)~%" final)))
      (multiple-value-bind (sane reason) (hash-table-sane-p session)
        (ok sane (or reason "session container is intact")))))

  (testing "repeating the scenario keeps producing a readable number"
    (dotimes (attempt 3)
      (let* ((result (run-concurrent-session-requests (make-session-counter-app)
                                                      :threads 2
                                                      :iterations 50))
             (final (gethash "counter" (getf result :session))))
        (ok (integerp final)
            (format nil "attempt ~D produced a number: ~D" attempt final))))))


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
      (ok (= (+ (length responses) (length (getf result :errors))) 100)
          "every one of the 100 concurrent requests was accounted for")
      (ok (hash-table-p session) "the session is still a hash table")
      (ok (every (lambda (response) (= (response-status response) 200)) responses))
      ;; Both failure modes of the current implementation are reported here
      ;; rather than asserted away: requests may signal, and the container
      ;; itself may end up inconsistent.  Neither is a test failure yet --
      ;; M4-T1 is the task that turns these numbers into assertions -- what
      ;; M1 owes is that both are observable.
      (let ((errors (getf result :errors)))
        (diag (format nil "~D of 100 requests signalled~@[; first: ~A~]~%"
                      (length errors)
                      (when errors (cdr (first errors))))))
      (multiple-value-bind (sane reason) (hash-table-sane-p session)
        (if sane
            (diag (format nil "session container intact; ~D of ~D written keys survived~%"
                          (hash-table-count session)
                          (* 100 keys-per-request)))
            (diag (format nil "session container damaged: ~A~%" reason)))
        (ok (or sane reason)
            "the helper reaches a verdict on the session container")))))
