(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-expiry
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/clock)
  (:import-from #:clails-test/support/session-store
                #:make-recording-session-store
                #:make-full-recording-session-store
                #:store-calls
                #:store-call-count
                #:clear-store-calls
                ;; The shared bodies, so that the in-memory derivation is
                ;; judged by exactly the assertions the database suites use.
                #:assert-idle-expiry-works
                #:assert-sweep-removes-idle-sessions)
  (:import-from #:clails/middleware/session/store
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/wrapper
                #:<session-store-wrapper>
                #:ensure-session-store-wrapper
                #:session-store-tracks-access-time-p
                #:session-store-wrapper-index-time
                #:+session-store-env-key+)
  (:import-from #:clails/middleware/session/expiry
                #:*session-idle-timeout*
                #:session-idle-expired-p
                #:session-expiry-reason
                #:+session-expiry-env-key+)
  (:import-from #:clails/middleware/session/lifecycle
                #:session-value
                #:session-table
                #:session-id
                #:session-id-changing-p))
(in-package #:clails-test/middleware/session-expiry)


;;; M5-T3: server-side expiry by idle time (G11) and the deletion that goes
;;; with it (G12, the lazy half; the periodic half is
;;; clails-test/middleware/session-sweeper).
;;;
;;; Nothing here waits for real time to pass.  The whole session layer reads
;;; the clock through one hook, and clails-test/support/clock freezes that
;;; hook together with its own, so "this session has been idle for 31
;;; minutes" is a frozen instant rather than a sleep -- see the reasoning in
;;; that file.


;;; ------------------------------------------------------------------
;;; The configuration (B5)
;;; ------------------------------------------------------------------

(deftest session-idle-timeout-setting-test
  (testing "the default idle limit is 30 minutes"
    (ok (eql *session-idle-timeout* 1800))
    (ok (eql *session-idle-timeout* (* 60 30))))

  (testing "it is re-exported the way B5 asks for"
    ;; Declared in a session-specific package
    ;; (clails/middleware/session/expiry), re-exported by the session
    ;; middleware package -- the one an application already configures -- and
    ;; re-exported again by the clails/middleware facade, which is how the
    ;; three session settings that existed before this milestone are reached.
    (ok (eq (symbol-value 'clails/middleware/session-middleware:*session-idle-timeout*)
            *session-idle-timeout*)
        "re-exported by the session middleware package")
    (let ((facade (find-package "CLAILS/MIDDLEWARE")))
      ;; Only loaded when the whole framework is (it is, in the full test
      ;; suite); a single-suite run does not pull it in.
      (if facade
          (dolist (name '("*SESSION-IDLE-TIMEOUT*"
                          "*SESSION-SWEEP-ENABLED*"
                          "*SESSION-SWEEP-INTERVAL*"
                          "*SESSION-COOKIE-SECURE*"))
            (multiple-value-bind (symbol status) (find-symbol name facade)
              (ok (and symbol (eq status :external))
                  (format nil "~A is external in clails/middleware" name))))
          (diag "clails/middleware is not loaded in this run; the facade check runs in the full suite~%"))))

  (testing "re-reading the configuration keeps the value the application set"
    ;; B5: declared with DEFVAR, so loading the framework again does not put
    ;; the default back over an application's setting.
    (let ((symbol 'clails/middleware/session/expiry::*session-idle-timeout*))
      (unwind-protect
           (progn
             (setf (symbol-value symbol) 60)
             (eval (list 'defvar symbol 1800))
             (ok (eql (symbol-value symbol) 60)))
        (setf (symbol-value symbol) 1800)))))


(deftest session-idle-expired-predicate-test
  (testing "the limit is exclusive: exactly at the limit is not expired"
    (let ((now 3000000000))
      (ok (not (session-idle-expired-p (- now 1800) 1800 now)))
      (ok (session-idle-expired-p (- now 1801) 1800 now))
      (ok (not (session-idle-expired-p now 1800 now)))))

  (testing "no limit means nothing expires"
    (let ((now 3000000000))
      (ok (not (session-idle-expired-p (- now (* 3600 24 365)) nil now)))))

  (testing "an unknown last-access time is not evidence of age"
    ;; A store that keeps no time says NIL, and a session must never be
    ;; expired on a guess.
    (let ((now 3000000000))
      (ok (not (session-idle-expired-p nil 1800 now)))))

  (testing "a last-access time in the future is not expired either"
    ;; Two processes sharing a table can disagree about the clock by a second.
    (let ((now 3000000000))
      (ok (not (session-idle-expired-p (+ now 60) 1800 now)))))

  (testing "the clock hook is what the predicate reads when no time is given"
    (let ((last-access 3000000000))
      (with-frozen-clock ((+ last-access 1801))
        (ok (session-idle-expired-p last-access 1800)))
      (with-frozen-clock ((+ last-access 1799))
        (ok (not (session-idle-expired-p last-access 1800)))))))


;;; ------------------------------------------------------------------
;;; Where the last-access time lives (decision 11)
;;; ------------------------------------------------------------------

(deftest session-last-access-time-is-store-side-test
  (testing "the time is not in the session value the application reads"
    (let ((store (make-memory-session-store))
          (seen-keys :unset))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf (session-value env "cart") 1)
                                            (setf seen-keys
                                                  (loop for key being the hash-keys
                                                          of (session-table env)
                                                        collect key)))))
        (let ((sid (response-session-id (call-app app "/"))))
          (ok (equal seen-keys (list "cart"))
              "the application sees its own key and nothing else")
          (let ((stored (fetch-stored-session store sid)))
            (ok (equal (loop for key being the hash-keys of stored collect key)
                       (list "cart"))
                "and the stored session holds no timestamp key either"))
          (ok (integerp (session-last-access-time store sid))
              "while the store does know when the session was last used")))))

  (testing "the time is updated on every request"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (opened-at (clock-now))
               (sid (progn (client-request client "/")
                           (set-cookie-value (session-client-last-set-cookie client)))))
          (dolist (offset '(10 600 1200))
            (with-clock-advanced-by (offset :from opened-at)
              (client-request client "/")
              (ok (eql (session-last-access-time store sid) (+ opened-at offset))
                  (format nil "the request at +~Ds moved the time to that instant"
                          offset))))))))

  (testing "the fourth operation records the time without writing the value"
    ;; Decision 11's goal, measured the way the plan prescribes: with every
    ;; operation recorded, recording an access must call the FOURTH operation
    ;; and must NOT call the value write.
    (let ((store (make-full-recording-session-store))
          (session (make-hash-table :test 'equal)))
      (setf (gethash "k" session) "v")
      (store-session store "sid" session)
      (clear-store-calls store)
      (ok (eql (touch-session store "sid" 3000000000) 3000000000))
      (ok (equal (mapcar #'car (store-calls store)) (list :touch))
          "the touch is the only operation that happened")
      (ok (zerop (store-call-count store :store))
          "no value write-back")
      (ok (zerop (store-call-count store :fetch))
          "and not even a read")
      (ok (eq (fetch-session store "sid") session)
          "the session value is the same object, unchanged")
      (ok (equal (gethash "k" session) "v"))))

  (testing "a store with the fourth operation has the touch forwarded to it"
    (let* ((inner (make-full-recording-session-store))
           (wrapper (ensure-session-store-wrapper inner))
           (session (make-hash-table :test 'equal)))
      (ok (session-store-tracks-access-time-p inner)
          "the wrapper can tell that this store implements the fourth operation")
      (store-session wrapper "sid" session)
      (clear-store-calls inner)
      (ok (eql (touch-session wrapper "sid" 3000000000) 3000000000))
      (ok (equal (mapcar #'car (store-calls inner)) (list :touch))
          "the inner store saw the touch, and nothing else")
      (ok (eql (session-last-access-time wrapper "sid") 3000000000))
      (ok (eql (session-last-access-time inner "sid") 3000000000)
          "the inner store is where the time was recorded")))

  (testing "a store WITHOUT it is not made to write the session back"
    ;; The naive base-class default would fetch and store on every request.
    ;; The wrapper therefore keeps the time in its own index instead -- which
    ;; is why a minimal third-party store costs nothing extra per request and
    ;; still expires correctly.
    (let* ((inner (make-recording-session-store))
           (wrapper (ensure-session-store-wrapper inner))
           (session (make-hash-table :test 'equal)))
      (ok (not (session-store-tracks-access-time-p inner)))
      (store-session wrapper "sid" session)
      (clear-store-calls inner)
      (ok (eql (touch-session wrapper "sid" 3000000000) 3000000000))
      (ok (null (store-calls inner))
          "the inner store saw no operation at all")
      (ok (eql (session-last-access-time wrapper "sid") 3000000000))
      (ok (eql (session-store-wrapper-index-time wrapper "sid") 3000000000)
          "the time is in the wrapper's index")))

  (testing "a continuing request writes the value table no more than before"
    ;; What the stage adds for the timestamp is one touch, not one write: the
    ;; value writes a continuing request performs are exactly the one the
    ;; session stage itself has always done.
    (let ((store (make-full-recording-session-store)))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env) (declare (ignore env)))))
        (let ((client (make-session-client app)))
          (client-request client "/")
          (clear-store-calls store)
          (client-request client "/")
          (let ((operations (mapcar #'car (store-calls store))))
            (ok (= (count :store operations) 1)
                (format nil "exactly one value write on a continuing request: ~S"
                        operations))
            (ok (= (count :touch operations) 1)
                "and the access was recorded with the fourth operation")))))))


(deftest session-expiry-works-without-the-fourth-operation-test
  ;; G2 must not be weakened: a store that implements only the three
  ;; operations of the abstraction still expires.
  (testing "a minimal three-operation store expires its sessions"
    (assert-idle-expiry-works (make-recording-session-store) "minimal test store"))

  (testing "and its sessions are swept"
    (assert-sweep-removes-idle-sessions (make-recording-session-store)
                                        "minimal test store")))


;;; ------------------------------------------------------------------
;;; The decision itself (G11)
;;; ------------------------------------------------------------------

(deftest session-idle-expiry-roundtrip-test
  (testing "the in-memory store expires, renames and drops an idle session"
    (assert-idle-expiry-works (make-memory-session-store) "clails memory store"))

  (testing "so does a lack store structure"
    (assert-idle-expiry-works (make-lack-memory-session-store) "lack memory store"))

  (testing "a store with the fourth operation does too"
    (assert-idle-expiry-works (make-full-recording-session-store)
                              "four-operation test store")))


(deftest session-expiry-happens-inside-the-stage-test
  ;; The decision is made one stage INSIDE the session stage, which is the
  ;; only place where the session's contents and its options are both
  ;; reachable.  Asserted by what the APPLICATION sees: an expired request
  ;; reaches the application with an empty session and a pending id change.
  (testing "the application is handed an empty session and a pending rename"
    (let ((store (make-memory-session-store))
          (seen '()))
      (with-middleware-stack
          (app :store store
               :inner (make-recording-app
                       (lambda (env)
                         (push (list :count (hash-table-count (session-table env))
                                     :id (session-id env)
                                     :changing (session-id-changing-p env)
                                     :reason (session-expiry-reason env))
                               seen))))
        (let* ((client (make-session-client app))
               (opened-at (clock-now)))
          (client-request client "/")
          (let ((first-id (set-cookie-value (session-client-last-set-cookie client))))
            ;; put something in the session so that "empty" is meaningful
            (with-middleware-stack
                (writer :store store
                        :inner (make-recording-app
                                (lambda (env) (setf (session-value env "a") 1))))
              (call-app writer "/" :cookie (session-client-cookie client)))
            (ok (eql (gethash "a" (fetch-stored-session store first-id)) 1))
            (with-clock-advanced-by ((+ *session-idle-timeout* 60) :from opened-at)
              (client-request client "/")
              (let ((record (first seen)))
                (ok (eql (getf record :count) 0)
                    "the application saw an empty session")
                (ok (string= (getf record :id) first-id)
                    "still under the old id while the action runs")
                (ok (getf record :changing)
                    "with the id marked to be replaced")
                (ok (eq (getf record :reason) :idle-timeout)
                    "and the reason recorded in the environment"))))))))

  (testing "the store is never asked to pretend a session is absent"
    ;; The rejected design.  If expiry were implemented by having the store
    ;; answer \"no such session\", the stage would open a fresh session UNDER
    ;; THE PRESENTED ID and a leaked id would stay valid for ever.  The
    ;; observable consequence of doing it properly is this: the expired
    ;; request still reads the session through the store (so the store was
    ;; asked, and answered truthfully), and the response carries a NEW id.
    (let ((store (make-full-recording-session-store)))
      (let ((*session-idle-timeout* 1800))
        (with-middleware-stack (app :store store :inner (make-session-counter-app))
          (let* ((client (make-session-client app))
                 (opened-at (clock-now))
                 (first-id (progn (client-request client "/")
                                  (set-cookie-value
                                   (session-client-last-set-cookie client)))))
            (clear-store-calls store)
            (with-clock-advanced-by ((+ 1800 60) :from opened-at)
              (client-request client "/")
              (let ((operations (mapcar #'car (store-calls store))))
                (ok (member :fetch operations)
                    (format nil "the store was read, not bypassed: ~S" operations))
                (ok (member :remove operations)
                    "and the expired session was removed from it"))
              (ok (string/= (set-cookie-value
                             (session-client-last-set-cookie client))
                            first-id)
                  "the presented id was not reused"))))))))


(deftest session-unresolvable-id-test
  ;; Expired, swept, forged, or left over from a restarted process: every id
  ;; that cannot be resolved arrives at the same door, and every one of them
  ;; is answered with a new id rather than with a session under the presented
  ;; one.
  (testing "an id no session exists for is replaced, not adopted"
    (let* ((store (make-memory-session-store))
           (stranger "0123456789abcdef0123456789abcdef01234567"))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((response (call-app app "/"
                                   :cookie (format nil "~A=~A"
                                                   +default-session-cookie-name+
                                                   stranger)))
               (cookie (response-set-cookie response +default-session-cookie-name+)))
          (ok cookie "the response issues a cookie")
          (ok (string/= (set-cookie-value cookie) stranger)
              "and the id in it is not the one that was presented")
          (ok (= (length (set-cookie-value cookie)) 40))
          (ok (string= (response-body-string response) "1")
              "the session is a fresh, empty one")
          (ok (null (fetch-stored-session store stranger))
              "and nothing was ever stored under the presented id")))))

  (testing "an id the sweeper removed behaves the same way"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (first-id (progn (client-request client "/")
                                (set-cookie-value
                                 (session-client-last-set-cookie client)))))
          (remove-stored-session store first-id)
          (let* ((response (client-request client "/"))
                 (cookie (session-client-last-set-cookie client)))
            (ok (string= (response-body-string response) "1"))
            (ok cookie "a cookie is issued")
            (ok (string/= (set-cookie-value cookie) first-id)
                "with a new id, not the one whose session had gone"))))))

  (testing "the reason is recorded as an unresolved id, not as a timeout"
    (let ((seen :unset))
      (with-middleware-stack (app :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen (session-expiry-reason env)))))
        (call-app app "/"
                  :cookie (format nil "~A=~A" +default-session-cookie-name+
                                  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
        (ok (eq seen :unknown-id)))))

  (testing "a malformed id never gets this far"
    ;; The state's validator rejects anything that is not 40 hexadecimal
    ;; characters, so the stage mints a fresh id before the expiry stage
    ;; runs: there is no presented id left to take away.
    (let ((seen :unset))
      (with-middleware-stack (app :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen (session-expiry-reason env)))))
        (call-app app "/" :cookie (format nil "~A=not-a-session-id"
                                          +default-session-cookie-name+))
        (ok (null seen)))))

  (testing "an ordinary first visit is not treated as an expiry"
    (let ((seen :unset))
      (with-middleware-stack (app :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen (session-expiry-reason env)))))
        (call-app app "/")
        (ok (null seen))))))


;;; ------------------------------------------------------------------
;;; Lazy deletion (G12, the access-time half)
;;; ------------------------------------------------------------------

(deftest session-lazy-deletion-test
  (testing "accessing an idle session removes it from the store there and then"
    (let ((store (make-memory-session-store))
          (removed-during-request :unset))
      (with-middleware-stack
          (app :store store
               :inner (make-recording-app
                       (lambda (env)
                         ;; Read from the INNER store, by the id the client
                         ;; presented: if lazy deletion works, the row is
                         ;; already gone while the application is still
                         ;; running -- long before the stage finalises the
                         ;; response and renames the session.
                         (setf removed-during-request
                               (null (fetch-stored-session
                                      store
                                      (session-id env)))))))
        (let* ((client (make-session-client app))
               (opened-at (clock-now))
               (first-id (progn (client-request client "/")
                                (set-cookie-value
                                 (session-client-last-set-cookie client)))))
          (ok (hash-table-p (fetch-stored-session store first-id)))
          (with-clock-advanced-by ((+ *session-idle-timeout* 60) :from opened-at)
            (client-request client "/")
            (ok (eq removed-during-request t)
                "the expired session was dropped on the way in, not on the way out")
            (ok (null (fetch-stored-session store first-id))
                "and it is still gone afterwards"))))))

  (testing "a live session is not removed"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (sid (progn (client-request client "/")
                           (set-cookie-value
                            (session-client-last-set-cookie client)))))
          (client-request client "/")
          (ok (hash-table-p (fetch-stored-session store sid))))))))


(deftest session-expiry-disabled-test
  (testing "with no idle limit a session survives any amount of idleness"
    (let ((*session-idle-timeout* nil)
          (store (make-memory-session-store)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (opened-at (clock-now))
               (sid (progn (client-request client "/")
                           (set-cookie-value
                            (session-client-last-set-cookie client)))))
          (with-clock-advanced-by ((* 3600 24 365) :from opened-at)
            (ok (string= (response-body-string (client-request client "/")) "2")
                "the session continues a year later")
            (ok (null (session-client-last-set-cookie client))
                "with no new cookie")
            (ok (hash-table-p (fetch-stored-session store sid)))))))))


(deftest session-expiry-is-deterministic-test
  ;; Ten consecutive runs of the whole scenario, because the suite has to
  ;; hold repeatedly and the clock is the thing that would make it flaky.
  (testing "the expiry verdict is the same on ten consecutive runs"
    (let ((verdicts '()))
      (dotimes (attempt 10)
        (declare (ignorable attempt))
        (let ((store (make-memory-session-store)))
          (with-middleware-stack (app :store store :inner (make-session-counter-app))
            (let* ((client (make-session-client app))
                   (opened-at (clock-now))
                   (first-id (progn (client-request client "/")
                                    (set-cookie-value
                                     (session-client-last-set-cookie client)))))
              (let ((inside
                      (with-clock-advanced-by ((1- *session-idle-timeout*) :from opened-at)
                        (response-body-string (client-request client "/"))))
                    (outside
                      (with-clock-advanced-by ((+ *session-idle-timeout* 60)
                                               :from (+ opened-at (1- *session-idle-timeout*)))
                        (list (response-body-string (client-request client "/"))
                              (and (string/= (set-cookie-value
                                              (session-client-last-set-cookie client))
                                             first-id)
                                   t)))))
                (push (list inside outside) verdicts))))))
      (ok (= (length verdicts) 10))
      (ok (every (lambda (verdict) (equal verdict '("2" ("1" t)))) verdicts)
          (format nil "every run: continued inside the limit, reset outside it; got ~S"
                  (remove-duplicates verdicts :test #'equal))))))
