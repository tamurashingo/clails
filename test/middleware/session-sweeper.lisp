(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-sweeper
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/clock)
  (:import-from #:clails-test/support/session-store
                #:make-recording-session-store
                #:make-full-recording-session-store
                #:assert-sweep-removes-idle-sessions)
  (:import-from #:clails/environment
                #:*startup-hooks*
                #:*shutdown-hooks*)
  (:import-from #:clails/periodic
                #:make-periodic-waiter
                #:wait-for-periodic-interval
                #:signal-periodic-waiter
                #:reset-periodic-waiter
                #:run-periodic-loop)
  (:import-from #:clails/middleware/session/store
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/wrapper
                #:ensure-session-store-wrapper
                #:session-store-wrapper-index-entries
                #:session-store-wrapper-indexed-session-ids
                #:session-store-wrapper-index-count
                #:session-store-wrapper-index-time)
  (:import-from #:clails/middleware/session/expiry
                #:*session-idle-timeout*)
  (:import-from #:clails/middleware/session/sweeper
                #:*session-sweep-enabled*
                #:*session-sweep-interval*
                #:*session-sweeper-thread*
                #:*session-sweeper-stop-requested*
                #:register-session-sweep-store
                #:forget-session-sweep-stores
                #:session-sweep-stores
                #:sweep-idle-sessions
                #:sweep-all-registered-stores
                #:run-session-sweeper-loop
                #:start-session-sweeper
                #:stop-session-sweeper
                #:session-sweeper-running-p))
(in-package #:clails-test/middleware/session-sweeper)


;;; M5-T3, the periodic half of G12: expired sessions are removed even when
;;; nobody ever comes back for them -- which is the normal case, because a
;;; user who abandons a session sends no further request and so never
;;; triggers the lazy deletion.
;;;
;;; Four things are asserted here, and the last two are about the MECHANISM
;;; rather than the policy, because that is where the defects live:
;;;
;;;   * a pass removes the idle sessions and only those;
;;;   * the ids come from the wrapping store's index, which is the only place
;;;     they can come from (neither store protocol can enumerate sessions);
;;;   * the thread is started and stopped by the framework's own hooks,
;;;     registered with the registration procedure so that they land AFTER
;;;     the framework's own entries;
;;;   * stopping takes well under a second even when the interval is minutes,
;;;     because the loop waits on a condition variable instead of sleeping.
;;;
;;; As everywhere in M5, no test waits for real time to pass -- except the
;;; two that measure how long STOPPING takes, where real elapsed time is the
;;; subject.

(defparameter +stop-latency-limit+ 1.0
  "How long STOP-SESSION-SWEEPER may take, in seconds.

   The condition says \"well under a second\"; this is the limit the
   assertions use, and the measured value is printed alongside so a
   regression is visible even when it still passes.")


(defun seconds-to-run (thunk)
  "Wall-clock seconds THUNK took, as a float."
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (/ (float (- (get-internal-real-time) start))
       internal-time-units-per-second)))

(defun hook-function (hook)
  "Resolve a hook as the framework's hook runner does: a function object is
   itself, a \"package:name\" string is looked up.

   Mirrors clails/cmd:call-startup-hooks, which is what makes the test below
   a test of the REGISTERED hooks rather than of the functions behind them.
   The resolution is duplicated here (rather than calling the runner) for one
   reason: the runner would also invoke the framework's other hooks, and
   starting the connection pool is not this suite's business."
  (etypecase hook
    (function hook)
    (string (let ((colon (position #\: hook)))
              (symbol-function
               (or (find-symbol (string-upcase (subseq hook (1+ colon)))
                                (string-upcase (subseq hook 0 colon)))
                   (error "No such hook function: ~A" hook)))))))

(defun call-sweeper-hooks (hooks)
  "Invoke the sweeper's own entries of HOOKS, the way the framework would."
  (dolist (hook hooks)
    (when (and (stringp hook)
               (search "session/sweeper" hook))
      (funcall (hook-function hook)))))

(defmacro with-no-registered-stores (&body body)
  "Run BODY with the sweeper's list of stores emptied and then restored, so
   that a suite that registers stores does not leave them behind for the
   next one (or for a sweeper thread the framework starts later)."
  `(let ((saved (session-sweep-stores)))
     (unwind-protect
          (progn (forget-session-sweep-stores) ,@body)
       (forget-session-sweep-stores)
       (dolist (store (reverse saved))
         (register-session-sweep-store store)))))


;;; ------------------------------------------------------------------
;;; The configuration (B5)
;;; ------------------------------------------------------------------

(deftest session-sweep-settings-test
  (testing "sweeping is on by default, every five minutes"
    (ok (eq *session-sweep-enabled* t))
    (ok (eql *session-sweep-interval* 300)))

  (testing "both are re-exported by the session middleware package"
    (ok (eq (symbol-value 'clails/middleware/session-middleware:*session-sweep-enabled*)
            *session-sweep-enabled*))
    (ok (eq (symbol-value 'clails/middleware/session-middleware:*session-sweep-interval*)
            *session-sweep-interval*)))

  (testing "re-reading the configuration keeps the values the application set"
    ;; B5: DEFVAR, not DEFPARAMETER.
    (dolist (entry (list (cons 'clails/middleware/session/sweeper::*session-sweep-enabled* t)
                         (cons 'clails/middleware/session/sweeper::*session-sweep-interval* 300)))
      (let ((symbol (car entry))
            (default (cdr entry)))
        (unwind-protect
             (progn
               (setf (symbol-value symbol) :set-by-the-application)
               (eval (list 'defvar symbol default))
               (ok (eq (symbol-value symbol) :set-by-the-application)
                   (format nil "~A survived the re-declaration" symbol)))
          (setf (symbol-value symbol) default))))))


;;; ------------------------------------------------------------------
;;; The index the sweeper reads
;;; ------------------------------------------------------------------

(deftest session-sweep-index-test
  (testing "the wrapping store indexes the sessions that go through it"
    (let* ((inner (make-memory-session-store))
           (store (ensure-session-store-wrapper inner)))
      (ok (zerop (session-store-wrapper-index-count store)) "the index starts empty")
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let ((first-id (response-session-id (call-app app "/")))
              (second-id (response-session-id (call-app app "/"))))
          (ok (equal (sort (session-store-wrapper-indexed-session-ids store) #'string<)
                     (sort (list first-id second-id) #'string<))
              "both sessions are in the index")
          (ok (integerp (session-store-wrapper-index-time store first-id))
              "each with a last-access time")
          (remove-session store first-id)
          (ok (equal (session-store-wrapper-indexed-session-ids store)
                     (list second-id))
              "and a removed session leaves it, so the index does not grow for ever")))))

  (testing "the index is a snapshot, so a sweep can walk it safely"
    (let* ((store (ensure-session-store-wrapper (make-memory-session-store)))
           (session (make-hash-table :test 'equal)))
      (store-session store "aaaa" session)
      (let ((entries (session-store-wrapper-index-entries store)))
        (remove-session store "aaaa")
        (ok (equal (mapcar #'car entries) (list "aaaa"))
            "the snapshot taken before the removal is unaffected by it")
        (ok (null (session-store-wrapper-indexed-session-ids store))))))

  (testing "a touch refreshes the index entry"
    (let ((store (ensure-session-store-wrapper (make-memory-session-store))))
      (store-session store "aaaa" (make-hash-table :test 'equal))
      (touch-session store "aaaa" 3000000000)
      (ok (eql (session-store-wrapper-index-time store "aaaa") 3000000000)))))


;;; ------------------------------------------------------------------
;;; One pass
;;; ------------------------------------------------------------------

(deftest session-sweep-pass-test
  (testing "a pass removes the idle sessions and leaves the live ones"
    (assert-sweep-removes-idle-sessions (make-memory-session-store)
                                        "clails memory store"))

  (testing "it works for a store with the fourth operation"
    (assert-sweep-removes-idle-sessions (make-full-recording-session-store)
                                        "four-operation test store"))

  (testing "and for a store without it, whose time the index keeps"
    (assert-sweep-removes-idle-sessions (make-recording-session-store)
                                        "minimal test store"))

  (testing "and for a lack store structure"
    (assert-sweep-removes-idle-sessions (make-lack-memory-session-store)
                                        "lack memory store"))

  (testing "with no idle limit nothing is swept"
    (let* ((inner (make-memory-session-store))
           (store (ensure-session-store-wrapper inner)))
      (with-middleware-stack (app :store store :inner (make-session-counter-app))
        (let ((sid (response-session-id (call-app app "/"))))
          (with-clock-advanced-by ((* 3600 24 365))
            (ok (null (sweep-idle-sessions store :idle-timeout nil))
                "the pass removes nothing at all")
            (ok (hash-table-p (fetch-stored-session inner sid))))))))

  (testing "a pass over an empty index does nothing and does not fail"
    (let ((store (ensure-session-store-wrapper (make-memory-session-store))))
      (ok (null (sweep-idle-sessions store)))))

  (testing "every registered store is swept by one call"
    (with-no-registered-stores
      (let* ((first-inner (make-memory-session-store))
             (second-inner (make-memory-session-store))
             (first-store (ensure-session-store-wrapper first-inner))
             (second-store (ensure-session-store-wrapper second-inner))
             (opened-at (clock-now))
             ids)
        (with-middleware-stack (app :store first-store :inner (make-session-counter-app))
          (push (response-session-id (call-app app "/")) ids))
        (with-middleware-stack (app :store second-store :inner (make-session-counter-app))
          (push (response-session-id (call-app app "/")) ids))
        (ok (= (length (session-sweep-stores)) 2)
            "the session stage registered both stores by itself")
        (with-clock-advanced-by ((+ *session-idle-timeout* 60) :from opened-at)
          (ok (= (sweep-all-registered-stores :now (clock-now)) 2)
              "one call removed one session from each store"))
        (ok (every #'null (list (fetch-stored-session first-inner (first ids))
                                (fetch-stored-session second-inner (second ids))))))))

  (testing "registering is idempotent and ignores a store it cannot walk"
    (with-no-registered-stores
      (let ((store (ensure-session-store-wrapper (make-memory-session-store))))
        (register-session-sweep-store store)
        (register-session-sweep-store store)
        (ok (= (length (session-sweep-stores)) 1))
        ;; a bare store knows no session ids, so there is nothing to walk;
        ;; its sessions are cleaned up by the lazy deletion alone
        (ok (null (register-session-sweep-store (make-memory-session-store))))
        (ok (= (length (session-sweep-stores)) 1))))))


;;; ------------------------------------------------------------------
;;; The reusable periodic loop (clails/periodic)
;;; ------------------------------------------------------------------
;;;
;;; The loop mechanics live in a package of their own so that the next
;;; periodic job in clails does not reinvent them, and they are asserted here
;;; as the general facility they are.

(deftest periodic-waiter-test
  (testing "a wait that is not signalled times out"
    (let ((waiter (make-periodic-waiter :name "test waiter")))
      (ok (null (wait-for-periodic-interval waiter 0.05))
          "and reports that it timed out rather than that it was woken")))

  (testing "a wait of zero seconds does not wait"
    (let ((waiter (make-periodic-waiter :name "test waiter")))
      (ok (< (seconds-to-run (lambda () (wait-for-periodic-interval waiter 0))) 0.1))))

  (testing "a signal sent BEFORE the wait is not lost"
    ;; The flag is what makes this true, and it is the whole point: a stop
    ;; request that arrives while the loop is doing its work must not be
    ;; dropped, or the loop waits out a full interval after all.
    (let ((waiter (make-periodic-waiter :name "test waiter")))
      (signal-periodic-waiter waiter)
      (let ((elapsed (seconds-to-run
                      (lambda ()
                        (ok (eq (wait-for-periodic-interval waiter 30) t))))))
        (ok (< elapsed 0.5)
            (format nil "the wait returned at once (~,3Fs), not after 30s" elapsed)))))

  (testing "a signal sent DURING the wait ends it"
    (let* ((waiter (make-periodic-waiter :name "test waiter"))
           (thread (bt:make-thread (lambda ()
                                     (sleep 0.05)
                                     (signal-periodic-waiter waiter)))))
      (let ((elapsed (seconds-to-run
                      (lambda ()
                        (ok (eq (wait-for-periodic-interval waiter 30) t))))))
        (bt:join-thread thread)
        (ok (< elapsed 1.0)
            (format nil "woken after ~,3Fs instead of waiting 30s" elapsed)))))

  (testing "the wake-up is consumed, so the next wait waits again"
    (let ((waiter (make-periodic-waiter :name "test waiter")))
      (signal-periodic-waiter waiter)
      (ok (eq (wait-for-periodic-interval waiter 30) t))
      (ok (null (wait-for-periodic-interval waiter 0.05)))))

  (testing "resetting clears a pending wake-up"
    (let ((waiter (make-periodic-waiter :name "test waiter")))
      (signal-periodic-waiter waiter)
      (reset-periodic-waiter waiter)
      (ok (null (wait-for-periodic-interval waiter 0.05))))))


(deftest periodic-loop-test
  (testing "the task runs once per iteration, up to max-iterations"
    (let ((runs 0)
          (waiter (make-periodic-waiter :name "test waiter")))
      (ok (= (run-periodic-loop :task (lambda () (incf runs))
                                :interval 0
                                :waiter waiter
                                :stop-requested-p (lambda () nil)
                                :max-iterations 3)
             3))
      (ok (= runs 3))))

  (testing "the stop flag is checked at the top of the loop"
    (let ((runs 0)
          (waiter (make-periodic-waiter :name "test waiter")))
      (ok (= (run-periodic-loop :task (lambda () (incf runs))
                                :interval 0
                                :waiter waiter
                                :stop-requested-p (lambda () t))
             0)
          "a loop asked to stop before it starts never runs the task")
      (ok (zerop runs))))

  (testing "a stop requested by the task itself ends the loop without waiting"
    (let* ((runs 0)
           (stop nil)
           (waiter (make-periodic-waiter :name "test waiter"))
           (elapsed (seconds-to-run
                     (lambda ()
                       (run-periodic-loop :task (lambda () (incf runs) (setf stop t))
                                          :interval 30
                                          :waiter waiter
                                          :stop-requested-p (lambda () stop))))))
      (ok (= runs 1))
      (ok (< elapsed 0.5)
          (format nil "the loop returned in ~,3Fs rather than waiting out the interval"
                  elapsed))))

  (testing "an error in the task does not end the loop"
    (let ((runs 0)
          (errors 0)
          (waiter (make-periodic-waiter :name "test waiter")))
      (run-periodic-loop :task (lambda () (incf runs) (error "boom"))
                         :interval 0
                         :waiter waiter
                         :stop-requested-p (lambda () nil)
                         :max-iterations 3
                         :on-error (lambda (condition)
                                     (declare (ignore condition))
                                     (incf errors)))
      (ok (= runs 3) "all three iterations ran")
      (ok (= errors 3) "and each error was reported instead of killing the loop"))))


;;; ------------------------------------------------------------------
;;; The thread, and the framework's hooks
;;; ------------------------------------------------------------------

(deftest session-sweeper-hook-registration-test
  (testing "the sweeper is registered as a startup and a shutdown hook"
    (ok (member "clails/middleware/session/sweeper:start-session-sweeper"
                *startup-hooks* :test #'equal))
    (ok (member "clails/middleware/session/sweeper:stop-session-sweeper"
                *shutdown-hooks* :test #'equal)))

  (testing "registered at the END of the lists, after the framework's own"
    ;; B1: PUSH would put the sweeper BEFORE the framework's default hooks,
    ;; i.e. before the connection pool the database session store needs.  The
    ;; registration procedures append, which is why the sweeper is last.
    (let ((startup-position
            (position "clails/middleware/session/sweeper:start-session-sweeper"
                      *startup-hooks* :test #'equal))
          (pool-position
            (position "clails/model/connection:startup-connection-pool"
                      *startup-hooks* :test #'equal)))
      (ok (and startup-position pool-position (> startup-position pool-position))
          (format nil "the sweeper hook (~A) comes after the connection pool (~A)"
                  startup-position pool-position)))
    (let ((shutdown-position
            (position "clails/middleware/session/sweeper:stop-session-sweeper"
                      *shutdown-hooks* :test #'equal))
          (pool-position
            (position "clails/model/connection:shutdown-connection-pool"
                      *shutdown-hooks* :test #'equal)))
      (ok (and shutdown-position pool-position (> shutdown-position pool-position)))))

  (testing "each hook is registered exactly once"
    ;; Re-loading the file must not register them twice.
    (ok (= (count "clails/middleware/session/sweeper:start-session-sweeper"
                  *startup-hooks* :test #'equal)
           1))
    (ok (= (count "clails/middleware/session/sweeper:stop-session-sweeper"
                  *shutdown-hooks* :test #'equal)
           1))))


(deftest session-sweeper-thread-test
  (testing "nothing is started when no store has been registered"
    ;; How a process without a session stage -- a task, the console -- ends
    ;; up with no sweeper thread at all.
    (with-no-registered-stores
      (unwind-protect
           (progn
             (ok (null (start-session-sweeper)))
             (ok (not (session-sweeper-running-p))))
        (stop-session-sweeper))))

  (testing "nothing is started when sweeping is switched off"
    (with-no-registered-stores
      (register-session-sweep-store
       (ensure-session-store-wrapper (make-memory-session-store)))
      (let ((*session-sweep-enabled* nil))
        (unwind-protect
             (progn
               (ok (null (start-session-sweeper)))
               (ok (not (session-sweeper-running-p))))
          (stop-session-sweeper)))))

  (testing "the thread starts, and the shape matches the job worker's"
    ;; Thread in a variable, stop flag in a variable, flag false while
    ;; running: the same four-part shape as clails/job/worker, which is the
    ;; one background thread clails already had.
    (with-no-registered-stores
      (register-session-sweep-store
       (ensure-session-store-wrapper (make-memory-session-store)))
      (unwind-protect
           (let ((*session-sweep-interval* 300))
             (ok (start-session-sweeper))
             (ok (session-sweeper-running-p))
             (ok (bt:thread-alive-p *session-sweeper-thread*))
             (ok (null *session-sweeper-stop-requested*))
             (ok (eq (start-session-sweeper) *session-sweeper-thread*)
                 "starting it twice does not start a second thread"))
        (stop-session-sweeper))
      (ok (not (session-sweeper-running-p)))
      (ok (null *session-sweeper-thread*))))

  (testing "stopping takes well under a second, even with a five minute interval"
    ;; The condition this test exists for (B1).  A loop built on SLEEP would
    ;; take up to one whole interval to notice the stop flag -- five minutes
    ;; added to every shutdown.  The loop waits on a condition variable with
    ;; a timeout instead, and STOP signals it, so the cost of stopping is the
    ;; cost of waking a thread.
    (with-no-registered-stores
      (register-session-sweep-store
       (ensure-session-store-wrapper (make-memory-session-store)))
      (let ((*session-sweep-interval* 300)
            (elapsed nil))
        (start-session-sweeper)
        (ok (session-sweeper-running-p))
        ;; give the thread time to finish its first pass and settle into the
        ;; wait, so that what is measured really is a wait being interrupted
        (sleep 0.1)
        (setf elapsed (seconds-to-run #'stop-session-sweeper))
        (diag (format nil "stopping the sweeper took ~,4F s (interval: ~Ds)~%"
                      elapsed *session-sweep-interval*))
        (ok (< elapsed +stop-latency-limit+)
            (format nil "the sweeper stopped in ~,4Fs, under the ~,1Fs limit"
                    elapsed +stop-latency-limit+))
        (ok (not (session-sweeper-running-p)) "and the thread is gone"))))

  (testing "the framework's own hook lists start it and stop it, promptly"
    ;; The condition as it is written: drive the registered STARTUP and
    ;; SHUTDOWN hooks and measure how long the join takes.  Nothing here
    ;; calls START-SESSION-SWEEPER or STOP-SESSION-SWEEPER by name -- the
    ;; functions are reached through *STARTUP-HOOKS* / *SHUTDOWN-HOOKS*,
    ;; which is how a running server reaches them.
    (with-no-registered-stores
      (register-session-sweep-store
       (ensure-session-store-wrapper (make-memory-session-store)))
      (let ((*session-sweep-interval* 300)
            (elapsed nil))
        (unwind-protect
             (progn
               (call-sweeper-hooks *startup-hooks*)
               (ok (session-sweeper-running-p)
                   "the startup hook started the sweeper thread")
               (sleep 0.1)
               (setf elapsed (seconds-to-run
                              (lambda () (call-sweeper-hooks *shutdown-hooks*))))
               (diag (format nil "the shutdown hook joined the sweeper in ~,4F s (interval: ~Ds)~%"
                             elapsed *session-sweep-interval*))
               (ok (not (session-sweeper-running-p))
                   "and the shutdown hook joined it")
               (ok (< elapsed +stop-latency-limit+)
                   (format nil "the join took ~,4Fs, under the ~,1Fs limit"
                           elapsed +stop-latency-limit+)))
          (stop-session-sweeper)))))

  (testing "stopping a sweeper that is not running is harmless"
    (ok (null (stop-session-sweeper)))
    (ok (not (session-sweeper-running-p))))

  (testing "the running thread really does sweep"
    (with-no-registered-stores
      (let* ((inner (make-memory-session-store))
             (store (ensure-session-store-wrapper inner)))
        (with-middleware-stack (app :store store :inner (make-session-counter-app))
          (let ((sid (response-session-id (call-app app "/"))))
            (ok (hash-table-p (fetch-stored-session inner sid)))
            ;; Make the session look ancient by moving its recorded time into
            ;; the past, rather than by moving the clock: the sweeper runs on
            ;; its own thread, where a dynamically bound clock would not be
            ;; visible.
            (touch-session store sid (- (get-universal-time)
                                        (* 10 *session-idle-timeout*)))
            (let ((*session-sweep-interval* 300))
              (unwind-protect
                   (progn
                     (ok (start-session-sweeper))
                     ;; the first pass happens immediately, before the first
                     ;; wait, so a short settle is enough
                     (loop repeat 100
                           until (null (fetch-stored-session inner sid))
                           do (sleep 0.01))
                     (ok (null (fetch-stored-session inner sid))
                         "the sweeper thread removed the idle session on its own"))
                (stop-session-sweeper)))))))))


(deftest session-sweeper-bounded-loop-test
  (testing "the loop can be run directly, bounded, for a test"
    (with-no-registered-stores
      (let ((*session-sweeper-stop-requested* nil))
        (ok (= (run-session-sweeper-loop :max-iterations 2 :interval 0) 2)))))

  (testing "the loop exits at once when the stop flag is already raised"
    (let ((*session-sweeper-stop-requested* t))
      (ok (zerop (run-session-sweeper-loop :max-iterations 5 :interval 300))))))
