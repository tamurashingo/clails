(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-clock
  (:use #:cl
        #:rove
        #:clails-test/support/clock))
(in-package #:clails-test/middleware/session-clock)


;;; Exercises the clock stubbing that M5 (idle-timeout expiry) and the sweeper
;;; tests will use.  The choice of time injection over tiny real deadlines,
;;; and the reasons for it, are written down in test/support/clock.lisp.


(defparameter +idle-timeout+ 1800
  "Idle limit, in seconds, used by these tests only.")

(deftest clock-stub-test
  (testing "by default the clock is the real one"
    (ok (<= (get-universal-time) (clock-now) (+ (get-universal-time) 1))))

  (testing "a frozen clock answers the same value every time"
    (with-frozen-clock (3000000000)
      (ok (= (clock-now) 3000000000))
      (ok (= (clock-now) 3000000000))
      (ok (= (idle-seconds 2999999000) 1000))))

  (testing "the clock is restored on the way out"
    (with-frozen-clock (3000000000)
      (ok (= (clock-now) 3000000000)))
    (ok (/= (clock-now) 3000000000)))

  (testing "frozen clocks nest"
    (with-frozen-clock (1000)
      (with-frozen-clock (2000)
        (ok (= (clock-now) 2000)))
      (ok (= (clock-now) 1000)))))


(deftest clock-idle-timeout-test
  ;; Every advance below is measured :FROM the base the test itself holds.
  ;; Reading the base once and advancing from that reading is what keeps the
  ;; real clock out of the comparison: without it, a second boundary falling
  ;; between the test's (CLOCK-NOW) and the macro's own would make "idle for
  ;; exactly 21 days" come out as 21 days plus one second -- real-clock
  ;; flakiness in the very file whose purpose is to remove the real clock.
  (testing "an over-the-limit idle state can be built without waiting"
    (let ((last-access (clock-now)))
      ;; just inside the limit
      (with-clock-advanced-by ((1- +idle-timeout+) :from last-access)
        (ok (not (reference-idle-expired-p last-access +idle-timeout+))))
      ;; just outside it
      (with-clock-advanced-by ((1+ +idle-timeout+) :from last-access)
        (ok (reference-idle-expired-p last-access +idle-timeout+)))
      ;; and arbitrarily far outside it, at no cost
      (with-clock-advanced-by ((* 3600 24 21) :from last-access)
        (ok (reference-idle-expired-p last-access +idle-timeout+))
        (ok (= (idle-seconds last-access) (* 3600 24 21))))))

  (testing "the same scenario gives the same answer on ten consecutive runs"
    (let ((verdicts '()))
      (dotimes (attempt 10)
        (declare (ignorable attempt))
        (let ((last-access (clock-now)))
          (push (list (with-clock-advanced-by ((1- +idle-timeout+) :from last-access)
                        (reference-idle-expired-p last-access +idle-timeout+))
                      (with-clock-advanced-by ((1+ +idle-timeout+) :from last-access)
                        (reference-idle-expired-p last-access +idle-timeout+)))
                verdicts)))
      (ok (= (length verdicts) 10))
      (ok (every (lambda (verdict) (equal verdict '(nil t))) verdicts))))

  (testing "no idle limit means nothing ever expires"
    (let ((last-access (clock-now)))
      (with-clock-advanced-by ((* 3600 24 365) :from last-access)
        (ok (not (reference-idle-expired-p last-access nil))))))

  (testing "the advance is exact to the second, and defaults to the current time"
    (let ((last-access (clock-now)))
      ;; :FROM makes the arithmetic exact, whatever the real clock does
      (dolist (seconds (list 0 1 59 3600 (* 3600 24 365)))
        (with-clock-advanced-by (seconds :from last-access)
          (ok (= (idle-seconds last-access) seconds)
              (format nil "advanced exactly ~D second(s)" seconds))))
      ;; without :FROM the base is read inside the macro, so the advance is
      ;; relative to "now" and only bounded by it
      (with-clock-advanced-by (100)
        (ok (<= 100 (idle-seconds last-access) 101))))))
