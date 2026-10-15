(in-package #:cl-user)
(defpackage #:clails-test/support/clock
  (:use #:cl)
  (:export #:*clock*
           #:*clock-variables*
           #:clock-now
           #:with-frozen-clock
           #:with-clock-advanced-by
           #:idle-seconds
           #:reference-idle-expired-p))
(in-package #:clails-test/support/clock)


;;; Clock stubbing for the tests that will judge idle-timeout expiry (G11)
;;; and periodic sweeping (G12).
;;;
;;; Two options were available:
;;;
;;;   (a) time injection -- route "what time is it" through a rebindable
;;;       function and let a test move it,
;;;   (b) tiny deadlines -- configure a one- or two-second idle limit and
;;;       really sleep past it.
;;;
;;; Time injection (a) is what this file implements, for three reasons:
;;;
;;;   1. It costs no wall-clock time.  (b) would add at least one real
;;;        second per expiry assertion, and M5/M12 need several of them, on
;;;        top of a suite that currently runs in about 1m36s.
;;;   2. It is deterministic.  With (b) the result depends on scheduling and
;;;        on how long the request itself took, so a test that must hold over
;;;        ten consecutive runs would need slack on both sides of the limit.
;;;   3. It can express "idle for three weeks" as cheaply as "idle for two
;;;        seconds", which the sweeping tests need.
;;;
;;; The trade-off accepted: production code has to read the time through a
;;; hook rather than calling GET-UNIVERSAL-TIME directly.  Nothing in src/
;;; does that yet -- the expiry feature itself does not exist -- so this file
;;; stays deliberately small and only provides the mechanism.
;;;
;;; How M5-T3 extends it: when the framework gains its own clock hook, push
;;; that variable's symbol onto *CLOCK-VARIABLES*.  WITH-FROZEN-CLOCK rebinds
;;; every variable listed there (via PROGV), so the framework's notion of
;;; "now" and the tests' notion of "now" move together without this file
;;; having to know the framework's variable name at compile time.

(defvar *clock* #'get-universal-time
  "Function of no arguments returning the current time as a universal time.")

(defvar *clock-variables* '(*clock*)
  "Symbols of the special variables that WITH-FROZEN-CLOCK rebinds to the
   frozen clock function.

   Starts out holding only this file's own *CLOCK*.  M5-T3 adds the
   framework's clock hook here so that one WITH-FROZEN-CLOCK freezes both.")

(defun clock-now ()
  "The current time according to the (possibly frozen) clock."
  (funcall *clock*))

(defun call-with-frozen-clock (universal-time thunk)
  (let ((frozen (lambda () universal-time)))
    (progv *clock-variables*
           (make-list (length *clock-variables*) :initial-element frozen)
      (funcall thunk))))

(defmacro with-frozen-clock ((universal-time) &body body)
  "Run BODY with every clock in *CLOCK-VARIABLES* frozen at UNIVERSAL-TIME.
   Repeated calls to CLOCK-NOW inside BODY return exactly that value."
  `(call-with-frozen-clock ,universal-time (lambda () ,@body)))

(defmacro with-clock-advanced-by ((seconds &key (from nil from-supplied-p)) &body body)
  "Run BODY with the clocks frozen SECONDS after FROM, which defaults to the
   current time.

   This is how a test builds the state \"this session has been idle for
   longer than the limit\" without waiting.

   FROM is what makes the result exact.  Left out, the base is read here, so
   a test that read its own base a few forms earlier is comparing against a
   *different* reading of the real clock: when a second boundary falls
   between the two, \"idle for exactly N seconds\" comes out as N+1.  Passing
   the same base the test holds removes the real clock from the comparison
   altogether:

     (let ((last-access (clock-now)))
       (with-clock-advanced-by ((* 3600 24 21) :from last-access)
         (= (idle-seconds last-access) (* 3600 24 21))))   ; always true

   Nesting it inside WITH-FROZEN-CLOCK has the same effect, because CLOCK-NOW
   then answers the frozen value."
  (let ((base (gensym "BASE")))
    `(let ((,base ,(if from-supplied-p from '(clock-now))))
       (call-with-frozen-clock (+ ,base ,seconds) (lambda () ,@body)))))

(defun idle-seconds (last-access-time)
  "How long ago LAST-ACCESS-TIME was, according to the current clock."
  (- (clock-now) last-access-time))

(defun reference-idle-expired-p (last-access-time idle-timeout)
  "Stand-in predicate: has a session last touched at LAST-ACCESS-TIME been
   idle for longer than IDLE-TIMEOUT seconds?

   This exists only to exercise the clock from a test while the real feature
   does not exist yet.  M5-T3 must assert against the framework's own
   predicate instead of this one; the clock machinery above is the part meant
   to be reused."
  (and idle-timeout
       (> (idle-seconds last-access-time) idle-timeout)))
