(in-package #:cl-user)
(defpackage #:clails-test/support/clock
  (:use #:cl)
  ;; The framework's own clock hook.  Everything in the session layer reads
  ;; the time through it, so listing it in *CLOCK-VARIABLES* below is what
  ;; makes WITH-FROZEN-CLOCK freeze the framework and the test together.
  (:import-from #:clails/middleware/session/clock
                #:*session-clock*)
  ;; The real idle-timeout predicate, which REFERENCE-IDLE-EXPIRED-P now
  ;; delegates to (it used to be a stand-in of its own).
  (:import-from #:clails/middleware/session/expiry
                #:session-idle-expired-p)
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
;;; hook rather than calling GET-UNIVERSAL-TIME directly.  M5-T3 added that
;;; hook -- clails/middleware/session/clock:*SESSION-CLOCK*, which the expiry
;;; stage, the stores' timestamps and the periodic sweeper all read -- and it
;;; is listed in *CLOCK-VARIABLES* below, so one WITH-FROZEN-CLOCK now freezes
;;; the framework's notion of "now" together with the test's.
;;;
;;; How this file is extended: push a new clock variable's symbol onto
;;; *CLOCK-VARIABLES*.  WITH-FROZEN-CLOCK rebinds
;;; every variable listed there (via PROGV), so the framework's notion of
;;; "now" and the tests' notion of "now" move together without this file
;;; having to know the framework's variable name at compile time.

(defvar *clock* #'get-universal-time
  "Function of no arguments returning the current time as a universal time.")

(defvar *clock-variables*
  (list '*clock* 'clails/middleware/session/clock:*session-clock*)
  "Symbols of the special variables that WITH-FROZEN-CLOCK rebinds to the
   frozen clock function.

   Two of them: this file's own *CLOCK*, which CLOCK-NOW reads, and the
   framework's *SESSION-CLOCK*, which the expiry stage, the session stores'
   timestamps and the periodic sweeper read.  Freezing both together is what
   lets a test say \"this session has been idle for 31 minutes\" and have the
   framework agree.

   A future feature with a clock hook of its own is added by pushing its
   symbol here; nothing else in this file changes.")

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
  "Has a session last used at LAST-ACCESS-TIME been idle for longer than
   IDLE-TIMEOUT seconds?

   THE FRAMEWORK'S OWN PREDICATE, reached through this name.  It used to be a
   stand-in implemented here, because the feature did not exist; M5-T3 built
   it, so this is now a one-line delegation to
   clails/middleware/session/expiry:SESSION-IDLE-EXPIRED-P and the suite that
   calls it (clails-test/middleware/session-clock) asserts about the real
   thing.  New tests should call SESSION-IDLE-EXPIRED-P directly; this name
   is kept because it reads well in the clock suite, whose subject is the
   clock rather than the predicate."
  (session-idle-expired-p last-access-time idle-timeout (clock-now)))
