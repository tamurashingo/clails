(in-package #:cl-user)
(defpackage #:clails/middleware/session/clock
  (:use #:cl)
  (:export #:*session-clock*
           #:session-clock-now
           #:session-idle-seconds))
(in-package #:clails/middleware/session/clock)


;;; ------------------------------------------------------------------
;;; "What time is it" for everything session related (M5-T3)
;;; ------------------------------------------------------------------
;;;
;;; Idle-timeout expiry and the periodic sweeper are the first features in
;;; clails whose behaviour is a function of the clock.  Both have to be
;;; testable, and a test that waits for real seconds to pass is neither cheap
;;; nor deterministic: "idle for 30 minutes" cannot be waited for at all, and
;;; even "idle for two seconds" makes the verdict depend on how long the
;;; request itself took.
;;;
;;; So every session-related reading of the clock goes through this one
;;; function.  A test rebinds *SESSION-CLOCK* to a function that answers a
;;; fixed instant and the whole session layer -- the expiry stage, the stores
;;; that stamp their rows, the sweeper -- then agrees on what "now" is.  The
;;; test-side mechanism that does the rebinding is
;;; clails-test/support/clock:WITH-FROZEN-CLOCK, which rebinds every variable
;;; listed in its *CLOCK-VARIABLES*; this variable's symbol is one of them.
;;;
;;; Why a variable holding a function rather than a variable holding a time:
;;; a frozen time would have to be distinguishable from "not frozen", and
;;; every caller would have to know the convention.  A function of no
;;; arguments has one meaning, the default is GET-UNIVERSAL-TIME itself, and
;;; rebinding it needs no cooperation from the callers.
;;;
;;; Scope: this is deliberately not a framework-wide clock.  The model layer
;;; stamps rows with GET-UNIVERSAL-TIME of its own and is not affected; only
;;; the session layer reads the time through here, which keeps the blast
;;; radius of a frozen clock inside the feature that needs freezing.

(defvar *session-clock* #'get-universal-time
  "Function of no arguments returning the current time as a universal time.

   Every session-related reading of the clock goes through it: the
   idle-timeout decision, the last-access time the stores record, and the
   periodic sweeper.  The default is GET-UNIVERSAL-TIME.

   Rebind it (never SETF it globally outside a test) to make the session
   layer believe a particular instant:

     (let ((clails/middleware/session/clock:*session-clock*
             (lambda () 3000000000)))
       ...)

   This is the hook clails-test/support/clock pushes onto its
   *CLOCK-VARIABLES*, so WITH-FROZEN-CLOCK / WITH-CLOCK-ADVANCED-BY move the
   framework's notion of \"now\" together with the test's own.")

(defun session-clock-now ()
  "The current time, as a universal time, according to *SESSION-CLOCK*.

   @return [integer] Universal time
   "
  (funcall *session-clock*))

(defun session-idle-seconds (last-access-time &optional (now (session-clock-now)))
  "How long ago LAST-ACCESS-TIME was, in seconds.

   Negative when LAST-ACCESS-TIME lies in the future, which can happen when
   two processes with slightly different clocks share a persistent store; a
   negative idle time simply never exceeds a (non-negative) limit, so such a
   session is treated as recently used rather than as expired.

   @param last-access-time [integer] Universal time of the last access
   @param now [integer] Universal time to measure against, defaulting to now
   @return [integer] Seconds elapsed
   "
  (- now last-access-time))
