(in-package #:cl-user)
(defpackage #:clails/periodic
  (:use #:cl)
  (:export #:periodic-waiter
           #:periodic-waiter-p
           #:make-periodic-waiter
           #:periodic-waiter-name
           #:wait-for-periodic-interval
           #:signal-periodic-waiter
           #:reset-periodic-waiter
           #:run-periodic-loop))
(in-package #:clails/periodic)


;;; ------------------------------------------------------------------
;;; Background work that has to happen every N seconds
;;; ------------------------------------------------------------------
;;;
;;; The mechanism behind the session sweeper (M5-T3), written as a general
;;; facility because the sweeper is not going to be the only periodic job in
;;; clails -- an expired-job reaper, a cache trimmer and a log rotator are all
;;; the same shape -- and because the one thing that is easy to get wrong
;;; should be got right once.
;;;
;;; That one thing is HOW THE LOOP WAITS.  The obvious loop is
;;;
;;;     (loop until *stop-requested* do (work) (sleep interval))
;;;
;;; and it has a defect that only shows up when you try to stop it: a thread
;;; inside SLEEP cannot notice that the flag changed, so "stop" means "finish
;;; sleeping first".  Setting the flag and then joining the thread blocks for
;;; up to a whole interval -- five minutes, for a sweeper that runs every five
;;; minutes -- which is five minutes added to every shutdown, and a shutdown
;;; hook that looks hung.
;;;
;;; So the wait is a CONDITION VARIABLE WITH A TIMEOUT instead.  Timing out is
;;; the normal case ("the interval elapsed, do the work again"); being
;;; signalled is the interesting one ("somebody wants us to stop, or to run
;;; now"), and it returns immediately.  Measured in the sweeper's tests, a
;;; stop that way completes in well under a second -- the cost of waking a
;;; thread -- instead of up to one interval.
;;;
;;; The rest of the shape follows the one background thread clails already
;;; had, the job worker (clails/job/worker): a special variable holding the
;;; thread, a special variable holding the stop flag, the flag checked at the
;;; top of each iteration, and "stop" meaning "set the flag, then join".  This
;;; file adds the waiter to that pattern and keeps the variables with the
;;; caller, because a caller that owns its own variables can be read (and
;;; stopped) without knowing about this file.
;;;
;;; Not built on the job queue, deliberately: see the note in
;;; clails/middleware/session/sweeper, which explains why the queue cannot
;;; host work of this kind at all.

(defstruct (periodic-waiter (:constructor %make-periodic-waiter))
  "The thing a periodic loop waits on between runs.

   Wraps a lock, a condition variable and a flag.  The flag is what makes the
   waiter correct rather than merely plausible: a signal that arrives between
   two waits must not be lost, so SIGNAL-PERIODIC-WAITER raises the flag and
   WAIT-FOR-PERIODIC-INTERVAL returns at once when it finds it already up.
   Without it, a stop request sent while the loop was doing its work would be
   dropped and the loop would wait out the full interval after all."
  (name "clails periodic waiter" :type string)
  (lock nil)
  (condition nil)
  (signalled nil :type boolean))

(defun make-periodic-waiter (&key (name "clails periodic waiter"))
  "Create a waiter for a periodic loop.

   @param name [string] Name for the lock and the condition variable, for
     thread listings and debuggers
   @return [periodic-waiter] A fresh waiter, not signalled
   "
  (%make-periodic-waiter
   :name name
   :lock (bt:make-lock name)
   :condition (bt:make-condition-variable :name name)))

(defun signal-periodic-waiter (waiter)
  "Wake WAITER now, and remember that it was woken.

   Called by whoever wants the loop to stop (or to run its work immediately
   rather than at the end of the current interval).  Safe to call when
   nothing is waiting: the flag keeps the wake-up until somebody collects it.

   @param waiter [periodic-waiter] The waiter to wake
   @return [t] T
   "
  (bt:with-lock-held ((periodic-waiter-lock waiter))
    (setf (periodic-waiter-signalled waiter) t)
    (bt:condition-notify (periodic-waiter-condition waiter)))
  t)

(defun reset-periodic-waiter (waiter)
  "Clear WAITER's remembered wake-up, so that the next wait really waits.

   Called when a loop is (re)started; a waiter left signalled by the previous
   stop would otherwise make the new loop's first wait return immediately.

   @param waiter [periodic-waiter] The waiter to reset
   @return [t] T
   "
  (bt:with-lock-held ((periodic-waiter-lock waiter))
    (setf (periodic-waiter-signalled waiter) nil))
  t)

(defun wait-for-periodic-interval (waiter seconds)
  "Wait up to SECONDS on WAITER, returning as soon as it is signalled.

   @param waiter [periodic-waiter] The waiter to wait on
   @param seconds [number] Maximum time to wait; zero or less does not wait
   @return [boolean] T when the wait ended because WAITER was signalled,
     NIL when SECONDS elapsed
   "
  (bt:with-lock-held ((periodic-waiter-lock waiter))
    (cond ((periodic-waiter-signalled waiter)
           (setf (periodic-waiter-signalled waiter) nil)
           t)
          ((<= seconds 0)
           nil)
          (t
           ;; CONDITION-WAIT may return without having been notified (a
           ;; spurious wake-up is allowed), so the flag -- not the return
           ;; value -- is what says why the wait ended.
           (bt:condition-wait (periodic-waiter-condition waiter)
                              (periodic-waiter-lock waiter)
                              :timeout seconds)
           (when (periodic-waiter-signalled waiter)
             (setf (periodic-waiter-signalled waiter) nil)
             t)))))

(defun run-periodic-loop (&key task interval waiter stop-requested-p
                               max-iterations on-error)
  "Run TASK every INTERVAL seconds until asked to stop.

   The loop body every periodic worker in clails shares:

     check the stop flag  ->  run the task  ->  check it again  ->  wait

   The stop flag is checked at the top of each iteration (so a stop requested
   while the task was running takes effect before the next one) and again
   before the wait (so a stop requested during the task is not followed by a
   pointless wait).  The wait itself ends the moment WAITER is signalled, so
   stopping takes about as long as waking a thread -- not up to INTERVAL.  The
   caller sets its flag and signals the waiter; see
   clails/middleware/session/sweeper for the canonical pair.

   An error in TASK must not kill a background thread that is supposed to run
   for the life of the process, so one is passed to ON-ERROR (or ignored when
   there is none) and the loop carries on.

   @param task [function] Called with no arguments, once per interval
   @param interval [number] Seconds between runs
   @param waiter [periodic-waiter] What the loop waits on between runs
   @param stop-requested-p [function] Called with no arguments; true stops
     the loop
   @param max-iterations [(or integer null)] Stop after this many runs; NIL
     (the default) runs until stopped.  Tests use it to keep a run bounded
   @param on-error [(or function null)] Called with the condition when TASK
     signals an error
   @return [integer] How many times TASK ran
   "
  (let ((runs 0))
    (loop
      (when (or (and max-iterations (>= runs max-iterations))
                (and stop-requested-p (funcall stop-requested-p)))
        (return))
      (handler-case
          (funcall task)
        (error (condition)
          (when on-error
            (funcall on-error condition))))
      (incf runs)
      (when (or (and max-iterations (>= runs max-iterations))
                (and stop-requested-p (funcall stop-requested-p)))
        (return))
      (wait-for-periodic-interval waiter interval))
    runs))
