(in-package #:cl-user)
(defpackage #:clails/datetime/constructors
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:from-timestamp
           #:make-datetime
           #:now
           #:today
           #:from-unix-time
           #:from-universal-time))

(in-package #:clails/datetime/constructors)

;;; Basic Constructor

(defun from-timestamp (timestamp)
  "Create a <datetime> from local-time:timestamp.
   
   @param timestamp [local-time:timestamp] Local-time timestamp instance
   @return [<datetime>] New datetime instance
   "
  (make-instance '<datetime> :timestamp timestamp))

;;; Component-based Constructor

(defun make-datetime (&key year month day
                           (hour 0) (minute 0) (second 0) (millisecond 0)
                           (timezone local-time:+utc-zone+))
  "Create a <datetime> from individual components.
   
   @param year [integer] Year
   @param month [integer] Month (1-12)
   @param day [integer] Day (1-31)
   @param hour [integer] Hour (0-23, default: 0)
   @param minute [integer] Minute (0-59, default: 0)
   @param second [integer] Second (0-59, default: 0)
   @param millisecond [integer] Millisecond (0-999, default: 0)
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] New datetime instance
   @condition error Invalid date/time components
   "
  (let ((nsec (* millisecond 1000000)))  ; Convert milliseconds to nanoseconds
    (from-timestamp
     (local-time:encode-timestamp nsec second minute hour day month year
                                  :timezone timezone))))

;;; Current Time Constructors

(defun now (&key (timezone local-time:+utc-zone+))
  "Get the current datetime.
   
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] Current datetime
   "
  (from-timestamp (local-time:now)))

(defun today (&key (timezone local-time:+utc-zone+))
  "Get today's date at midnight (00:00:00.000).
   
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] Today at midnight
   "
  (from-timestamp (local-time:today :timezone timezone)))

;;; Unix Time Constructor

(defun from-unix-time (unix-time &key (timezone local-time:+utc-zone+))
  "Create a <datetime> from Unix timestamp (seconds since 1970-01-01 00:00:00 UTC).
   
   @param unix-time [integer] Unix timestamp in seconds
   @param unix-time [real] Unix timestamp with fractional seconds
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] New datetime instance
   "
  (from-timestamp (local-time:unix-to-timestamp unix-time)))

;;; Universal Time Constructor

(defun from-universal-time (universal-time &key (timezone local-time:+utc-zone+))
  "Create a <datetime> from Common Lisp universal-time.
   
   Universal-time is seconds since 1900-01-01 00:00:00.
   
   @param universal-time [integer] Universal-time value
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] New datetime instance
   "
  (from-timestamp (local-time:universal-to-timestamp universal-time)))
