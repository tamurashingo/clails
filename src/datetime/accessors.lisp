(in-package #:cl-user)
(defpackage #:clails/datetime/accessors
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:year
           #:month
           #:day
           #:hour
           #:minute
           #:sec
           #:millisecond
           #:day-of-week
           #:decode-datetime
           #:to-timestamp
           #:to-unix-time
           #:to-universal-time))

(in-package #:clails/datetime/accessors)

;;; Component Accessors

(defun year (datetime &key (timezone local-time:+utc-zone+))
  "Get the year component.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Year
   "
  (local-time:timestamp-year (timestamp datetime) :timezone timezone))

(defun month (datetime &key (timezone local-time:+utc-zone+))
  "Get the month component (1-12).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Month (1-12)
   "
  (local-time:timestamp-month (timestamp datetime) :timezone timezone))

(defun day (datetime &key (timezone local-time:+utc-zone+))
  "Get the day component (1-31).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Day (1-31)
   "
  (local-time:timestamp-day (timestamp datetime) :timezone timezone))

(defun hour (datetime &key (timezone local-time:+utc-zone+))
  "Get the hour component (0-23).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Hour (0-23)
   "
  (local-time:timestamp-hour (timestamp datetime) :timezone timezone))

(defun minute (datetime &key (timezone local-time:+utc-zone+))
  "Get the minute component (0-59).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Minute (0-59)
   "
  (local-time:timestamp-minute (timestamp datetime) :timezone timezone))

(defun sec (datetime &key (timezone local-time:+utc-zone+))
  "Get the second component (0-59).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Second (0-59)
   "
  (local-time:timestamp-second (timestamp datetime) :timezone timezone))

(defun millisecond (datetime)
  "Get the millisecond component (0-999).
   
   @param datetime [<datetime>] Datetime instance
   @return [integer] Millisecond (0-999)
   "
  (floor (local-time:timestamp-millisecond (timestamp datetime))))

(defun day-of-week (datetime &key (timezone local-time:+utc-zone+))
  "Get the day of week (0=Sunday, 1=Monday, ..., 6=Saturday).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [integer] Day of week (0-6)
   "
  (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone))

;;; Decoding

(defun decode-datetime (datetime &key (timezone local-time:+utc-zone+))
  "Decode <datetime> into individual components.
   
   Returns multiple values: year month day hour minute second millisecond day-of-week.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone for decoding (default: UTC)
   @return [integer] Year
   @return [integer] Month (1-12)
   @return [integer] Day (1-31)
   @return [integer] Hour (0-23)
   @return [integer] Minute (0-59)
   @return [integer] Second (0-59)
   @return [integer] Millisecond (0-999)
   @return [integer] Day of week (0=Sunday, 1=Monday, ..., 6=Saturday)
   "
  (let ((ts (timestamp datetime)))
    (local-time:with-decoded-timestamp (:year year :month month :day day
                                         :hour hour :minute minute :sec sec
                                         :nsec nsec :day-of-week dow)
        ts
      (values year month day hour minute sec (floor nsec 1000000) dow))))

;;; Conversion Functions

(defun to-timestamp (datetime)
  "Extract the underlying local-time:timestamp from <datetime>.
   
   @param datetime [<datetime>] Datetime instance
   @return [local-time:timestamp] Underlying timestamp
   "
  (timestamp datetime))

(defun to-unix-time (datetime)
  "Convert <datetime> to Unix timestamp (seconds since 1970-01-01 00:00:00 UTC).
   
   @param datetime [<datetime>] Datetime instance
   @return [integer] Unix timestamp in seconds
   "
  (local-time:timestamp-to-unix (timestamp datetime)))

(defun to-universal-time (datetime)
  "Convert <datetime> to Common Lisp universal-time.
   
   Universal-time is seconds since 1900-01-01 00:00:00.
   
   @param datetime [<datetime>] Datetime instance
   @return [integer] Universal-time value
   "
  (local-time:timestamp-to-universal (timestamp datetime)))
