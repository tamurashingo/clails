(in-package #:cl-user)
(defpackage #:clails/datetime/utilities
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:import-from #:clails/datetime/constructors
                #:make-datetime)
  (:import-from #:clails/datetime/accessors
                #:year
                #:month
                #:day)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:beginning-of-day
           #:end-of-day
           #:beginning-of-month
           #:end-of-month
           #:days-in-month
           #:leap-year-p
           #:sunday-p
           #:monday-p
           #:tuesday-p
           #:wednesday-p
           #:thursday-p
           #:friday-p
           #:saturday-p))

(in-package #:clails/datetime/utilities)

;;; Day Boundaries

(defun beginning-of-day (datetime &key (timezone local-time:+utc-zone+))
  "Get the beginning of the day (00:00:00.000).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] Beginning of day
   "
  (make-datetime :year (year datetime :timezone timezone)
                 :month (month datetime :timezone timezone)
                 :day (day datetime :timezone timezone)
                 :hour 0 :minute 0 :second 0 :millisecond 0
                 :timezone timezone))

(defun end-of-day (datetime &key (timezone local-time:+utc-zone+))
  "Get the end of the day (23:59:59.999).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] End of day
   "
  (make-datetime :year (year datetime :timezone timezone)
                 :month (month datetime :timezone timezone)
                 :day (day datetime :timezone timezone)
                 :hour 23 :minute 59 :second 59 :millisecond 999
                 :timezone timezone))

;;; Month Boundaries

(defun beginning-of-month (datetime &key (timezone local-time:+utc-zone+))
  "Get the beginning of the month (1st day, 00:00:00.000).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] Beginning of month
   "
  (make-datetime :year (year datetime :timezone timezone)
                 :month (month datetime :timezone timezone)
                 :day 1
                 :hour 0 :minute 0 :second 0 :millisecond 0
                 :timezone timezone))

(defun end-of-month (datetime &key (timezone local-time:+utc-zone+))
  "Get the end of the month (last day, 23:59:59.999).
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [<datetime>] End of month
   "
  (let* ((y (year datetime :timezone timezone))
         (m (month datetime :timezone timezone))
         (last-day (days-in-month y m)))
    (make-datetime :year y
                   :month m
                   :day last-day
                   :hour 23 :minute 59 :second 59 :millisecond 999
                   :timezone timezone)))

;;; Date Information

(defun days-in-month (year month)
  "Get the number of days in a month.
   
   @param year [integer] Year
   @param month [integer] Month (1-12)
   @return [integer] Number of days
   "
  (local-time:days-in-month month year))

(defun leap-year-p (year)
  "Check if a year is a leap year.
   
   @param year [integer] Year
   @return [boolean] T if leap year, NIL otherwise
   "
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

;;; Day of Week Predicates

(defun sunday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Sunday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Sunday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 0))

(defun monday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Monday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Monday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 1))

(defun tuesday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Tuesday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Tuesday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 2))

(defun wednesday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Wednesday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Wednesday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 3))

(defun thursday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Thursday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Thursday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 4))

(defun friday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Friday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Friday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 5))

(defun saturday-p (datetime &key (timezone local-time:+utc-zone+))
  "Check if the datetime is Saturday.
   
   @param datetime [<datetime>] Datetime instance
   @param timezone [local-time:timezone] Timezone (default: UTC)
   @return [boolean] T if Saturday, NIL otherwise
   "
  (= (local-time:timestamp-day-of-week (timestamp datetime) :timezone timezone) 6))
