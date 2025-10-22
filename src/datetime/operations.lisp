(in-package #:cl-user)
(defpackage #:clails/datetime/operations
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:import-from #:clails/datetime/constructors
                #:from-timestamp)
  (:export #:add-years
           #:add-months
           #:add-days
           #:add-hours
           #:add-minutes
           #:add-seconds
           #:add-milliseconds))

(in-package #:clails/datetime/operations)

;;; Addition Operations

(defun add-years (datetime years)
  "Add years to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param years [integer] Number of years to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       years :year)))
    (from-timestamp new-ts)))

(defun add-months (datetime months)
  "Add months to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param months [integer] Number of months to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       months :month)))
    (from-timestamp new-ts)))

(defun add-days (datetime days)
  "Add days to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param days [integer] Number of days to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       days :day)))
    (from-timestamp new-ts)))

(defun add-hours (datetime hours)
  "Add hours to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param hours [integer] Number of hours to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       hours :hour)))
    (from-timestamp new-ts)))

(defun add-minutes (datetime minutes)
  "Add minutes to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param minutes [integer] Number of minutes to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       minutes :minute)))
    (from-timestamp new-ts)))

(defun add-seconds (datetime seconds)
  "Add seconds to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param seconds [integer] Number of seconds to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let ((new-ts (local-time:timestamp+ (timestamp datetime)
                                       seconds :sec)))
    (from-timestamp new-ts)))

(defun add-milliseconds (datetime milliseconds)
  "Add milliseconds to datetime (returns new instance).
   
   @param datetime [<datetime>] Datetime instance
   @param milliseconds [integer] Number of milliseconds to add (negative to subtract)
   @return [<datetime>] New datetime instance
   "
  (let* ((nsec (* milliseconds 1000000))
         (new-ts (local-time:timestamp+ (timestamp datetime)
                                        nsec :nsec)))
    (from-timestamp new-ts)))
