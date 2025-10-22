(in-package #:cl-user)
(defpackage #:clails/datetime/comparisons
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:export #:datetime=
           #:datetime<
           #:datetime<=
           #:datetime>
           #:datetime>=
           #:datetime-difference))

(in-package #:clails/datetime/comparisons)

;;; Comparison Operations

(defun datetime= (datetime1 datetime2)
  "Check if two datetimes are equal.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @return [boolean] T if equal, NIL otherwise
   "
  (local-time:timestamp= (timestamp datetime1) (timestamp datetime2)))

(defun datetime< (datetime1 datetime2)
  "Check if datetime1 is before datetime2.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @return [boolean] T if datetime1 < datetime2, NIL otherwise
   "
  (local-time:timestamp< (timestamp datetime1) (timestamp datetime2)))

(defun datetime<= (datetime1 datetime2)
  "Check if datetime1 is before or equal to datetime2.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @return [boolean] T if datetime1 <= datetime2, NIL otherwise
   "
  (local-time:timestamp<= (timestamp datetime1) (timestamp datetime2)))

(defun datetime> (datetime1 datetime2)
  "Check if datetime1 is after datetime2.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @return [boolean] T if datetime1 > datetime2, NIL otherwise
   "
  (local-time:timestamp> (timestamp datetime1) (timestamp datetime2)))

(defun datetime>= (datetime1 datetime2)
  "Check if datetime1 is after or equal to datetime2.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @return [boolean] T if datetime1 >= datetime2, NIL otherwise
   "
  (local-time:timestamp>= (timestamp datetime1) (timestamp datetime2)))

;;; Difference Calculation

(defun datetime-difference (datetime1 datetime2 &key (unit :seconds))
  "Calculate the difference between two datetimes.
   
   Returns the difference in the specified unit.
   
   @param datetime1 [<datetime>] First datetime
   @param datetime2 [<datetime>] Second datetime
   @param unit [keyword] Unit for result (:seconds, :minutes, :hours, :days)
   @return [integer] Difference in specified unit (datetime1 - datetime2)
   "
  (let ((diff (local-time:timestamp-difference (timestamp datetime1)
                                                (timestamp datetime2))))
    (ecase unit
      (:seconds diff)
      (:minutes (floor diff 60))
      (:hours (floor diff 3600))
      (:days (floor diff 86400)))))
