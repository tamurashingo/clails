(in-package #:cl-user)
(defpackage #:clails/datetime/database
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>)
  (:import-from #:clails/datetime/parser
                #:parse)
  (:import-from #:clails/datetime/formatter
                #:format-datetime)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:datetime-to-db-string
           #:db-string-to-datetime))

(in-package #:clails/datetime/database)

;;; Database Conversion Functions

(defun datetime-to-db-string (datetime &key (format :mysql))
  "Convert <datetime> to database string format.
   
   @param datetime [<datetime>] Datetime instance
   @param format [keyword] Database format (default: :mysql)
   @return [string] Formatted string for database storage
   "
  (format-datetime datetime :format format :timezone local-time:+utc-zone+))

(defun db-string-to-datetime (string &key (format :mysql))
  "Convert database string to <datetime> instance.
   
   @param string [string] Database datetime string
   @param format [keyword] Database format (default: :mysql)
   @return [<datetime>] Parsed datetime instance
   "
  (parse string :format format :timezone local-time:+utc-zone+))
