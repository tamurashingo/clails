(in-package #:cl-user)
(defpackage #:clails/datetime/formatter
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>
                #:timestamp)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:format-datetime))

(in-package #:clails/datetime/formatter)

;;; Format Specifications

(defparameter *format-specs*
  '((:iso8601 . ((local-time:+iso-8601-format+)))
    (:iso8601-date . ((:year 4) #\- (:month 2) #\- (:day 2)))
    (:rfc3339 . ((local-time:+rfc3339-format+)))
    (:mysql . ((:year 4) #\- (:month 2) #\- (:day 2) #\Space
               (:hour 2) #\: (:min 2) #\: (:sec 2)))
    (:date-only . ((:year 4) #\- (:month 2) #\- (:day 2))))
  "Preset format specifications")

;;; Custom Format Processor

(defun process-custom-format (format-string)
  "Process custom format string into format spec list.
   
   @param format-string [string] Custom format string
   @return [list] Format specification list
   "
  (let ((specs '())
        (pos 0)
        (len (length format-string)))
    (loop while (< pos len)
          do (let ((remaining (subseq format-string pos)))
               (cond
                 ;; Year (yyyy)
                 ((and (>= (length remaining) 4)
                       (string= remaining "yyyy" :end1 4))
                  (push '(:year 4) specs)
                  (incf pos 4))
                 
                 ;; Month (mm or m)
                 ((and (>= (length remaining) 2)
                       (string= remaining "mm" :end1 2))
                  (push '(:month 2) specs)
                  (incf pos 2))
                 ((and (>= (length remaining) 1)
                       (char= (char remaining 0) #\m))
                  (push '(:month 0) specs)
                  (incf pos 1))
                 
                 ;; Day (dd or d)
                 ((and (>= (length remaining) 2)
                       (string= remaining "dd" :end1 2))
                  (push '(:day 2) specs)
                  (incf pos 2))
                 ((and (>= (length remaining) 1)
                       (char= (char remaining 0) #\d))
                  (push '(:day 0) specs)
                  (incf pos 1))
                 
                 ;; Hour (HH or H)
                 ((and (>= (length remaining) 2)
                       (string= remaining "HH" :end1 2))
                  (push '(:hour 2) specs)
                  (incf pos 2))
                 ((and (>= (length remaining) 1)
                       (char= (char remaining 0) #\H))
                  (push '(:hour 0) specs)
                  (incf pos 1))
                 
                 ;; Minute (MM or M)
                 ((and (>= (length remaining) 2)
                       (string= remaining "MM" :end1 2))
                  (push '(:min 2) specs)
                  (incf pos 2))
                 ((and (>= (length remaining) 1)
                       (char= (char remaining 0) #\M))
                  (push '(:min 0) specs)
                  (incf pos 1))
                 
                 ;; Second (SS or S)
                 ((and (>= (length remaining) 2)
                       (string= remaining "SS" :end1 2))
                  (push '(:sec 2) specs)
                  (incf pos 2))
                 ((and (>= (length remaining) 1)
                       (char= (char remaining 0) #\S))
                  (push '(:sec 0) specs)
                  (incf pos 1))
                 
                 ;; Literal character
                 (t
                  (push (char format-string pos) specs)
                  (incf pos 1)))))
    (nreverse specs)))

;;; Formatter

(defun format-with-spec (datetime spec timezone)
  "Format datetime according to specification.
   
   @param datetime [<datetime>] Datetime instance
   @param spec [list] Format specification
   @param timezone [local-time:timezone] Timezone
   @return [string] Formatted datetime string
   "
  (with-output-to-string (stream)
    (dolist (item spec)
      (cond
        ;; Local-time format constant
        ((and (consp item) (eq (length item) 1) (symbolp (car item)))
         (write-string (local-time:format-timestring nil (timestamp datetime)
                                                     :format (symbol-value (car item))
                                                     :timezone timezone)
                       stream))
        
        ;; Custom component
        ((consp item)
         (let ((component (first item))
               (width (second item)))
           (local-time:with-decoded-timestamp (:year year :month month :day day
                                                :hour hour :minute minute :sec sec)
               (timestamp datetime)
             (let ((value (ecase component
                            (:year year)
                            (:month month)
                            (:day day)
                            (:hour hour)
                            (:min minute)
                            (:sec sec))))
               (if (zerop width)
                   (format stream "~D" value)
                   (format stream "~V,'0D" width value))))))
        
        ;; Literal character
        (t
         (write-char item stream))))))

;;; Main Format Function

(defun format-datetime (datetime &key (format :iso8601) (timezone local-time:+utc-zone+))
  "Format a <datetime> as a string.
   
   @param datetime [<datetime>] Datetime instance to format
   @param format [keyword] Format specifier (default: :iso8601)
   @param format [string] Custom format pattern
   @param timezone [local-time:timezone] Timezone for output (default: UTC)
   @return [string] Formatted datetime string
   "
  (let ((spec (etypecase format
                (keyword (cdr (assoc format *format-specs*)))
                (string (process-custom-format format)))))
    (unless spec
      (error "Unknown format: ~A" format))
    (format-with-spec datetime spec timezone)))
