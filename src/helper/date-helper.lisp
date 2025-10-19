(in-package #:cl-user)
(defpackage #:clails/helper/date-helper
  (:use #:cl)
  (:export view/datetime))

(in-package #:clails/helper/date-helper)

(defun view/datetime (ut &key (fmt "%Y/%m/%d %H:%M:%S"))
  "Convert universal time to formatted string.
   
   Formats a universal time value according to the specified format string.
   Format specifiers:
   - %Y: 4-digit year (e.g., 1998)
   - %y: 2-digit year (e.g., 98)
   - %m: 2-digit month (01-12)
   - %d: 2-digit day (01-31)
   - %H: 2-digit hour (00-23)
   - %M: 2-digit minute (00-59)
   - %S: 2-digit second (00-59)
   - %%: Literal percent sign
   
   @param ut [integer] Universal time value
   @param ut [nil] Returns empty string if nil
   @param fmt [string] Format string (default: \"%Y/%m/%d %H:%M:%S\")
   @return [string] Formatted datetime string, or empty string if ut is nil
   "
  (if ut
      (with-output-to-string (s)
        (multiple-value-bind (sec min hour date mon year day daylight-p zone)
            (decode-universal-time ut)
          (declare (ignore day daylight-p zone))
          (loop for c across fmt
                with percent-p = NIL
                if (not percent-p)
                  if (char= c #\%)
                    do (setf percent-p T)
                  else
                    do (write-char c s)
                  end
                else
                  if (char= c #\Y)
                    do (write-sequence (write-to-string year) s)
                       (setf percent-p nil)
                  else if (char= c #\y)
                    do (write-sequence (write-to-string year) s :start 2)
                       (setf percent-p nil)
                  else if (and (char= c #\m)
                               (< mon 10))
                    do (write-char #\0 s)
                       (write-sequence (write-to-string mon) s)
                       (setf percent-p nil)
                  else if (char= c #\m)
                    do (write-sequence (write-to-string mon) s)
                       (setf percent-p nil)
                  else if (and (char= c #\d)
                           (< date 10))
                     do (write-char #\0 s)
                        (write-sequence (write-to-string date) s)
                        (setf percent-p nil)
                  else if (char= c #\d)
                       do (write-sequence (write-to-string date) s)
                          (setf percent-p nil)
                  else if (and (char= c #\H)
                               (< hour 10))
                    do (write-char #\0 s)
                       (write-sequence (write-to-string hour) s)
                       (setf percent-p nil)
                  else if (char= c #\H)
                    do (write-sequence (write-to-string hour) s)
                       (setf percent-p nil)
                  else if (and (char= c #\M)
                               (< min 10))
                    do (write-char #\0 s)
                       (write-sequence (write-to-string min) s)
                       (setf percent-p nil)
                  else if (char= c #\M)
                    do (write-sequence (write-to-string min) s)
                       (setf percent-p nil)
                  else if (and (char= c #\S)
                               (< sec 10))
                    do (write-char #\0 s)
                       (write-sequence (write-to-string sec) s)
                       (setf percent-p nil)
                  else if (char= c #\S)
                    do (write-sequence (write-to-string sec) s)
                       (setf percent-p nil)
                  else if (char= c #\%)
                    do (write-char c s)
                       (setf percent-p nil)
                  else
                    do (write-char #\% s)
                       (write-char c s)
                       (setf percent-p nil)
                  end
                end)))
      ""))






