(in-package #:cl-user)
(defpackage #:clails/helper/date-helper
  (:use #:cl)
  (:export view/datetime))

(in-package #:clails/helper/date-helper)

;; 1998/01/02 13:34:45
;; Y -> 1998
;; y -> 98
;; m -> 01
;; d -> 02
;; H -> 13
;; M -> 34
;; S -> 45

(defun view/datetime (ut &key (fmt "%Y/%m/%d %H:%M:%S"))
  "universal time to string"
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
            end))))






