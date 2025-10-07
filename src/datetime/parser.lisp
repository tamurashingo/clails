(in-package #:cl-user)
(defpackage #:clails/datetime/parser
  (:use #:cl)
  (:import-from #:clails/datetime/core
                #:<datetime>)
  (:import-from #:clails/datetime/constructors
                #:from-timestamp
                #:make-datetime)
  (:import-from #:local-time
                #:+utc-zone+)
  (:export #:parse))

(in-package #:clails/datetime/parser)

;;; Helper Functions

(defun starts-with-p (string prefix)
  "Check if string starts with the specified prefix.
   
   @param string [string] String to check
   @param prefix [string] Prefix
   @return [boolean] T if starts with prefix
   "
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun extract-number (string pos width)
  "Extract a number from string at specified position.
   
   @param string [string] Input string
   @param pos [integer] Start position
   @param width [integer] Fixed width (number of digits)
   @param width [:flex] Flexible width (1-2 digits)
   @return [integer] Extracted number
   @return [integer] Next position
   @condition error Number extraction fails
   "
  (if (eq width :flex)
      ;; Flexible width (1-2 digits)
      (let* ((end-pos (min (+ pos 2) (length string)))
             (substr (subseq string pos end-pos))
             (digit-count (or (position-if-not #'digit-char-p substr) (length substr))))
        (when (zerop digit-count)
          (error "Expected number at position ~D" pos))
        (values (parse-integer (subseq string pos (+ pos digit-count)))
                (+ pos digit-count)))
      ;; Fixed width
      (let ((end-pos (+ pos width)))
        (when (> end-pos (length string))
          (error "Insufficient characters for ~D-digit number at position ~D" width pos))
        (let ((substr (subseq string pos end-pos)))
          (unless (every #'digit-char-p substr)
            (error "Expected ~D-digit number at position ~D, got '~A'" width pos substr))
          (values (parse-integer substr) end-pos)))))

;;; Tokenizer

(defun read-token (format-string pos)
  "Read a token from the format string at the specified position.
   
   Returns (token . advance) format.
   token is like (:year . 4) or (:literal . \"/\").
   
   @param format-string [string] Format string
   @param pos [integer] Current position
   @return [cons] Token
   @return [integer] Number of characters to advance
   "
  (let ((remaining (subseq format-string pos)))
    (cond
      ;; Year (yyyy)
      ((starts-with-p remaining "yyyy")
       (values '(:year . 4) 4))
      
      ;; Month (mm or m)
      ((starts-with-p remaining "mm")
       (values '(:month . 2) 2))
      ((starts-with-p remaining "m")
       (values '(:month . :flex) 1))
      
      ;; Day (dd or d)
      ((starts-with-p remaining "dd")
       (values '(:day . 2) 2))
      ((starts-with-p remaining "d")
       (values '(:day . :flex) 1))
      
      ;; Hour (HH or H)
      ((starts-with-p remaining "HH")
       (values '(:hour . 2) 2))
      ((starts-with-p remaining "H")
       (values '(:hour . :flex) 1))
      
      ;; Minute (MM or M)
      ((starts-with-p remaining "MM")
       (values '(:minute . 2) 2))
      ((starts-with-p remaining "M")
       (values '(:minute . :flex) 1))
      
      ;; Second (SS or S)
      ((starts-with-p remaining "SS")
       (values '(:second . 2) 2))
      ((starts-with-p remaining "S")
       (values '(:second . :flex) 1))
      
      ;; Literal character
      (t
       (values (cons :literal (char format-string pos)) 1)))))

(defun tokenize-format (format-string)
  "Convert format string into a list of tokens.
   
   @param format-string [string] Format string
   @return [list] List of tokens
   "
  (let ((tokens '())
        (pos 0)
        (len (length format-string)))
    (loop while (< pos len)
          do (multiple-value-bind (token advance)
                 (read-token format-string pos)
               (push token tokens)
               (incf pos advance)))
    (nreverse tokens)))

;;; Custom Format Parser

(defun parse-custom-format (string format-string timezone)
  "Parse datetime string with custom format.
   
   @param string [string] Datetime string to parse
   @param format-string [string] Custom format pattern
   @param timezone [local-time:timezone] Timezone
   @return [local-time:timestamp] Parsed timestamp
   @condition error Parsing fails
   "
  (let ((tokens (tokenize-format format-string))
        (pos 0)
        (year nil)
        (month nil)
        (day nil)
        (hour 0)
        (minute 0)
        (second 0))
    
    ;; Extract values for each token
    (dolist (token tokens)
      (ecase (car token)
        (:year
         (multiple-value-bind (value new-pos)
             (extract-number string pos 4)
           (setf year value
                 pos new-pos)))
        
        (:month
         (multiple-value-bind (value new-pos)
             (extract-number string pos (if (eq (cdr token) :flex) :flex 2))
           (setf month value
                 pos new-pos)))
        
        (:day
         (multiple-value-bind (value new-pos)
             (extract-number string pos (if (eq (cdr token) :flex) :flex 2))
           (setf day value
                 pos new-pos)))
        
        (:hour
         (multiple-value-bind (value new-pos)
             (extract-number string pos (if (eq (cdr token) :flex) :flex 2))
           (setf hour value
                 pos new-pos)))
        
        (:minute
         (multiple-value-bind (value new-pos)
             (extract-number string pos (if (eq (cdr token) :flex) :flex 2))
           (setf minute value
                 pos new-pos)))
        
        (:second
         (multiple-value-bind (value new-pos)
             (extract-number string pos (if (eq (cdr token) :flex) :flex 2))
           (setf second value
                 pos new-pos)))
        
        (:literal
         ;; Skip literal character
         (let ((literal (cdr token)))
           (unless (and (< pos (length string))
                       (char= (char string pos) literal))
             (error "Expected '~A' at position ~D" literal pos))
           (incf pos)))))
    
    ;; Validate required fields
    (unless (and year month day)
      (error "Year, month, and day are required"))
    
    ;; Create timestamp
    (local-time:encode-timestamp 0 second minute hour day month year
                                 :timezone timezone)))

;;; Preset Format Parser

(defun parse-preset-format (string format timezone)
  "Parse datetime string with preset format.
   
   @param string [string] Datetime string to parse
   @param format [keyword] Preset format
   @param timezone [local-time:timezone] Timezone
   @return [local-time:timestamp] Parsed timestamp
   @condition error Parsing fails
   "
  (ecase format
    (:iso8601
     ;; "2024-03-15T14:30:45" or "2024-03-15T14:30:45.123Z"
     (local-time:parse-timestring string))
    
    (:rfc3339
     ;; "2024-03-15T14:30:45+09:00"
     ;; Let local-time handle timezone offset
     (local-time:parse-timestring string))
    
    (:mysql
     ;; "2024-03-15 14:30:45"
     (parse-custom-format string "yyyy-mm-dd HH:MM:SS" timezone))
    
    (:date-only
     ;; "2024-03-15"
     (parse-custom-format string "yyyy-mm-dd" timezone))))

;;; Main Parse Function

(defun parse (string &key (format :iso8601) (timezone local-time:+utc-zone+))
  "Parse a datetime string into a <datetime> instance.
   
   @param string [string] Datetime string to parse
   @param format [keyword] Format specifier (default: :iso8601)
   @param format [string] Custom format pattern
   @param timezone [local-time:timezone] Timezone for parsing (default: UTC)
   @return [<datetime>] Parsed datetime instance
   @condition error Parsing fails
   "
  (let ((ts (etypecase format
              (keyword (parse-preset-format string format timezone))
              (string (parse-custom-format string format timezone)))))
    (from-timestamp ts)))
