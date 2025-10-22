(in-package #:cl-user)
(defpackage #:clails-test/datetime/all
  (:use #:cl
        #:rove
        #:clails/datetime))

(in-package #:clails-test/datetime/all)

;;; Constructor Tests

(deftest test-make-datetime
  (testing "Creating datetime from components"
    (let ((dt (make-datetime :year 2024 :month 3 :day 15
                             :hour 14 :minute 30 :second 45 :millisecond 123)))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 30))
      (ok (= (sec dt) 45))
      (ok (= (millisecond dt) 123)))))

(deftest test-datetime-type
  (testing "Datetime type checking"
    (let ((dt (now)))
      (ok (datetime-p dt))
      (ok (not (datetime-p "not a datetime"))))))

;;; Parser Tests

(deftest test-parse-iso8601
  (testing "Parsing ISO 8601 format"
    (let ((dt (parse "2024-03-15T14:30:45")))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 30))
      (ok (= (sec dt) 45)))))

(deftest test-parse-mysql
  (testing "Parsing MySQL format"
    (let ((dt (parse "2024-03-15 14:30:45" :format :mysql)))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 30))
      (ok (= (sec dt) 45)))))

(deftest test-parse-custom-format
  (testing "Parsing custom formats"
    ;; Slash-separated with fixed width
    (let ((dt (parse "2025/01/02 12:34:56" :format "yyyy/mm/dd HH:MM:SS")))
      (ok (= (year dt) 2025))
      (ok (= (month dt) 1))
      (ok (= (day dt) 2))
      (ok (= (hour dt) 12))
      (ok (= (minute dt) 34))
      (ok (= (sec dt) 56)))
    
    ;; Japanese format with flexible width
    (let ((dt (parse "2024年3月15日 14時30分" :format "yyyy年m月d日 H時M分")))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 30))
      (ok (= (sec dt) 0)))
    
    ;; European format (day/month/year)
    (let ((dt (parse "15/3/2024" :format "d/m/yyyy")))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15)))
    
    ;; Single-digit month and day
    (let ((dt (parse "2024-3-5" :format "yyyy-m-d")))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 5)))))

;;; Formatter Tests

(deftest test-format-datetime
  (testing "Formatting datetime to string"
    (let ((dt (make-datetime :year 2024 :month 3 :day 15
                             :hour 14 :minute 30 :second 45)))
      ;; MySQL format
      (ok (string= (format-datetime dt :format :mysql)
                   "2024-03-15 14:30:45"))
      
      ;; Date only
      (ok (string= (format-datetime dt :format :date-only)
                   "2024-03-15")))))

(deftest test-format-custom
  (testing "Formatting with custom format"
    (let ((dt (make-datetime :year 2024 :month 3 :day 5
                             :hour 9 :minute 8 :second 7)))
      ;; Fixed width
      (ok (string= (format-datetime dt :format "yyyy/mm/dd HH:MM:SS")
                   "2024/03/05 09:08:07"))
      
      ;; Flexible width
      (ok (string= (format-datetime dt :format "yyyy-m-d H:M:S")
                   "2024-3-5 9:8:7"))
      
      ;; Japanese format
      (ok (string= (format-datetime dt :format "yyyy年m月d日")
                   "2024年3月5日")))))

;;; Operation Tests

(deftest test-add-operations
  (testing "Adding days, months, years"
    (let ((dt (parse "2024-03-15T00:00:00")))
      ;; Add days
      (let ((dt2 (add-days dt 7)))
        (ok (= (day dt2) 22)))
      
      ;; Add months
      (let ((dt2 (add-months dt 1)))
        (ok (= (month dt2) 4)))
      
      ;; Add years
      (let ((dt2 (add-years dt 1)))
        (ok (= (year dt2) 2025)))
      
      ;; Subtract days
      (let ((dt2 (add-days dt -7)))
        (ok (= (day dt2) 8))))))

(deftest test-time-operations
  (testing "Adding hours, minutes, seconds"
    (let ((dt (parse "2024-03-15T12:30:45")))
      ;; Add hours
      (let ((dt2 (add-hours dt 2)))
        (ok (= (hour dt2) 14)))
      
      ;; Add minutes
      (let ((dt2 (add-minutes dt 30)))
        (ok (= (minute dt2) 0))
        (ok (= (hour dt2) 13)))
      
      ;; Add seconds
      (let ((dt2 (add-seconds dt 15)))
        (ok (= (sec dt2) 0))
        (ok (= (minute dt2) 31))))))

;;; Comparison Tests

(deftest test-comparisons
  (testing "Datetime comparisons"
    (let ((dt1 (parse "2024-03-15T14:30:45"))
          (dt2 (parse "2024-03-20T14:30:45"))
          (dt3 (parse "2024-03-15T14:30:45")))
      (ok (datetime< dt1 dt2))
      (ok (datetime<= dt1 dt2))
      (ok (datetime<= dt1 dt3))
      (ok (datetime> dt2 dt1))
      (ok (datetime>= dt2 dt1))
      (ok (datetime>= dt1 dt3))
      (ok (datetime= dt1 dt3))
      (ok (not (datetime= dt1 dt2))))))

(deftest test-datetime-difference
  (testing "Calculating datetime difference"
    (let ((dt1 (parse "2024-03-15T14:30:45"))
          (dt2 (parse "2024-03-20T14:30:45")))
      (ok (= (datetime-difference dt2 dt1 :unit :days) 5))
      (ok (= (datetime-difference dt2 dt1 :unit :hours) 120))
      (ok (= (datetime-difference dt2 dt1 :unit :minutes) 7200))
      (ok (= (datetime-difference dt2 dt1 :unit :seconds) 432000)))))

;;; Utility Tests

(deftest test-beginning-end-of-day
  (testing "Beginning and end of day"
    (let ((dt (parse "2024-03-15T14:30:45")))
      (let ((bod (beginning-of-day dt)))
        (ok (= (hour bod) 0))
        (ok (= (minute bod) 0))
        (ok (= (sec bod) 0))
        (ok (= (day bod) 15)))
      
      (let ((eod (end-of-day dt)))
        (ok (= (hour eod) 23))
        (ok (= (minute eod) 59))
        (ok (= (sec eod) 59))
        (ok (= (day eod) 15))))))

(deftest test-beginning-end-of-month
  (testing "Beginning and end of month"
    (let ((dt (parse "2024-03-15T14:30:45")))
      (let ((bom (beginning-of-month dt)))
        (ok (= (day bom) 1))
        (ok (= (hour bom) 0)))
      
      (let ((eom (end-of-month dt)))
        (ok (= (day eom) 31))
        (ok (= (hour eom) 23))))))

(deftest test-leap-year
  (testing "Leap year detection"
    (ok (leap-year-p 2024))
    (ok (not (leap-year-p 2023)))
    (ok (leap-year-p 2000))
    (ok (not (leap-year-p 1900)))))

(deftest test-days-in-month
  (testing "Days in month"
    (ok (= (days-in-month 2024 2) 29))  ; Leap year
    (ok (= (days-in-month 2023 2) 28))  ; Non-leap year
    (ok (= (days-in-month 2024 4) 30))
    (ok (= (days-in-month 2024 1) 31))))

(deftest test-day-of-week-predicates
  (testing "Day of week predicates"
    ;; 2024-03-15 is Friday
    (let ((dt (parse "2024-03-15T00:00:00")))
      (ok (not (sunday-p dt)))
      (ok (not (monday-p dt)))
      (ok (not (tuesday-p dt)))
      (ok (not (wednesday-p dt)))
      (ok (not (thursday-p dt)))
      (ok (friday-p dt))
      (ok (not (saturday-p dt))))
    
    ;; 2024-03-17 is Sunday
    (let ((dt (parse "2024-03-17T00:00:00")))
      (ok (sunday-p dt))
      (ok (not (monday-p dt))))
    
    ;; 2024-03-18 is Monday
    (let ((dt (parse "2024-03-18T00:00:00")))
      (ok (not (sunday-p dt)))
      (ok (monday-p dt)))
    
    ;; 2024-03-16 is Saturday
    (let ((dt (parse "2024-03-16T00:00:00")))
      (ok (not (friday-p dt)))
      (ok (saturday-p dt)))))

;;; Immutability Tests

(deftest test-immutability
  (testing "Operations return new instances"
    (let* ((dt1 (now))
           (dt2 (add-days dt1 1)))
      (ok (not (eq dt1 dt2)))
      (ok (datetime< dt1 dt2)))))

;;; Database Integration Tests

(deftest test-database-conversion
  (testing "Database string conversion"
    (let ((dt (make-datetime :year 2024 :month 3 :day 15
                             :hour 14 :minute 30 :second 45)))
      ;; To DB string
      (let ((db-str (datetime-to-db-string dt)))
        (ok (string= db-str "2024-03-15 14:30:45"))
        
        ;; From DB string
        (let ((dt2 (db-string-to-datetime db-str)))
          (ok (datetime= dt dt2)))))))

;;; Conversion Tests

(deftest test-unix-time-conversion
  (testing "Unix time conversion"
    (let ((unix-time 1710508245))  ; 2024-03-15T14:30:45Z
      (let ((dt (from-unix-time unix-time)))
        (ok (= (to-unix-time dt) unix-time))))))

(deftest test-universal-time-conversion
  (testing "Universal time conversion"
    (let ((ut (get-universal-time)))
      (let ((dt (from-universal-time ut)))
        (ok (= (to-universal-time dt) ut))))))

;;; Accessor Tests

(deftest test-decode-datetime
  (testing "Decoding datetime to components"
    (let ((dt (make-datetime :year 2024 :month 3 :day 15
                             :hour 14 :minute 30 :second 45 :millisecond 123)))
      (multiple-value-bind (year month day hour minute second millisecond dow)
          (decode-datetime dt)
        (ok (= year 2024))
        (ok (= month 3))
        (ok (= day 15))
        (ok (= hour 14))
        (ok (= minute 30))
        (ok (= second 45))
        (ok (= millisecond 123))
        (ok (numberp dow))))))
