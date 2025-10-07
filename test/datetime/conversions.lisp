(in-package #:cl-user)
(defpackage #:clails-test/datetime/conversions
  (:use #:cl
        #:rove
        #:clails/datetime))

(in-package #:clails-test/datetime/conversions)

(deftest from-timestamp-test
  (testing "from-timestamp basic conversion"
    (let* ((lt-timestamp (local-time:encode-timestamp 0 30 45 14 15 3 2024 
                                                      :timezone local-time:+utc-zone+))
           (dt (from-timestamp lt-timestamp)))
      (ok (typep dt '<datetime>))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 45))
      (ok (= (sec dt) 30))
      (ok (= (millisecond dt) 0))))
  
  (testing "from-timestamp with milliseconds"
    (let* ((lt-timestamp (local-time:encode-timestamp 123000000 45 30 14 15 3 2024
                                                      :timezone local-time:+utc-zone+))
           (dt (from-timestamp lt-timestamp)))
      (ok (= (year dt) 2024))
      (ok (= (month dt) 3))
      (ok (= (day dt) 15))
      (ok (= (hour dt) 14))
      (ok (= (minute dt) 30))
      (ok (= (sec dt) 45))
      (ok (= (millisecond dt) 123)))))

(deftest to-timestamp-test
  (testing "to-timestamp basic conversion"
    (let* ((dt (make-datetime :year 2024 :month 3 :day 15
                              :hour 14 :minute 30 :second 45 :millisecond 123))
           (lt-timestamp (to-timestamp dt)))
      (ok (typep lt-timestamp 'local-time:timestamp))
      (ok (= (local-time:timestamp-year lt-timestamp) 2024))
      (ok (= (local-time:timestamp-month lt-timestamp) 3))
      (ok (= (local-time:timestamp-day lt-timestamp) 15))
      (ok (= (local-time:timestamp-hour lt-timestamp) 14))
      (ok (= (local-time:timestamp-minute lt-timestamp) 30))
      (ok (= (local-time:timestamp-second lt-timestamp) 45))
      (ok (= (local-time:timestamp-millisecond lt-timestamp) 123)))))

(deftest roundtrip-test
  (testing "roundtrip conversion datetime -> timestamp -> datetime"
    (let* ((dt1 (make-datetime :year 2024 :month 3 :day 15
                               :hour 14 :minute 30 :second 45 :millisecond 123))
           (lt (to-timestamp dt1))
           (dt2 (from-timestamp lt)))
      (ok (datetime= dt1 dt2))
      (ok (= (year dt1) (year dt2)))
      (ok (= (month dt1) (month dt2)))
      (ok (= (day dt1) (day dt2)))
      (ok (= (hour dt1) (hour dt2)))
      (ok (= (minute dt1) (minute dt2)))
      (ok (= (sec dt1) (sec dt2)))
      (ok (= (millisecond dt1) (millisecond dt2)))))
  
  (testing "roundtrip conversion timestamp -> datetime -> timestamp"
    (let* ((lt1 (local-time:encode-timestamp 123000000 45 30 14 15 3 2024
                                             :timezone local-time:+utc-zone+))
           (dt (from-timestamp lt1))
           (lt2 (to-timestamp dt)))
      (ok (= (local-time:timestamp-year lt1) (local-time:timestamp-year lt2)))
      (ok (= (local-time:timestamp-month lt1) (local-time:timestamp-month lt2)))
      (ok (= (local-time:timestamp-day lt1) (local-time:timestamp-day lt2)))
      (ok (= (local-time:timestamp-hour lt1) (local-time:timestamp-hour lt2)))
      (ok (= (local-time:timestamp-minute lt1) (local-time:timestamp-minute lt2)))
      (ok (= (local-time:timestamp-second lt1) (local-time:timestamp-second lt2)))
      (ok (= (local-time:timestamp-millisecond lt1) (local-time:timestamp-millisecond lt2))))))
