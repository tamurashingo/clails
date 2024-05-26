(in-package #:cl-user)
(defpackage #:test.clails-model
  (:use #:cl
        #:rove
        #:clails-model))
(in-package #:test.clails-model)

(setup
 (format T "setup~%"))

(teardown
 (format T "teardown~%"))

(deftest parse-when
  (testing "parse operation"
    (ok (string= "ID = 1"
                 (parse-when '(= id 1))))
    (ok (string= "CREATED_AT < '2024/01/01 00:00:00'"
                 (parse-when '(< created-at "2024/01/01 00:00:00"))))

))
