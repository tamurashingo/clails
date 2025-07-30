(in-package #:cl-user)
(defpackage #:clails-test/helper/date-helper
  (:use #:cl
        #:rove
        #:clails/helper/date-helper))

(in-package #:clails-test/helper/date-helper)

(deftest test-view/datetime
  (let ((ut (encode-universal-time 45 34 13 02 01 1998)))
    (ok (string= (view/datetime ut)
                 "1998/01/02 13:34:45"))
    (ok (string= (view/datetime ut :fmt "%Y")
                 "1998"))
    (ok (string= (view/datetime ut :fmt "%y")
                 "98"))

    (ok (string= (view/datetime ut :fmt "%Y%m%d%H%M%S")
                 "19980102133445"))

    (ok (string= (view/datetime ut :fmt "YMDHMS")
                 "YMDHMS"))

    (ok (string= (view/datetime ut :fmt "%%Y")
                 "%Y"))

    (ok (string= (view/datetime ut :fmt "%a")
                 "%a"))))


