(in-package #:cl-user)
(defpackage #:clials-test/project/project
  (:use #:cl
        #:rove
        #:clails/project/project))
(in-package #:clials-test/project/project)

(deftest test-1
  (assert (eql 1 1)))

(uiop:with-temporary-file (path)
  (with-open-file (stream path :direction :output)
    (format stream "test")))
