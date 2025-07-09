(in-package #:cl-user)
(defpackage #:clails-test/util
  (:use #:cl
        #:rove
        #:clails/util))

(in-package #:clails-test/util)

(deftest plist-exists
  (ok (plist-exists '(:a 1 :b 2 :c :d :e nil) :a))
  (ok (plist-exists '(:a 1 :b 2 :c :d :e nil) :b))
  (ok (plist-exists '(:a 1 :b 2 :c :d :e nil) :c))
  (ok (plist-exists '(:a 1 :b 2 :c :d :e nil) :e))

  (ng (plist-exists '(:a 1 :b 2 :c :d :e nil) :d))
  (ng (plist-exists '(:a 1 :b 2 :c :d :e nil) :f)))

