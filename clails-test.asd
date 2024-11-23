(in-package #:cl-user)
(defpackage #:clails-test-system
  (:use #:asdf #:cl))
(in-package #:clails-test-system)

(defsystem clails-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on (#:babel
               #:clails
               #:rove
               #:clails-test/model/impl/sqlite3
               #:clails-test/model/impl/mysql
               #:clails-test/model/impl/postgresql)
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))

