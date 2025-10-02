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
               #:clails-test/util
               #:clails-test/model/impl/sqlite3
               #:clails-test/model/impl/mysql
               #:clails-test/model/impl/postgresql
               #:clails-test/model/connection
               #:clails-test/model/query
               #:clails-test/model/query/sqlite3
               #:clails-test/model/query/mysql
               #:clails-test/model/query/postgresql
               #:clails-test/controller/base-controller
               #:clails-test/helper/date-helper
               #:clails-test/model/join-query
               #:clails-test/model/save
               #:clails-test/model/optimistic-lock)
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))

