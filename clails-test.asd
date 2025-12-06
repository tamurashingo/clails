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
               #:clails-test/model/optimistic-lock
               #:clails-test/model/default-value
               #:clails-test/model/migration
               #:clails-test/model/transaction
               #:clails-test/model/transaction/transaction-sqlite3
               #:clails-test/model/transaction/transaction-mysql
               #:clails-test/model/transaction/transaction-postgresql
               #:clails-test/model/pessimistic-lock/lock-sqlite3
               #:clails-test/model/pessimistic-lock/lock-mysql
               #:clails-test/model/pessimistic-lock/lock-postgresql
               #:clails-test/logger/registry
               #:clails-test/logger/purpose-specific
               #:clails-test/logger/file-appender
               #:clails-test/logger/dynamic-config
               #:clails-test/logger/package-hierarchy
               #:clails-test/logger/console-appender-threads
               #:clails-test/view/parser
               #:clails-test/view/compiler
               #:clails-test/view/renderer
               #:clails-test/datetime/all
               #:clails-test/datetime/conversions
               #:clails-test/model/native-query
               #:clails-test/model/native-query/sqlite3
               #:clails-test/model/native-query/mysql
               #:clails-test/model/native-query/postgresql)
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))

