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
               ;; HTTP round-trip helpers for the tests (no server, no DB) are
               ;; built on lack-test, and clails-test/middleware/middleware-stack
               ;; uses lack-middleware-csrf as a real extra stage (the framework
               ;; itself does not depend on it).
               ;;
               ;; Neither system is listed here, because neither has to be:
               ;; lack.asd defines forwarding systems named "lack/test" and
               ;; "lack/middleware/csrf", each with nothing but a :depends-on
               ;; of the hyphenated system that actually holds the code.  A
               ;; package-inferred component writing
               ;; (:import-from #:lack/test ...) therefore makes ASDF look for
               ;; a system called "lack/test", find that forwarding definition
               ;; and load lack-test through it.  This is a different
               ;; mechanism from the nickname registration in lack's own
               ;; defpackage forms, which only teaches ASDF that the dotted
               ;; names (LACK.TEST, ...) belong to an already known system.
               #:clails-test/util
               #:clails-test/environment
               #:clails-test/condition
               #:clails-test/model/impl/sqlite3
               #:clails-test/model/impl/mysql
               #:clails-test/model/impl/postgresql
               #:clails-test/model/connection
               #:clails-test/model/query
               #:clails-test/model/query-builder
               #:clails-test/model/query-cache
               #:clails-test/model/query-lazy-initialization
               #:clails-test/model/query/sqlite3
               #:clails-test/model/query/mysql
               #:clails-test/model/query/postgresql
               #:clails-test/controller/base-controller
               #:clails-test/controller/routing-roundtrip
               #:clails-test/middleware/session-middleware
               #:clails-test/middleware/session-http-roundtrip
               #:clails-test/middleware/middleware-stack
               #:clails-test/middleware/session-concurrency
               #:clails-test/middleware/session-clock
               #:clails-test/middleware/session-store
               #:clails-test/middleware/session-store-swap
               #:clails-test/middleware/session-store-wrapper
               #:clails-test/middleware/session-lock
               #:clails-test/middleware/forbidden-response
               #:clails-test/helper/date-helper
               #:clails-test/model/join-query
               #:clails-test/model/save
               #:clails-test/model/optimistic-lock
               #:clails-test/model/default-value
               #:clails-test/model/migration
               #:clails-test/model/migration-filename-validation
               #:clails-test/model/transaction
               #:clails-test/model/transaction/transaction-sqlite3
               #:clails-test/model/transaction/transaction-mysql
               #:clails-test/model/transaction/transaction-postgresql
               #:clails-test/model/pessimistic-lock/lock-sqlite3
               #:clails-test/model/pessimistic-lock/lock-mysql
               #:clails-test/model/pessimistic-lock/lock-postgresql
               #:clails-test/model/bulk
               #:clails-test/model/bulk/bulk-sqlite3
               #:clails-test/model/bulk/bulk-mysql
               #:clails-test/model/bulk/bulk-postgresql
               #:clails-test/model/bulk/insert-sqlite3
               #:clails-test/model/bulk/insert-mysql
               #:clails-test/model/bulk/insert-postgresql
               #:clails-test/model/bulk/update-sqlite3
               #:clails-test/model/bulk/update-mysql
               #:clails-test/model/bulk/update-postgresql
               #:clails-test/model/bulk/delete-sqlite3
               #:clails-test/model/bulk/delete-mysql
               #:clails-test/model/bulk/delete-postgresql
               #:clails-test/logger/registry
               #:clails-test/logger/purpose-specific
               #:clails-test/logger/file-appender
               #:clails-test/logger/dynamic-config
               #:clails-test/logger/package-hierarchy
               #:clails-test/logger/console-appender-threads
               #:clails-test/logger/audit-destination
               #:clails-test/view/parser
               #:clails-test/view/compiler
               #:clails-test/view/renderer
               #:clails-test/datetime/all
               #:clails-test/datetime/conversions
               #:clails-test/model/native-query
               #:clails-test/model/native-query/sqlite3
               #:clails-test/model/native-query/mysql
               #:clails-test/model/native-query/postgresql
               #:clails-test/model/datetime/datetime-sqlite3
               #:clails-test/model/datetime/datetime-mysql
               #:clails-test/model/datetime/datetime-postgresql
               #:clails-test/model/type-conversion/mysql
               #:clails-test/model/type-conversion/postgresql
               #:clails-test/model/type-conversion/sqlite3
               #:clails-test/model/todo-tag-junction-query
               #:clails-test/task/registry
               #:clails-test/task/runner
               #:clails-test/task/core
               #:clails-test/job/registry
               #:clails-test/job/core
               #:clails-test/job/queue
               #:clails-test/job/worker
               #:clails-test/project/generate
               #:clails-test/cmd)
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))

