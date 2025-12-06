(in-package #:cl-user)
(defpackage #:clails/model/impl/sqlite3-lock
  (:use #:cl)
  (:import-from #:clails/environment
                #:*sqlite3-transaction-mode*)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:import-from #:dbd.sqlite3
                #:dbd-sqlite3-connection))
(in-package #:clails/model/impl/sqlite3-lock)


;;;; ========================================
;;;; Override begin-transaction for SQLite3

(defmethod dbi:begin-transaction ((conn dbd.sqlite3:dbd-sqlite3-connection))
  "Override begin-transaction for SQLite3 to support lock modes.
   
   Checks *sqlite3-transaction-mode* to determine which BEGIN statement to use.
   This allows with-locked-transaction to control the transaction lock level
   while maintaining compatibility with regular with-transaction usage.

   Transaction modes:
   - :immediate -> BEGIN IMMEDIATE
   - :exclusive -> BEGIN EXCLUSIVE
   - nil        -> BEGIN TRANSACTION (default)

   @param conn [dbd-sqlite3-connection] SQLite3 database connection
   "
  (let* ((tx-mode clails/environment:*sqlite3-transaction-mode*)
         (begin-sql (case tx-mode
                      (:immediate "BEGIN IMMEDIATE")
                      (:exclusive "BEGIN EXCLUSIVE")
                      (otherwise "BEGIN TRANSACTION"))))
    (when (log-level-enabled-p :sql :debug)
      (log.sql begin-sql))
    (sqlite:execute-non-query 
     (dbi.driver:connection-handle conn) 
     begin-sql)))
