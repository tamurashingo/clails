(in-package #:cl-user)
(defpackage #:clails/model/transaction
  (:use #:cl)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/logger
                #:log-level-enabled-p
                #:log.sql)
  (:export #:with-transaction))
(in-package #:clails/model/transaction)


(defmacro with-transaction (&body body)
  "Execute body within a database transaction.
   
   Wraps the body in dbi-cp:with-transaction macro which handles
   begin, commit, and rollback automatically. Supports nested transactions
   via cl-dbi's automatic savepoint management.
   
   If called within an existing transaction (e.g., from middleware),
   creates a savepoint. If called at the top level, creates a new transaction.
   
   @param body [form] Forms to execute within the transaction
   @return [value] Returns the value of the last form in body
   @condition any Re-raises any condition after rollback
   
   Example:
   (with-transaction
     (save user)
     (save profile))
   
   Example with nested transaction:
   (with-transaction
     (save user)
     (with-transaction  ; This creates a savepoint
       (save address)))
   "
  (let ((connection-var (gensym "CONNECTION")))
    `(let ((,connection-var (get-connection)))
       (when (log-level-enabled-p :sql :debug)
         (log.sql "BEGIN TRANSACTION"))
       (dbi-cp:with-transaction ,connection-var
         (when (log-level-enabled-p :sql :debug)
           (log.sql "EXECUTING TRANSACTION"))
         ,@body))))
