(in-package #:cl-user)
(defpackage #:clails/model
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:ref-error
                #:ref-in
                #:has-dirty-p
                #:has-error-p
                #:frozen-p
                #:initialize-table-information)
  (:import-from #:clails/model/query
                #:query
                #:execute-query
                #:save
                #:make-record
                #:destroy
                #:to-string
                #:to-text
                #:to-integer
                #:to-float
                #:to-decimal
                #:to-datetime
                #:to-date
                #:to-time
                #:to-boolean)
  (:import-from #:clails/model/transaction
                #:with-transaction
                #:with-transaction-using-connection)
  (:import-from #:clails/model/lock
                #:with-locked-transaction)
  (:import-from #:clails/model/bulk
                #:with-query-cursor
                #:show-query-sql)
  (:import-from #:clails/model/impl/sqlite3)
  (:import-from #:clails/model/impl/mysql)
  (:import-from #:clails/model/impl/postgresql)
  (:import-from #:clails/model/impl/dummy)
  (:export #:<base-model>
           #:defmodel
           #:ref
           #:ref-error
           #:ref-in
           #:has-dirty-p
           #:has-error-p
           #:frozen-p
           #:initialize-table-information

           #:query
           #:execute-query
           #:save
           #:make-record
           #:destroy
           #:with-transaction
           #:with-transaction-using-connection
           #:with-locked-transaction

           #:with-query-cursor
           #:show-query-sql
           
           #:to-string
           #:to-text
           #:to-integer
           #:to-float
           #:to-decimal
           #:to-datetime
           #:to-date
           #:to-time
           #:to-boolean))






