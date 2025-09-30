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
                #:frozen-p)
  (:import-from #:clails/model/query
                #:query
                #:execute-query
                #:save
                #:make-record
                #:destroy)
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

           #:query
           #:execute-query
           #:save
           #:make-record
           #:destroy))






