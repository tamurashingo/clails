(in-package #:cl-user)
(defpackage #:clails/main
  (:use #:cl)
  (:nicknames #:clails)
  (:import-from #:clails/cmd
                #:create-project
                #:generate/model
                #:generate/migration
                #:db/create
                #:db/migrate)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:<base-model>)
  (:import-from #:clails/model/query)
  (:import-from #:clails/model/impl/sqlite3)
  (:import-from #:clails/model/impl/mysql)
  (:import-from #:clails/model/impl/postgresql)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:db/create
           #:db/migrate))
(in-package #:clails/main)

