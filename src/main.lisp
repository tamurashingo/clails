(in-package #:cl-user)
(defpackage #:clails/main
  (:use #:cl)
  (:nicknames #:clails)
  (:import-from #:clails/project/project)
  (:import-from #:clails/cmd
                #:create-project
                #:generate/model
                #:generate/migration
                #:db/create
                #:db/migrate
                #:db/migrate-up
                #:db/migrate-down
                #:db/rollback)
  (:import-from #:clails/model)
  (:import-from #:clails/view)
  (:import-from #:clails/middleware)
  (:import-from #:clails/logger)
  (:import-from #:clails/datetime)
  (:import-from #:clails/task)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:db/create
           #:db/migrate
           #:db/migrate-up
           #:db/migrate-down
           #:db/rollback))
(in-package #:clails/main)


