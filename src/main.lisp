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
                #:db/migrate)
  (:import-from #:clails/model)
  (:import-from #:clails/view)
  (:import-from #:clails/middleware)
  (:import-from #:clails/logger)
  (:import-from #:clails/datetime)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:db/create
           #:db/migrate))
(in-package #:clails/main)

