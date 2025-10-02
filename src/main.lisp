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
  (:import-from #:clails/middleware/clails-middleware)
  (:import-from #:clails/logger)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:db/create
           #:db/migrate))
(in-package #:clails/main)

