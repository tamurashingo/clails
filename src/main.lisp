(in-package #:cl-user)
(defpackage #:clails/main
  (:use #:cl)
  (:nicknames #:clails)
  (:import-from #:clails/cmd
                #:create-project
                #:generate/model
                #:generate/migration)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:<base-model>)
  (:export #:create-project
           #:generate/model
           #:generate/migration))
(in-package #:clails/main)





