(in-package #:cl-user)
(defpackage #:clails/main
  (:use #:cl)
  (:nicknames #:clails)
  (:import-from #:clails/cmd
                #:create-project
                #:generate/model
                #:generate/migration)
  (:export #:create-project
           #:generate/model
           #:generate/migration))
(in-package #:clails/main)





