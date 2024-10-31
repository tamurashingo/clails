(in-package #:cl-user)
(defpackage #:clails/main
  (:use #:cl)
  (:nicknames #:clails)
  (:import-from #:clails/cmd
                #:create-project)
  (:export #:create-project))
(in-package #:clails/main)

