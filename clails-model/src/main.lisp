(in-package #:cl-user)
(defpackage #:clails-model/main
  (:nicknames #:clails-model)
  (:use #:cl)
  (:import-from #:clails-model/base-model
                #:<base-model>)
  (:export #:<base-model>))
(in-package #:clails-model/main)

