(in-package #:cl-user)
(defpackage #:clails/view
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:view)
  (:export #:view))
