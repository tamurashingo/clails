(in-package #:cl-user)
(defpackage #:clails/middleware
  (:use #:cl)
  (:import-from #:clails/middleware/core
                #:*clails-middleware-stack*
                #:add-middleware-before
                #:add-middleware-after
                #:show-middleware-stack)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:export #:*clails-middleware-stack*
           #:*lack-middleware-clails-controller*
           #:add-middleware-before
           #:add-middleware-after
           #:show-middleware-stack))

