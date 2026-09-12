;;;; Job Core
;;;; DSL for defining background jobs

(defpackage #:clails/job/core
  (:use #:cl)
  (:import-from #:clails/job/registry
                #:register-job)
  (:export #:defjob))

(in-package #:clails/job/core)

(defmacro defjob (name &key description (max-attempts 25) function)
  "Define a background job handler.

   Jobs defined with defjob are executed asynchronously by the job worker
   (see clails/job/worker), after being persisted with enqueue-job. This is
   the asynchronous sibling of clails/task:deftask: where deftask registers a
   one-shot, synchronous, rake-style command, defjob registers a handler that
   the worker looks up by name and runs later, with automatic retry/backoff.

   Example:
     (defjob :send-welcome-email
       :description \"Send a welcome email to a new user\"
       :max-attempts 5
       :function (lambda (&key user-id)
                   (send-mail (find-user-email user-id) \"Welcome!\")))

     (enqueue-job :send-welcome-email :arguments (list :user-id 42))

   @param name [keyword] Job name, used both to register the handler and to
                         look it up when the worker claims a persisted job
   @param description [string or nil] Job description (optional)
   @param max-attempts [integer] Default number of attempts (including the
                                 first) before a failing job is marked
                                 :failed instead of retried (default: 25).
                                 Can be overridden per-call via
                                 (enqueue-job ... :max-attempts n)
   @param function [form] Job implementation (should be a lambda form taking
                          the keyword arguments passed to enqueue-job)
   @return [keyword] The registered job name
   "
  `(register-job ',name
                 :description ,description
                 :max-attempts ,max-attempts
                 :function ,function))
