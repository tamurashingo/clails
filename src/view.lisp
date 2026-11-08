(in-package #:cl-user)
(defpackage #:clails/view
  (:use #:cl)
  ;; What a template can read: the data an action passed, and the request the
  ;; action is answering (M7-T1).
  (:import-from #:clails/view/view-helper
                #:view
                #:*view-context*
                #:*view-controller*
                #:view-context-error
                #:in-view-p
                #:current-controller
                #:current-request
                #:current-env
                #:current-session
                #:current-param)
  ;; The form helpers (M7-T2, M7-T3).  Re-exported here for two reasons: so
  ;; that a project reaches them through the one view package it already knows
  ;; about, and so that naming CLAILS/VIEW is enough to get the system loaded
  ;; -- which is what makes the fully-qualified calling convention
  ;; (clails/view/form-helper:form-tag ...) work in a template whose own
  ;; package imports nothing.
  (:import-from #:clails/view/form-helper
                #:+form-methods+
                #:form-helper-error
                #:form-method-override-p
                #:escape-html
                #:hidden-field-tag
                #:form-authenticity-token
                #:csrf-token-tag
                #:method-override-tag
                #:form-tag
                #:end-form-tag
                #:form-with)
  (:export #:view
           #:*view-context*
           #:*view-controller*
           #:view-context-error
           #:in-view-p
           #:current-controller
           #:current-request
           #:current-env
           #:current-session
           #:current-param
           #:+form-methods+
           #:form-helper-error
           #:form-method-override-p
           #:escape-html
           #:hidden-field-tag
           #:form-authenticity-token
           #:csrf-token-tag
           #:method-override-tag
           #:form-tag
           #:end-form-tag
           #:form-with))
