(in-package #:cl-user)
(defpackage #:clails/view/view-resolver
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:<rest-controller>
                #:code
                #:header
                #:view
                #:view-data
                #:response)
  (:import-from #:clails/controller/error-handle-controller
                #:<error-handle-controller>
                #:exception)
  (:import-from #:clails/condition
                #:message
                #:path)
  (:import-from #:clails/view/renderer
                #:render)
  (:export #:resolve-view))


(in-package #:clails/view/view-resolver)

(defgeneric resolve-view (controller)
  (:documentation ""))

(defmethod resolve-view ((controller <web-controller>))
  (let ((content (if (view controller)
                     (extract-template controller (view controller))
                     "")))
    (view/html-template controller content)))

(defmethod resolve-view ((controller <rest-controller>))
  (let ((response (jonathan:to-json (response controller) :from :alist)))
    (view/json controller response)))


(defmethod resolve-view ((controller <error-handle-controller>))
  (let* ((exception (exception controller))
         (code (clails/condition:code exception)))
    `(,code
      (:content-type "text/html")
      ("error"))))


(defun extract-template (controller template-name)
  "Render template using the new template engine.
   Use the controller's package for evaluating template code."
  (let* ((controller-package (symbol-package (class-name (class-of controller))))
         (data (or (view-data controller)
                   (list :controller controller))))
    (render template-name data :package controller-package)))


(defun view/html-template (controller content)
  `(,(code controller)
    ,(header controller)
    (,content)))


(defun view/json (controller content)
  `(,(code controller)
    ,(header controller)
    (,content)))

