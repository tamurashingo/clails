(in-package #:cl-user)
(defpackage #:clails/view/view-resolver
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:<rest-controller>
                #:view)
  (:export #:resolve-view))


(in-package #:clails/view/view-resolver)

(defgeneric resolve-view (controller)
  (:documentation ""))

(defmethod resolve-view ((controller <web-controller>))
  (view/html-template controller (view controller)))

(defmethod resolve-view ((controller <rest-controller>))
  "json")


(defun view/html-template (controller template-name)
  (let ((tmpl (uiop:read-file-string template-name
                                     :external-format :utf-8)))
    `(200
      (:content-type "text/html")
      (,(funcall (cl-template:compile-template tmpl)
                 `(:controller ,controller))))))

(defun view/json (plist)
  "json")

