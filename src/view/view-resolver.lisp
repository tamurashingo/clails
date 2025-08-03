(in-package #:cl-user)
(defpackage #:clails/view/view-resolver
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:<rest-controller>
                #:code
                #:header
                #:view)
  (:import-from #:clails/controller/error-handle-controller
                #:<error-handle-controller>
                #:exception)
  (:import-from #:clails/condition
                #:message
                #:path)
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
  "json")

(defmethod resolve-view ((controller <error-handle-controller>))
  (let* ((exception (exception controller))
         (code (clails/condition:code exception)))
    `(,code
      (:content-type "text/html")
      ("error"))))


(defun extract-template (controller template-name)
  ;; A hack to avoid having to specify package names within cl-template.
  (let ((*package* (symbol-package (class-name (class-of controller)))))
    (let ((tmpl (uiop:read-file-string template-name
                                       :external-format :utf-8)))
      (funcall (cl-template:compile-template tmpl)
               `(:controller ,controller)))))


(defun view/html-template (controller content)
  `(,(code controller)
    ,(header controller)
    (,content)))


(defun view/json (plist)
  "json")

