(in-package #:cl-user)
(defpackage #:clails/view/view-resolver
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:<rest-controller>
                #:view)
  (:import-from #:clails/controller/error-handle-controller
                #:<error-handle-controller>
                #:exception)
  (:import-from #:clails/condition
                #:code
                #:message
                #:path)
  (:export #:resolve-view))


(in-package #:clails/view/view-resolver)

(defgeneric resolve-view (controller)
  (:documentation ""))

(defmethod resolve-view ((controller <web-controller>))
  (view/html-template controller (view controller)))

(defmethod resolve-view ((controller <rest-controller>))
  "json")

(defmethod resolve-view ((controller <error-handle-controller>))
  (let* ((exception (exception controller))
         (code (code exception)))
    `(,code
      (:content-type "text/html")
      ("error"))))



(defun view/html-template (controller template-name)
  ;; A hack to avoid having to specify package names within cl-template.
  (let ((*package* (symbol-package (class-name (class-of controller)))))
    (let ((tmpl (uiop:read-file-string template-name
                                       :external-format :utf-8)))
      `(200
        (:content-type "text/html")
        (,(funcall (cl-template:compile-template tmpl)
                   `(:controller ,controller)))))))

(defun view/json (plist)
  "json")

