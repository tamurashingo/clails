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
                #:view-package
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
  (:documentation "Resolve and render the view for the given controller.
   
   Determines the appropriate response format (HTML, JSON, error page)
   based on the controller type and generates the HTTP response.
   
   @param controller [<base-controller>] Controller instance
   @return [list] HTTP response as (status-code headers body)
   "))

(defmethod resolve-view ((controller <web-controller>))
  "Resolve web controller view to HTML response.
   
   @param controller [<web-controller>] Web controller instance
   @return [list] HTTP response with HTML content
   "
  (let ((content (if (view controller)
                     (extract-template controller (view controller))
                     "")))
    (view/html-template controller content)))

(defmethod resolve-view ((controller <rest-controller>))
  "Resolve REST controller response to JSON.
   
   @param controller [<rest-controller>] REST controller instance
   @return [list] HTTP response with JSON content
   "
  (let ((response (jonathan:to-json (response controller) :from :alist)))
    (view/json controller response)))


(defmethod resolve-view ((controller <error-handle-controller>))
  "Resolve error controller to error page.
   
   @param controller [<error-handle-controller>] Error handler controller instance
   @return [list] HTTP error response
   "
  (let* ((exception (exception controller))
         (code (clails/condition:code exception)))
    `(,code
      (:content-type "text/html")
      ("error"))))


(defun extract-template (controller template-name)
  "Render template using the template engine.
   
   Uses the controller's view-package if available, otherwise falls back
   to the controller's package.
   
   @param controller [<web-controller>] Controller instance
   @param template-name [pathname] Path to template file
   @param template-name [string] Path to template file
   @return [string] Rendered HTML content
   "
  (let* ((view-pkg (view-package controller))
         (controller-package (symbol-package (class-name (class-of controller))))
         (package (if view-pkg
                      (find-package view-pkg)
                      controller-package))
         (data (or (view-data controller)
                   (list :controller controller))))
    (render template-name data :package package)))


(defun view/html-template (controller content)
  "Build HTML response from controller and content.
   
   @param controller [<web-controller>] Controller instance
   @param content [string] Rendered HTML content
   @return [list] HTTP response as (status-code headers body)
   "
  `(,(code controller)
    ,(header controller)
    (,content)))


(defun view/json (controller content)
  "Build JSON response from controller and content.
   
   @param controller [<rest-controller>] Controller instance
   @param content [string] JSON content
   @return [list] HTTP response as (status-code headers body)
   "
  `(,(code controller)
    ,(header controller)
    (,content)))

