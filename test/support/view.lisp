(in-package #:cl-user)
(defpackage #:clails-test/support/view
  (:use #:cl)
  ;; lack-test, reached through the forwarding system "lack/test" -- see the
  ;; note at the top of test/support/http.lisp for why a single :import-from
  ;; of that name is resolvable.
  (:import-from #:lack/test
                #:generate-env)
  (:import-from #:lack/request
                #:make-request)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>)
  (:import-from #:clails/view/renderer
                #:render)
  (:import-from #:clails/view/view-helper
                #:*view-controller*)
  (:import-from #:cl-ppcre
                #:scan-to-strings
                #:do-matches-as-strings)
  (:export ;; a controller to render for
           #:<test-web-controller>
           #:make-view-controller
           #:controller-for-env
           #:controller-session
           ;; rendering a template written inline
           #:with-template
           #:call-with-template
           #:render-template-string
           ;; pretending to be the renderer
           #:with-view-context
           #:call-with-view-context
           ;; reading a rendered form
           #:form-opening-tag
           #:form-attribute
           #:form-action
           #:form-method-attribute
           #:form-enctype
           #:form-hidden-fields
           #:form-hidden-field
           #:form-post-content
           #:unescape-html))
(in-package #:clails-test/support/view)


;;; Shared ground for the M7 view suites (clails-test/view/view-context,
;;; .../form-helper and .../form-helper-roundtrip).  Three jobs: making a
;;; controller that can be rendered for without a routing table, rendering a
;;; template that is written inline in the test rather than kept as a file,
;;; and reading a rendered <form> back apart so that its fields can be posted
;;; again.


;;; ------------------------------------------------------------------
;;; A controller to render for
;;; ------------------------------------------------------------------

(defclass <test-web-controller> (<web-controller>)
  ()
  (:documentation "A <web-controller> with no actions, used as \"the
   controller this template is being rendered for\".

   The view layer only ever reads REQUEST, ENV, PARAMS and the session off
   the controller, so the suites do not need a controller that can be
   dispatched to -- and building one without a routing table is what lets the
   M7-T1 tests be unit tests."))

(defun controller-for-env (env &key params (class '<test-web-controller>))
  "A controller wrapping the EXISTING request environment ENV.

   This is the one to use inside a middleware stack: the environment is the
   real one the stages built, so the session is the session the session stage
   installed and the CSRF stage's token belongs to it.

   @param env [list] Clack/Lack request environment plist
   @param params [list] Alist of request parameters to put in the controller's
     params table, as the controller stage would have
   @param class [symbol] Controller class to instantiate
   @return [<web-controller>] A controller holding ENV
   "
  (let ((controller (make-instance class)))
    (setf (slot-value controller 'clails/controller/base-controller::request)
          (make-request env))
    (setf (slot-value controller 'clails/controller/base-controller::env) env)
    (loop with table = (slot-value controller 'clails/controller/base-controller::params)
          for (key . value) in params
          do (setf (gethash key table) value))
    controller))

(defun make-view-controller (&key (uri "/") (method :get) (session :fresh) params
                                  (class '<test-web-controller>))
  "A controller holding a freshly generated request environment.

   For the tests that need no middleware stack at all.

   @param uri [string] Request URI, query string included
   @param method [keyword] Request method
   @param session [hash-table] The session table to install
   @param session [nil] Install no session, so that CURRENT-SESSION signals
   @param session [:fresh] Install a new empty session (the default)
   @param params [list] Alist of request parameters
   @param class [symbol] Controller class to instantiate
   @return [<web-controller>] A controller holding that environment
   "
  (let* ((table (case session
                  ((:fresh) (make-hash-table :test 'equal))
                  ((nil) nil)
                  (t session)))
         (generated (generate-env uri :method method))
         (env (if table
                  (list* :lack.session table generated)
                  generated)))
    (controller-for-env env :params params :class class)))

(defun controller-session (controller)
  "The session table of CONTROLLER's request, or NIL when it has none."
  (getf (slot-value controller 'clails/controller/base-controller::env)
        :lack.session))


;;; ------------------------------------------------------------------
;;; Rendering a template written inline in the test
;;; ------------------------------------------------------------------

(defvar *template-counter* 0
  "Serial number for the temporary template files, so that two templates
   written by one test never share a path -- and therefore never share an
   entry in the renderer's compiled-template cache, which is keyed by path
   and package.")

(defun call-with-template (template function)
  "Write TEMPLATE to a temporary file, call FUNCTION with its path, delete it.

   @param template [string] Template source
   @param function [function] Called with the pathname
   @return [t] Whatever FUNCTION returned
   "
  (let ((path (make-pathname :name (format nil "clails-test-view-~D-~D"
                                           (get-universal-time)
                                           (incf *template-counter*))
                             :type "html"
                             :defaults (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :external-format :utf-8)
             (write-string template out))
           (funcall function path))
      (when (probe-file path)
        (delete-file path)))))

(defmacro with-template ((path-var template) &body body)
  "Write TEMPLATE to a temporary file, bind PATH-VAR to its path for BODY."
  `(call-with-template ,template (lambda (,path-var) ,@body)))

(defun render-template-string (template &key data controller package)
  "Render TEMPLATE (a string) and return the output.

   @param template [string] Template source
   @param data [list] The plist an action would have passed
   @param controller [<base-controller>] The controller to render for, or NIL
     for \"no request\" -- which is how \"the binding is not in effect\" is
     spelled
   @param package [package] Package the template's expressions are read in.
     Pass it explicitly: *PACKAGE* inside a running test is the test runner's,
     not the suite's
   @return [string] The rendered output
   "
  (with-template (path template)
    (render path data
            :package (or package *package*)
            :controller controller)))


;;; ------------------------------------------------------------------
;;; Pretending to be the renderer
;;; ------------------------------------------------------------------

(defun call-with-view-context (controller thunk)
  "Call THUNK with CONTROLLER installed as the controller being rendered.

   The binding RENDER establishes, without a template: this is what lets a
   suite call a form helper directly and assert on the string it returns."
  (let ((*view-controller* controller))
    (funcall thunk)))

(defmacro with-view-context ((controller) &body body)
  "Run BODY as if a template were being rendered for CONTROLLER."
  `(call-with-view-context ,controller (lambda () ,@body)))


;;; ------------------------------------------------------------------
;;; Reading a rendered form back apart
;;; ------------------------------------------------------------------

(defun unescape-html (text)
  "TEXT with the five entities the form helpers write turned back.

   So that a value read out of a rendered form is the value a browser would
   have sent."
  (let ((result text))
    (loop for (entity . character) in '(("&lt;" . "<")
                                        ("&gt;" . ">")
                                        ("&quot;" . "\"")
                                        ("&#39;" . "'")
                                        ("&amp;" . "&"))
          do (setf result (cl-ppcre:regex-replace-all entity result character)))
    result))

(defun form-opening-tag (html)
  "The <form ...> opening tag of the first form in HTML, or NIL."
  (scan-to-strings "<form[^>]*>" html))

(defun tag-attribute (tag name)
  "The value of attribute NAME in TAG, unescaped, or NIL when absent."
  (when tag
    (multiple-value-bind (whole groups)
        (scan-to-strings (format nil "[ <]~A=\"([^\"]*)\"" name) tag)
      (declare (ignore whole))
      (when groups
        (unescape-html (aref groups 0))))))

(defun form-attribute (html name)
  "The value of attribute NAME on the first <form> element of HTML."
  (tag-attribute (form-opening-tag html) name))

(defun form-action (html)
  "The action attribute of the first form in HTML, or NIL when it has none."
  (form-attribute html "action"))

(defun form-method-attribute (html)
  "The method attribute of the first form in HTML, as written."
  (form-attribute html "method"))

(defun form-enctype (html)
  "The enctype attribute of the first form in HTML, or NIL when it has none."
  (form-attribute html "enctype"))

(defun form-hidden-fields (html)
  "Every hidden input of HTML as an (NAME . VALUE) alist, in document order.

   This is \"what the browser would send\": posting this alist back is
   posting the helper's own output, unedited."
  (let ((fields '()))
    (do-matches-as-strings (tag "<input[^>]*>" html)
      (when (string= "hidden" (or (tag-attribute tag "type") ""))
        (push (cons (tag-attribute tag "name")
                    (or (tag-attribute tag "value") ""))
              fields)))
    (nreverse fields)))

(defun form-hidden-field (html name)
  "The value of the hidden field NAME in HTML, or NIL when there is none."
  (cdr (assoc name (form-hidden-fields html) :test #'string=)))

(defun form-post-content (html &key extra)
  "The body a browser would send for the form in HTML, for CALL-APP's
   :content.

   Every hidden field the helper wrote, plus EXTRA -- the form's own visible
   fields, which a test spells out because the template's <input> elements are
   the application's, not the framework's."
  (append (form-hidden-fields html) extra))
