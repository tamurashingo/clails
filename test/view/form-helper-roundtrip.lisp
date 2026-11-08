(in-package #:cl-user)
(defpackage #:clails-test/view/form-helper-roundtrip
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf
        #:clails-test/support/method-override
        #:clails-test/support/routing
        #:clails-test/support/view)
  ;; Calling convention 1: the helpers are imported into the package the
  ;; templates are read in, which is what a generated view package does, and
  ;; what lets a template say (form-tag ...).
  (:import-from #:clails/view/form-helper
                #:form-tag
                #:end-form-tag
                #:form-with)
  ;; Nothing is imported from the facade, but naming it here is what makes
  ;; ASDF load it -- so that the template written against CLAILS/VIEW below
  ;; can be read when this suite is run on its own, exactly as it can when
  ;; the whole of CLAILS (which loads CLAILS/MAIN, which loads CLAILS/VIEW)
  ;; is loaded.
  (:import-from #:clails/view)
  (:import-from #:clails/view/view-helper
                #:view)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:view-package
                #:view-data
                #:param
                #:env
                #:resources)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*csrf-form-token*)
  (:import-from #:clails/middleware/method-override-middleware
                #:+method-override-methods+
                #:*method-override-parameter*)
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+))

;;; Calling convention 2: a view package that imports NOTHING -- the helpers
;;; are reached by naming their package (or the CLAILS/VIEW facade that
;;; re-exports them).  One of the templates below is read in this package, so
;;; that both conventions are exercised against a real request rather than
;;; only described in the documentation.
;;;
;;; (This extra DEFPACKAGE is read by ASDF's package-inferred-system along
;;; with the primary one above; it names no other package in a :USE or
;;; :IMPORT-FROM clause, so it adds no dependency.)
(defpackage #:clails-test-bare-view-package
  (:use #:cl))

(in-package #:clails-test/view/form-helper-roundtrip)


;;; M7 seen from the outside: a form the helpers rendered is posted back,
;;; unedited, through the real stack.
;;;
;;; Requests go through session -> CSRF verification -> request-method
;;; override -> the controller (routing) stage, against the routes RESOURCES
;;; generates.  A test asks for a page, reads the <form> out of the rendered
;;; HTML, collects the hidden fields the helper put there and posts exactly
;;; those back.  Nothing in a test knows what a token looks like or what the
;;; override field is called; whatever the helper wrote is what is sent.
;;;
;;; That is what makes this the test of the milestone's point (G13): if the
;;; helper's output and the two stages disagree about anything -- the field
;;; names, the method, where the forged value sits -- the submission answers
;;; 403 or 404 instead of reaching an action.


;;; ------------------------------------------------------------------
;;; A resource whose pages are rendered from real templates
;;; ------------------------------------------------------------------

(defvar *page-template* nil
  "Path of the template the page actions (index, new, edit, show) render.
   Bound per test, so one controller serves every form shape.")

(defvar *page-package* :clails-test/view/form-helper-roundtrip
  "Package *PAGE-TEMPLATE* is read in.  Bound to another package by the test
   of the second calling convention.")

(defvar *report-template* nil
  "Path of the template the mutating actions render: it prints which action
   ran, which id it saw and which method reached it.")

(defclass <todo-controller> (<web-controller>)
  ())

(defun render-page (controller)
  "Render *PAGE-TEMPLATE* -- \"the page with the form on it\".

   SET-VIEW is bypassed on purpose: it builds the template path out of
   CLAILS/ENVIRONMENT:*PROJECT-DIR*, and these templates are temporary files
   belonging to the test, not files of a generated project.  Everything else
   about the rendering is the normal path, RESOLVE-VIEW included."
  (setf (clails/controller/base-controller:view controller) *page-template*)
  (setf (view-package controller) *page-package*)
  (setf (view-data controller) (list :id (or (param controller "id") "7"))))

(defun render-report (controller action)
  "Render *REPORT-TEMPLATE* for ACTION, so the response says what was reached."
  (setf (clails/controller/base-controller:view controller) *report-template*)
  (setf (view-package controller) :clails-test/view/form-helper-roundtrip)
  (setf (view-data controller)
        (list :action action
              :id (param controller "id")
              :method (getf (env controller) :request-method))))

(defgeneric index (controller))
(defmethod index ((controller <todo-controller>)) (render-page controller))

(defgeneric new (controller))
(defmethod new ((controller <todo-controller>)) (render-page controller))

(defgeneric show (controller))
(defmethod show ((controller <todo-controller>)) (render-report controller "show"))

(defgeneric edit (controller))
(defmethod edit ((controller <todo-controller>)) (render-page controller))

(defgeneric create (controller))
(defmethod create ((controller <todo-controller>)) (render-report controller "create"))

(defgeneric update (controller))
(defmethod update ((controller <todo-controller>)) (render-report controller "update"))

(defgeneric destroy (controller))
(defmethod destroy ((controller <todo-controller>)) (render-report controller "destroy"))

(defparameter +todo-controller+
  "clails-test/view/form-helper-roundtrip::<todo-controller>")

(defparameter +report-template+
  "action=<%= (view :action) %> id=<%= (view :id) %> method=<%= (view :method) %>"
  "What a mutating action answers.  Read with REPORTED-ACTION and friends.")


;;; ------------------------------------------------------------------
;;; The templates under test
;;; ------------------------------------------------------------------
;;;
;;; Each one is a page with one form on it, written the way a hand-written
;;; template would write it.  The helpers are called from inside the template,
;;; i.e. inside the rendering, i.e. inside the CSRF stage -- which is the
;;; whole reason they can ask for a token.

(defparameter +create-form+
  "<h1>New</h1>
<%= (form-tag :url \"/todos\" :method :post) %>
<input type=\"text\" name=\"title\" value=\"milk\">
<input type=\"submit\" value=\"Create\">
<%= (end-form-tag) %>")

(defparameter +update-form+
  "<h1>Edit</h1>
<%= (form-tag :url (format nil \"/todos/~A\" (view :id)) :method :put) %>
<input type=\"text\" name=\"title\" value=\"milk\">
<input type=\"submit\" value=\"Save\">
<%= (end-form-tag) %>")

(defparameter +patch-form+
  "<%= (form-tag :url (format nil \"/todos/~A\" (view :id)) :method :patch) %>
<input type=\"text\" name=\"title\" value=\"milk\">
<%= (end-form-tag) %>")

(defparameter +delete-form+
  "<%= (form-with :url (format nil \"/todos/~A\" (view :id))
                  :method :delete
                  :body \"<input type=\\\"submit\\\" value=\\\"Delete\\\">\") %>")

(defparameter +search-form+
  "<%= (form-tag :url \"/todos\" :method :get) %>
<input type=\"text\" name=\"q\">
<%= (end-form-tag) %>")

(defparameter +upload-form+
  "<%= (form-tag :url (format nil \"/todos/~A\" (view :id))
                 :method :put
                 :multipart t) %>
<input type=\"text\" name=\"title\" value=\"milk\">
<input type=\"file\" name=\"attachment\">
<%= (end-form-tag) %>")

(defparameter +qualified-update-form+
  ;; Calling convention 2: nothing is imported into
  ;; CLAILS-TEST-BARE-VIEW-PACKAGE, so every name is written out.  Both
  ;; spellings are exercised: the helper's own package and the CLAILS/VIEW
  ;; facade that re-exports it.
  "<%= (clails/view/form-helper:form-tag :url \"/todos/7\" :method :put) %>
<input type=\"text\" name=\"title\" value=\"milk\">
<%= (clails/view:end-form-tag) %>
<%= (clails/view:form-with :url \"/todos/7\" :method :delete :body \"<input type=\\\"submit\\\">\") %>")


;;; ------------------------------------------------------------------
;;; Driving the stack
;;; ------------------------------------------------------------------

(defmacro with-todo-client ((client page &key (page-package nil) (override t))
                            &body body)
  "Install the routes of a \"todos\" resource, build
   session -> CSRF -> override -> routing, write PAGE and the report template
   to temporary files, and bind CLIENT to a session client against the stack.

   The loggers are rebound to none, so the audit record a refused request
   leaves does not print into the middle of the test report."
  `(with-template (page-path ,page)
     (with-template (report-path +report-template+)
       (let ((*page-template* page-path)
             (*report-template* report-path)
             (*page-package* (or ,page-package *page-package*)))
         (with-routing-table ((resources "todos" +todo-controller+))
           (with-no-loggers
             (with-override-stack (app :override ,override
                                       :inner #'not-found-app
                                       :inner-middleware (list *lack-middleware-clails-controller*))
               (let ((,client (open-session app)))
                 ,@body))))))))

(defun fetch-page (client uri)
  "GET URI and return the HTML the page action rendered."
  (response-body-string (client-request client uri)))

(defun submit-form (client html &key extra)
  "Post the form in HTML back, exactly as rendered.

   The method, the action and every hidden field come out of the rendered
   HTML; EXTRA is the form's own visible fields, which belong to the template
   rather than to the framework."
  (client-request client
                  (or (form-action html) "/")
                  :method (intern (string-upcase (form-method-attribute html)) :keyword)
                  :content (form-post-content html :extra extra)))

(defun submit-form-without-token (client html &key extra)
  "Post the same form with the token field removed -- the contrast that shows
   the token is what carried the submission through."
  (let ((fields (remove *csrf-form-token* (form-hidden-fields html)
                        :key #'car :test #'string=)))
    (client-request client
                    (or (form-action html) "/")
                    :method (intern (string-upcase (form-method-attribute html)) :keyword)
                    :content (append fields extra))))

(defun reported (response key)
  "The value the report template printed for KEY, or NIL."
  (let* ((body (response-body-string response))
         (needle (format nil "~A=" key))
         (start (search needle body)))
    (when start
      (let* ((from (+ start (length needle)))
             (to (or (position #\Space body :start from) (length body))))
        (subseq body from to)))))

(defun reported-action (response)
  "The action the request reached, as a keyword, or NIL when it reached none."
  (let ((action (reported response "action")))
    (when action (intern (string-upcase action) :keyword))))


;;; ------------------------------------------------------------------
;;; The round trip: a rendered form reaches its action (M7-T2, M7-T3, G13)
;;; ------------------------------------------------------------------

(deftest rendered-form-passes-csrf-verification-test
  ;; M7-T2's round trip: the helper's output, posted unedited, is verified.
  (testing "a POST form rendered by the helper reaches the create action"
    (with-todo-client (client +create-form+)
      (let ((html (fetch-page client "/todos")))
        (ok (search "<form" html) "the page carries a form")
        (ok (stringp (form-hidden-field html *csrf-form-token*))
            "and the form carries a token field")
        (let ((response (submit-form client html)))
          (ok (eql (response-status response) 200)
              "the submission is not refused")
          (ok (eq (reported-action response) :create)
              "and it reached the create action")))))

  (testing "the same submission without the token field is refused"
    ;; The contrast: everything else about the request is identical, so it is
    ;; the helper's token field that carried the first one through.
    (with-todo-client (client +create-form+)
      (let* ((html (fetch-page client "/todos"))
             (response (submit-form-without-token client html)))
        (ok (eql (response-status response) +forbidden-status+))
        (ok (null (reported-action response)) "no action ran"))))

  (testing "a page rendered twice gives tokens that both work"
    (with-todo-client (client +create-form+)
      (dotimes (i 2)
        (let ((response (submit-form client (fetch-page client "/todos"))))
          (ok (eq (reported-action response) :create)
              (format nil "submission ~D reached create" (1+ i))))))))

(deftest rendered-form-reaches-update-and-destroy-test
  ;; M7-T3's round trip, and G13: the point of the whole goal.  An HTML form
  ;; can only be sent with POST, and the resource's update and destroy routes
  ;; are PUT and DELETE; the helper's hidden override field is what bridges
  ;; that, and nothing in this test spells the field out.
  (testing "a form declaring :PUT reaches the update action"
    (with-todo-client (client +update-form+)
      (let ((html (fetch-page client "/todos/7/edit")))
        (ok (string= (form-method-attribute html) "post")
            "the form is sent as a POST")
        (ok (string= (form-action html) "/todos/7")
            "to the member path, with no query string")
        (let ((response (submit-form client html)))
          (ok (eql (response-status response) 200))
          (ok (eq (reported-action response) :update))
          (ok (string= (reported response "id") "7")
              "and the action read the id out of the path")
          (ok (string= (reported response "method") "PUT")
              "the action sees the method the form declared")))))

  (testing "a form declaring :DELETE reaches the destroy action"
    (with-todo-client (client +delete-form+)
      (let* ((html (fetch-page client "/todos/9/edit"))
             (response (submit-form client html)))
        (ok (eql (response-status response) 200))
        (ok (eq (reported-action response) :destroy))
        (ok (string= (reported response "id") "9")))))

  (testing "a form declaring :PATCH reaches the update action as a PATCH"
    (with-todo-client (client +patch-form+)
      (let* ((html (fetch-page client "/todos/3/edit"))
             (response (submit-form client html)))
        (ok (eq (reported-action response) :update))
        (ok (string= (reported response "method") "PATCH")
            "the action can tell a partial update from a replacement"))))

  (testing "every method the override stage accepts works from a rendered form"
    ;; The behavioural half of \"the helper's list and the stage's list are
    ;; the same list\": each one is rendered, posted and followed to an action.
    (dolist (method +method-override-methods+)
      (let ((template (format nil
                              "<%= (form-tag :url \"/todos/5\" :method ~S) %><%= (end-form-tag) %>"
                              method)))
        (with-todo-client (client template)
          (let* ((html (fetch-page client "/todos/5/edit"))
                 (response (submit-form client html)))
            (ok (eql (response-status response) 200)
                (format nil "a ~S form is not refused" method))
            (ok (member (reported-action response) '(:update :destroy))
                (format nil "a ~S form reaches a mutating action" method))
            (ok (string= (reported response "method") (symbol-name method))
                (format nil "and the action is reached as ~S" method)))))))

  (testing "the forged value travelled in the body, not in the action URL"
    (with-todo-client (client +update-form+)
      (let ((html (fetch-page client "/todos/7/edit")))
        (ok (not (search *method-override-parameter* (form-action html)))
            "the action URL says nothing about the method")
        (ok (string= (form-hidden-field html *method-override-parameter*) "put")
            "the method is a hidden field of the body"))))

  (testing "without the override stage the very same form answers 404"
    ;; What the defect looked like before M9, kept as the contrast: the
    ;; helper's output is unchanged, only the stage is gone.
    (with-todo-client (client +update-form+ :override nil)
      (let ((html (fetch-page client "/todos/7/edit")))
        (ok (eql (response-status (submit-form client html)) 404))))))

(deftest rendered-get-form-test
  (testing "a GET form carries no token field, and still reaches its action"
    ;; Two statements in one: the token is absent (it must never be put in a
    ;; URL), and a search form keeps working.
    (with-todo-client (client +search-form+)
      (let ((html (fetch-page client "/todos")))
        (ok (string= (form-method-attribute html) "get"))
        (ok (null (form-hidden-fields html)) "no hidden fields at all")
        (ok (not (search *csrf-form-token* html))
            "the token's field name appears nowhere in the page")
        (ok (search "<form" (fetch-page client "/todos?q=milk"))
            "and submitting it is an ordinary GET of the same page")))))

(deftest rendered-multipart-form-test
  ;; A1: a form with a file input sends multipart/form-data, where every field
  ;; is a part of its own and a parameter's value arrives as a list rather
  ;; than a string.  The token still has to be read, and the override field
  ;; with it.
  (testing "a multipart form's token is a top-level part and is verified"
    (with-todo-client (client +upload-form+)
      (let* ((html (fetch-page client "/todos/7/edit"))
             (hidden (form-hidden-fields html))
             (token (cdr (assoc *csrf-form-token* hidden :test #'string=)))
             (others (remove *csrf-form-token* hidden :key #'car :test #'string=)))
        (ok (string= (form-enctype html) "multipart/form-data"))
        (ok (stringp token) "the form carries a token")
        (ok (string= (cdr (assoc *method-override-parameter* others :test #'string=))
                     "put")
            "and the override field")
        ;; The body is assembled from the fields the helper rendered, with a
        ;; file part added -- which is what the browser would send.
        (let ((response (client-request client (form-action html)
                                        :method :post
                                        :content (multipart-content
                                                  token
                                                  :extra (append others
                                                                 '(("title" . "milk"))))
                                        :headers (multipart-headers))))
          (ok (eql (response-status response) 200)
              "the multipart submission is verified")
          (ok (eq (reported-action response) :update)
              "and it reached the update action")
          (ok (string= (reported response "method") "PUT"))))))

  (testing "the same multipart submission without the token is refused"
    (with-todo-client (client +upload-form+)
      (let* ((html (fetch-page client "/todos/7/edit"))
             (others (remove *csrf-form-token* (form-hidden-fields html)
                             :key #'car :test #'string=))
             (response (client-request client (form-action html)
                                       :method :post
                                       :content (multipart-content
                                                 nil
                                                 :extra (append others
                                                                '(("title" . "milk"))))
                                       :headers (multipart-headers))))
        (ok (eql (response-status response) +forbidden-status+))
        (ok (null (reported-action response)))))))


;;; ------------------------------------------------------------------
;;; Both ways of calling a helper from a template
;;; ------------------------------------------------------------------

(deftest calling-conventions-test
  ;; Convention 1 -- the helpers imported into the view's own package -- is
  ;; what every other test in this file uses, since the templates above say
  ;; (form-tag ...) and this package imports it.  This test is the other one.
  (testing "a view package that imports nothing can name the helper's package"
    (with-todo-client (client +qualified-update-form+
                              :page-package :clails-test-bare-view-package)
      (let ((html (fetch-page client "/todos/7/edit")))
        (ok (search "<form" html) "the page rendered")
        (ok (string= (form-hidden-field html *method-override-parameter*) "put"))
        (ok (stringp (form-hidden-field html *csrf-form-token*)))
        (let ((response (submit-form client html)))
          (ok (eql (response-status response) 200))
          (ok (eq (reported-action response) :update)
              "and the form it rendered works exactly as the imported one does")))))

  (testing "the CLAILS/VIEW facade re-exports the same helpers"
    ;; The second form on that page is written with CLAILS/VIEW: naming the
    ;; facade has to work as well as naming the defining package, because that
    ;; is the package an application already knows about.
    (with-todo-client (client +qualified-update-form+
                              :page-package :clails-test-bare-view-package)
      (let* ((html (fetch-page client "/todos/7/edit"))
             (second-form (subseq html (+ (search "</form>" html) (length "</form>")))))
        (ok (string= (form-hidden-field second-form *method-override-parameter*)
                     "delete")
            "the facade's FORM-WITH rendered a delete form")
        (ok (eq (reported-action (submit-form client second-form)) :destroy)
            "and it reaches the destroy action")))))
