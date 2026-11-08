(in-package #:cl-user)
(defpackage #:clails/view/form-helper
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:current-session)
  ;; The CSRF verification stage.  CSRF-TOKEN and CSRF-HTML-TAG are lack's own
  ;; (re-exported by the clails stage): they read dynamic variables the stage
  ;; binds per request, so they only work while the stage is on the stack --
  ;; which, for a template, it is.  See the commentary below.
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:csrf-token
                #:csrf-html-tag)
  ;; The request-method override stage.  The allow-list is taken FROM the
  ;; stage rather than written out again here, so the two cannot disagree --
  ;; a method this helper will emit is by construction a method that stage
  ;; accepts.
  (:import-from #:clails/middleware/method-override-middleware
                #:+method-override-methods+
                #:*method-override-parameter*)
  (:export #:+form-methods+
           #:form-helper-error
           #:form-helper-error-message
           #:form-method-override-p
           #:escape-html
           #:hidden-field-tag
           #:form-authenticity-token
           #:csrf-token-tag
           #:method-override-tag
           #:form-tag
           #:end-form-tag
           #:form-with))

(in-package #:clails/view/form-helper)

;;; ------------------------------------------------------------------
;;; Form helpers that reach the CSRF token (M7-T2) and the method
;;; override field (M7-T3)
;;; ------------------------------------------------------------------
;;;
;;; What these are for
;;; ------------------
;;; With the CSRF verification stage enabled (M6), every POST / PUT / PATCH /
;;; DELETE has to carry the session's token, so every form in every template
;;; has to carry a hidden field holding it.  And because an HTML form can only
;;; be sent with GET or POST, a form that wants to update or delete has to
;;; declare the method it meant in a second hidden field, which the override
;;; stage (M9) reads and honours.  Writing either field by hand in every
;;; template is how a page ends up 403-ing or 404-ing, so the framework writes
;;; them: ask for :METHOD :PUT and the right two hidden fields appear.
;;;
;;; Why they can only be called from a template
;;; -------------------------------------------
;;; Two reasons, and they are different:
;;;
;;;   * the SESSION comes from the controller being rendered, which is
;;;     CLAILS/VIEW/VIEW-HELPER:*VIEW-CONTROLLER* -- bound only while a
;;;     template runs (M7-T1);
;;;   * the TOKEN comes from CSRF-TOKEN, which reads dynamic variables bound
;;;     by the CSRF stage for the duration of one request.  Outside that
;;;     stage those variables are unbound and the call fails with an
;;;     unbound-variable error from inside lack.
;;;
;;; Both hold for a template, because rendering happens at the INNERMOST stage
;;; of the stack (the controller stage) and the CSRF stage is outside it.  Any
;;; attempt to use these helpers somewhere else is turned into a
;;; FORM-HELPER-ERROR or a VIEW-CONTEXT-ERROR naming what is missing, rather
;;; than an unbound variable surfacing out of a dependency.
;;;
;;; Why the token field is rendered by lack's own CSRF-HTML-TAG
;;; ----------------------------------------------------------
;;; The field's NAME has to be the name the verifying stage reads.  That name
;;; is a configuration value (*CSRF-FORM-TOKEN*) which the stage captures when
;;; the middleware stack is BUILT, not per request -- so a helper that read
;;; the variable itself could legitimately read a different value from the one
;;; the running stage is comparing against, and write a form whose token field
;;; is invisible to the verifier.  CSRF-HTML-TAG reads the per-request
;;; variable the stage bound, i.e. the name that stage really is using, so the
;;; helper's output and the verifier cannot drift apart.  The same argument
;;; does not apply to the override field, whose name is read here from
;;; *METHOD-OVERRIDE-PARAMETER* -- that stage exposes no equivalent, and the
;;; name is the configured one in every normal setup.

(define-condition form-helper-error (error)
  ((message :initarg :message
            :initform "Form helper error"
            :reader form-helper-error-message))
  (:report (lambda (condition stream)
             (write-string (form-helper-error-message condition) stream)))
  (:documentation "Signalled when a form helper is asked for something it
   cannot produce: a method no form can be sent with, or a CSRF token while
   the verification stage is not running."))


;;; ------------------------------------------------------------------
;;; The methods a form may declare
;;; ------------------------------------------------------------------

(defparameter +form-methods+ (list* :get :post +method-override-methods+)
  "Every method a form built by these helpers may declare.

   :GET and :POST are what an HTML form can actually be sent with; the rest
   is taken verbatim from
   CLAILS/MIDDLEWARE/METHOD-OVERRIDE-MIDDLEWARE:+METHOD-OVERRIDE-METHODS+,
   the allow-list of the stage that honours the hidden override field.  It is
   taken rather than repeated so that the two lists cannot drift apart: a
   method added to (or removed from) the stage's allow-list is added to (or
   removed from) what a form can ask for, in the same release, with no second
   edit.")

(defun form-method-override-p (method)
  "True when METHOD has to be sent as a POST carrying a hidden override field.

   @param method [keyword] A method from +FORM-METHODS+
   @return [boolean] T for the methods of +METHOD-OVERRIDE-METHODS+
   "
  (and (member method +method-override-methods+ :test #'eq) t))

(defun normalize-method (method)
  "METHOD as one of the keywords of +FORM-METHODS+.

   A keyword, a string or any other symbol is accepted and compared without
   regard to case, because a template may well have the method as a string
   (\"put\", or a value out of the request).

   @param method [keyword] e.g. :PUT
   @param method [string] e.g. \"put\"
   @param method [symbol] e.g. 'PUT
   @return [keyword] The canonical keyword
   @condition form-helper-error Signalled for anything else, naming what is
     allowed -- a silently ignored method would be a form that reaches the
     wrong action, or none
   "
  (let ((keyword (typecase method
                   (keyword method)
                   (string (intern (string-upcase method) :keyword))
                   (symbol (intern (string-upcase (symbol-name method)) :keyword))
                   (t nil))))
    (unless (member keyword +form-methods+ :test #'eq)
      (error 'form-helper-error
             :message (format nil
                              "~S is not a method a form can declare. ~
                               Use one of ~{~S~^, ~}: :GET and :POST are sent ~
                               as themselves, the others are sent as a POST ~
                               carrying a hidden ~S field."
                              method +form-methods+ *method-override-parameter*)))
    keyword))

(defun wire-method (method)
  "The method the browser actually sends for a form declaring METHOD.

   :GET stays a GET; everything else is a POST -- an overridden method is a
   POST carrying the hidden field, never a GET, so that the real submission is
   verified by the CSRF stage.

   @param method [keyword] A canonical method from +FORM-METHODS+
   @return [string] \"get\" or \"post\", as the method= attribute spells it
   "
  (if (eq method :get) "get" "post"))


;;; ------------------------------------------------------------------
;;; Writing HTML
;;; ------------------------------------------------------------------

(defun escape-html (value)
  "VALUE as a string with the five HTML-significant characters escaped.

   Used for every attribute value and every field value these helpers write,
   so that a URL or a field value carrying a quote cannot close the attribute
   it sits in.

   @param value [t] Any object; printed with PRINC-TO-STRING unless it is
     already a string
   @return [string] The escaped text
   "
  (let ((text (if (stringp value) value (princ-to-string value))))
    (with-output-to-string (out)
      (loop for character across text
            do (case character
                 (#\& (write-string "&amp;" out))
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (#\" (write-string "&quot;" out))
                 (#\' (write-string "&#39;" out))
                 (t (write-char character out)))))))

(defun attribute-name (name)
  "NAME as an HTML attribute name.

   A keyword is downcased (:CLASS -> \"class\"), a string is taken as it is,
   so that an attribute whose name is not a valid Lisp symbol can still be
   written.

   @param name [keyword] e.g. :CLASS
   @param name [string] e.g. \"data-role\"
   @return [string] The attribute name
   "
  (if (stringp name)
      name
      (string-downcase (symbol-name name))))

(defun render-attributes (attributes)
  "ATTRIBUTES as HTML attribute text, each one preceded by a space.

   @param attributes [list] Plist of name -> value; a NIL value leaves the
     attribute out entirely (so a caller can pass an optional attribute
     without a conditional), and T writes it with its own name as the value,
     which is how a boolean attribute is spelled
   @return [string] e.g. \" class=\\\"edit\\\" id=\\\"todo-7\\\"\"
   "
  (with-output-to-string (out)
    (loop for (name value) on attributes by #'cddr
          do (cond ((null value))
                   ((eq value t)
                    (format out " ~A=\"~A\"" (attribute-name name)
                            (escape-html (attribute-name name))))
                   (t
                    (format out " ~A=\"~A\"" (attribute-name name)
                            (escape-html value)))))))

(defun hidden-field-tag (name value)
  "A hidden input field carrying VALUE under NAME.

   Both are escaped, and the field is a top-level field of whatever form it is
   written into -- which is what the CSRF stage and the override stage read
   (neither looks inside a nested structure), and which holds for a
   multipart/form-data form just as it does for a urlencoded one: a hidden
   input becomes a part of its own.

   @param name [string] Field name
   @param value [t] Field value
   @return [string] The <input> element
   "
  (format nil "<input type=\"hidden\" name=\"~A\" value=\"~A\">"
          (escape-html name)
          (escape-html value)))


;;; ------------------------------------------------------------------
;;; The CSRF token
;;; ------------------------------------------------------------------

(defun call-with-csrf-stage (helper thunk)
  "Call THUNK, turning \"the CSRF stage is not running\" into a clear error.

   CSRF-TOKEN and CSRF-HTML-TAG read dynamic variables that only the CSRF
   stage binds, and signal UNBOUND-VARIABLE when it is not on the stack.
   That can only happen when a template is rendered outside a request (or by
   a stack assembled by hand without the stage while
   *ENABLE-CSRF-MIDDLEWARE* is true), and the unbound-variable error names a
   variable internal to lack, which says nothing about what to do.

   @param helper [symbol] The helper being called, for the message
   @param thunk [function] What to call
   @return [t] Whatever THUNK returned
   @condition form-helper-error Signalled instead of UNBOUND-VARIABLE
   "
  (handler-case (funcall thunk)
    (unbound-variable ()
      (error 'form-helper-error
             :message (format nil
                              "~A: the CSRF token is not available here. ~
                               CLAILS/MIDDLEWARE/CSRF-MIDDLEWARE:*ENABLE-CSRF-MIDDLEWARE* ~
                               is true, but the verification stage is not ~
                               handling this request -- it binds the token ~
                               per request, so the token can only be read ~
                               from inside it (a template rendered by the ~
                               controller stage is inside it)."
                              helper)))))

(defun form-authenticity-token ()
  "This session's CSRF token, or NIL when verification is not enabled.

   Minting on demand: asking for the token is what puts one in the session if
   the session has none yet, and every call within one request returns the
   same string -- so a page carrying several forms gives them all the same
   token.

   NIL rather than an error when
   CLAILS/MIDDLEWARE/CSRF-MIDDLEWARE:*ENABLE-CSRF-MIDDLEWARE* is NIL: nothing
   verifies anything then, and a template that renders a form must not stop
   working because a project has not switched verification on.

   @return [string] The token
   @return [nil] When CSRF verification is disabled
   @condition view-context-error Signalled outside a template rendering
   @condition form-helper-error Signalled when verification is enabled but
     the stage is not running
   "
  (when *enable-csrf-middleware*
    (let ((session (current-session 'form-authenticity-token)))
      (call-with-csrf-stage 'form-authenticity-token
                            (lambda () (csrf-token session))))))

(defun csrf-token-tag ()
  "The hidden field carrying this session's CSRF token.

   The empty string when verification is disabled, so that it can be written
   into a template unconditionally.

   @return [string] The <input> element, or \"\"
   @condition view-context-error Signalled outside a template rendering
   @condition form-helper-error Signalled when verification is enabled but
     the stage is not running
   "
  (if *enable-csrf-middleware*
      (let ((session (current-session 'csrf-token-tag)))
        ;; lack's own renderer, deliberately -- see the commentary above for
        ;; why the field name must come from the running stage.
        (call-with-csrf-stage 'csrf-token-tag
                              (lambda () (csrf-html-tag session))))
      ""))


;;; ------------------------------------------------------------------
;;; The method override field
;;; ------------------------------------------------------------------

(defun method-override-tag (method)
  "The hidden field that tells the override stage which method was meant.

   The empty string for :GET and :POST, which need no field.  The value is
   the method's lower-case spelling, which the stage reads case-insensitively
   (CLAILS/MIDDLEWARE/METHOD-OVERRIDE-MIDDLEWARE:METHOD-OVERRIDE-VALUE).

   The field goes into the form's BODY, never into the action URL's query
   string: the stage reads the body only, on purpose -- a value in a query
   string would let any link turn into a deletion.

   @param method [keyword] e.g. :PUT, or a string/symbol spelling it
   @return [string] The <input> element, or \"\" for :GET and :POST
   @condition form-helper-error Signalled for a method no form can declare
   "
  (let ((method (normalize-method method)))
    (if (form-method-override-p method)
        (hidden-field-tag *method-override-parameter*
                          (string-downcase (symbol-name method)))
        "")))


;;; ------------------------------------------------------------------
;;; The form itself
;;; ------------------------------------------------------------------

(defun form-tag (&key url (method :post) multipart attributes)
  "The opening tag of a form, together with the hidden fields it needs.

   Everything the framework has to contribute to a form is here: the method
   the browser can actually send, the hidden override field when the declared
   method is not one of those, and the CSRF token field for anything but a
   GET.  The caller writes the form's own fields next, and closes it with
   END-FORM-TAG.

     <%= (form-tag :url \"/todos\" :method :post) %>
       <input type=\"text\" name=\"title\">
       <input type=\"submit\" value=\"Create\">
     <%= (end-form-tag) %>

     <%= (form-tag :url (format nil \"/todos/~A\" (view :id)) :method :put) %>
     <%= (form-tag :url (format nil \"/todos/~A\" (view :id)) :method :delete) %>
     <%= (form-tag :url \"/todos\" :method :get) %>

   @param url [string] The form's action.  NIL (the default) writes no action
     attribute, which submits to the current URL
   @param method [keyword] The method the form MEANS: one of +FORM-METHODS+.
     :PUT, :PATCH and :DELETE are sent as a POST carrying the override field
   @param multipart [boolean] T adds enctype=\"multipart/form-data\", for a
     form with a file input.  The token still travels as a top-level field
   @param attributes [list] Plist of further attributes for the <form>
     element, e.g. (:class \"edit\" :id \"todo-form\")
   @return [string] The <form> element's opening tag and its hidden fields
   @condition form-helper-error Signalled for a method no form can declare
   @condition view-context-error Signalled outside a template rendering, when
     a token would have to be written
   "
  (let* ((method (normalize-method method))
         (hidden (remove ""
                         (list (method-override-tag method)
                               ;; A GET form carries no token: the CSRF stage
                               ;; does not verify safe methods, and a token in
                               ;; a GET form would be appended to the URL as a
                               ;; query parameter -- landing in browser
                               ;; history, in the Referer header and in every
                               ;; access log.
                               (if (eq method :get) "" (csrf-token-tag)))
                         :test #'string=)))
    (format nil "<form~@[ action=\"~A\"~] method=\"~A\"~@[ enctype=\"~A\"~]~A>~{~%~A~}"
            (and url (escape-html url))
            (wire-method method)
            (and multipart "multipart/form-data")
            (render-attributes attributes)
            hidden)))

(defun end-form-tag ()
  "The closing tag of a form opened with FORM-TAG.

   A function rather than a literal \"</form>\" in the template so that the
   two halves of a form read as a pair, and so that a later addition to the
   end of a form has one place to go.

   @return [string] \"</form>\"
   "
  "</form>")

(defun form-with (&key url (method :post) multipart attributes body)
  "A whole form: FORM-TAG, BODY and END-FORM-TAG.

   The one-expression spelling, for a form whose fields are already a string
   -- a delete button, say, which is a form with nothing in it but its submit
   control:

     <%= (form-with :url (format nil \"/todos/~A\" (view :id))
                    :method :delete
                    :body \"<input type=\\\"submit\\\" value=\\\"Delete\\\">\") %>

   A form with several fields is usually clearer written out between FORM-TAG
   and END-FORM-TAG.

   @param url [string] See FORM-TAG
   @param method [keyword] See FORM-TAG
   @param multipart [boolean] See FORM-TAG
   @param attributes [list] See FORM-TAG
   @param body [string] The form's own markup, written between the tags
   @param body [list] A list of strings, concatenated in order
   @param body [nil] An empty form
   @return [string] The complete <form> element
   @condition form-helper-error Signalled for a method no form can declare
   @condition view-context-error Signalled outside a template rendering, when
     a token would have to be written
   "
  (format nil "~A~{~%~A~}~%~A"
          (form-tag :url url
                    :method method
                    :multipart multipart
                    :attributes attributes)
          (cond ((null body) '())
                ((listp body) body)
                (t (list body)))
          (end-form-tag)))
