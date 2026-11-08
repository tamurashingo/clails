(in-package #:cl-user)
(defpackage #:clails-test/view/form-helper
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf
        #:clails-test/support/view)
  ;; The form helpers, imported the way a generated view package would import
  ;; them: the first of the two calling conventions, which is what lets the
  ;; calls below read as FORM-TAG rather than CLAILS/VIEW/FORM-HELPER:FORM-TAG.
  ;; The other convention (naming the package, importing nothing) is exercised
  ;; by clails-test/view/form-helper-roundtrip.
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
  (:import-from #:clails/view/view-helper
                #:view-context-error)
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:*csrf-form-token*)
  (:import-from #:clails/middleware/method-override-middleware
                #:+method-override-methods+
                #:+method-override-default-parameter+
                #:*method-override-parameter*
                #:method-override-value))
(in-package #:clails-test/view/form-helper)


;;; M7-T2 and M7-T3 seen from close up: what the helpers put in the string.
;;;
;;; Most of it has to be asserted from INSIDE a middleware stack, because the
;;; CSRF token is only reachable while the verification stage is handling the
;;; request (it binds the token's session key per request).  So the shape of
;;; every test that involves a token is: build a session -> CSRF stack whose
;;; innermost application calls the helper and answers its output, ask for one
;;; page, and assert on the body that comes back.  That the form then actually
;;; works -- posted back, verified, routed to an action -- is
;;; clails-test/view/form-helper-roundtrip's business.


;;; ------------------------------------------------------------------
;;; Calling a helper inside a running stack
;;; ------------------------------------------------------------------

(defvar *form-thunk* (lambda () "")
  "What the innermost application of the stack renders.  Rebound per test by
   RENDER-IN-STACK, so that one stack shape serves every assertion.")

(defun form-render-app (env)
  "An application answering whatever *FORM-THUNK* returns.

   The thunk is called with the controller of this request installed as the
   controller being rendered -- which is exactly the binding
   CLAILS/VIEW/RENDERER:RENDER establishes, minus the template."
  (let ((controller (controller-for-env env)))
    (list 200
          (list :content-type "text/html")
          (list (with-view-context (controller) (funcall *form-thunk*))))))

(defun render-in-stack (thunk &key (csrf t) form-token parameter)
  "Call THUNK inside a session -> CSRF stack and return the string it made.

   @param thunk [function] Called with no arguments, inside the rendering
     binding, while the CSRF stage is handling a request
   @param csrf [boolean] Whether the verification stage is enabled
   @param form-token [string] Override the token field's name
   @param parameter [string] Override the override field's name
   @return [string] Whatever THUNK returned
   "
  (let ((*form-thunk* thunk)
        (*method-override-parameter* (or parameter *method-override-parameter*)))
    (with-csrf-stack (app :inner #'form-render-app
                          :csrf csrf
                          :form-token form-token
                          :store (make-memory-session-store))
      (response-body-string (client-request (make-session-client app) "/")))))

(defun render-outside-stack (thunk &key (csrf nil))
  "Call THUNK inside the rendering binding but with no middleware stack.

   This is \"a template rendered outside a request\": the controller is there,
   so the session is reachable, but no CSRF stage is running."
  (let ((*enable-csrf-middleware* csrf))
    (with-view-context ((make-view-controller)) (funcall thunk))))


;;; ------------------------------------------------------------------
;;; The token field (M7-T2)
;;; ------------------------------------------------------------------

(deftest csrf-token-tag-test
  (testing "the tag carries the session's token, under the configured name"
    (let* ((rendered (render-in-stack (lambda ()
                                        (format nil "~A~%~A"
                                                (csrf-token-tag)
                                                (form-authenticity-token)))))
           (lines (uiop:split-string rendered :separator (string #\Newline)))
           (html (first lines))
           (token (second lines)))
      (ok (plusp (length token)) "the session has a token")
      (ok (search token html) "the token value is in the rendered tag")
      (ok (equal (form-hidden-field html *csrf-form-token*) token)
          "and it is the value of a hidden field named _csrf_token")
      (ok (search "type=\"hidden\"" html) "the field is hidden")))

  (testing "the field name follows the configured one"
    (let ((html (render-in-stack (lambda () (csrf-token-tag))
                                 :form-token "authenticity_token")))
      (ok (stringp (form-hidden-field html "authenticity_token")))
      (ok (null (form-hidden-field html "_csrf_token"))
          "the default name is not written when another one is configured")))

  (testing "every call within one request returns the same token"
    ;; What lets a page carry several forms.
    (let* ((rendered (render-in-stack (lambda ()
                                       (format nil "~A~A~A"
                                               (csrf-token-tag)
                                               (csrf-token-tag)
                                               (csrf-token-tag)))))
           (fields (form-hidden-fields rendered)))
      (ok (= (length fields) 3))
      (ok (= 1 (length (remove-duplicates (mapcar #'cdr fields) :test #'string=)))
          "all three carry one and the same token")))

  (testing "with verification disabled the tag is empty and the token is NIL"
    ;; A template must not stop rendering because a project has not switched
    ;; verification on.
    (ok (string= (render-in-stack (lambda () (csrf-token-tag)) :csrf nil) ""))
    (ok (string= (render-in-stack (lambda ()
                                    (princ-to-string (null (form-authenticity-token))))
                                  :csrf nil)
                 "T")))

  (testing "outside a rendering the helpers signal VIEW-CONTEXT-ERROR"
    (let ((*enable-csrf-middleware* t))
      (ok (signals (csrf-token-tag) 'view-context-error))
      (ok (signals (form-authenticity-token) 'view-context-error))))

  (testing "inside a rendering but outside the stage they signal FORM-HELPER-ERROR"
    ;; The pitfall this helper exists to hide: lack's CSRF-TOKEN reads a
    ;; variable the stage binds per request, so off the stack it is an unbound
    ;; variable naming something internal to lack.  It is turned into an error
    ;; that says what is wrong.
    (ok (signals (render-outside-stack (lambda () (csrf-token-tag)) :csrf t)
                 'form-helper-error))
    (ok (signals (render-outside-stack (lambda () (form-authenticity-token)) :csrf t)
                 'form-helper-error))
    (ok (search "not available here"
                (handler-case (render-outside-stack (lambda () (csrf-token-tag)) :csrf t)
                  (form-helper-error (c) (princ-to-string c)))))))


;;; ------------------------------------------------------------------
;;; The form tag (M7-T2)
;;; ------------------------------------------------------------------

(deftest form-tag-test
  (testing "a POST form carries the token field"
    (let ((html (render-in-stack (lambda () (form-tag :url "/todos" :method :post)))))
      (ok (string= (form-action html) "/todos"))
      (ok (string= (form-method-attribute html) "post"))
      (ok (stringp (form-hidden-field html *csrf-form-token*))
          "the token field is there")
      (ok (null (form-hidden-field html *method-override-parameter*))
          "and no override field, because POST needs none")))

  (testing "a GET form carries NO token field"
    ;; Safe methods are not verified, and a token in a GET form would be
    ;; appended to the URL -- landing in history, in Referer and in every
    ;; access log.
    (let ((html (render-in-stack (lambda () (form-tag :url "/todos" :method :get)))))
      (ok (string= (form-method-attribute html) "get"))
      (ok (null (form-hidden-fields html)) "no hidden fields at all")
      (ok (not (search *csrf-form-token* html))
          "the token's field name does not appear anywhere in the form")))

  (testing "POST is the default method"
    (let ((html (render-in-stack (lambda () (form-tag :url "/todos")))))
      (ok (string= (form-method-attribute html) "post"))
      (ok (stringp (form-hidden-field html *csrf-form-token*)))))

  (testing "a multipart form carries the token as a top-level field"
    ;; A1: a hidden input is a part of its own, so the token still arrives as
    ;; a top-level body parameter -- which is the only place the stage looks.
    (let ((html (render-in-stack (lambda ()
                                   (form-tag :url "/todos" :method :post
                                             :multipart t)))))
      (ok (string= (form-enctype html) "multipart/form-data"))
      (ok (stringp (form-hidden-field html *csrf-form-token*)))))

  (testing "without :url no action attribute is written"
    ;; Which submits to the current URL -- the useful default for a form that
    ;; re-renders itself.
    (let ((html (render-in-stack (lambda () (form-tag :method :post)))))
      (ok (null (form-action html)))
      (ok (string= (form-method-attribute html) "post"))))

  (testing "further attributes are written, and NIL ones are left out"
    (let ((html (render-in-stack (lambda ()
                                   (form-tag :url "/todos" :method :post
                                             :attributes '(:class "edit"
                                                           :id "todo-form"
                                                           :data-remote nil))))))
      (ok (string= (form-attribute html "class") "edit"))
      (ok (string= (form-attribute html "id") "todo-form"))
      (ok (not (search "data-remote" html)))))

  (testing "attribute values are escaped"
    (let ((html (render-in-stack (lambda ()
                                   (form-tag :url "/search?q=a&b=\"c\""
                                             :method :get)))))
      (ok (search "&amp;" html) "the ampersand is escaped")
      (ok (search "&quot;" html) "the quote is escaped")
      (ok (string= (form-action html) "/search?q=a&b=\"c\"")
          "and it reads back as the URL that was passed")))

  (testing "a method no form can declare is refused, loudly"
    (dolist (method '(:head :options :trace :connect "putt" "" nil 7))
      (ok (signals (render-in-stack (lambda () (form-tag :url "/x" :method method)))
                   'form-helper-error)
          (format nil "~S is refused" method))))

  (testing "the method may be spelled as a string or a symbol, in any case"
    (dolist (spelling '("put" "PUT" "Put" put))
      (let ((html (render-in-stack (lambda () (form-tag :url "/x" :method spelling)))))
        (ok (string= (form-hidden-field html *method-override-parameter*) "put")
            (format nil "~S is read as :PUT" spelling))))))

(deftest form-with-test
  (testing "FORM-WITH is the opening tag, the body and the closing tag"
    (let ((html (render-in-stack (lambda ()
                                   (form-with :url "/todos/7"
                                              :method :delete
                                              :body "<input type=\"submit\" value=\"Delete\">")))))
      (ok (string= (form-method-attribute html) "post"))
      (ok (string= (form-hidden-field html *method-override-parameter*) "delete"))
      (ok (stringp (form-hidden-field html *csrf-form-token*)))
      (ok (search "value=\"Delete\"" html))
      (ok (search "</form>" html))
      (ok (> (search "</form>" html) (search "value=\"Delete\"" html))
          "the body comes before the closing tag")))

  (testing "a list body is written in order, and no body is an empty form"
    (let ((html (render-in-stack (lambda ()
                                   (form-with :url "/todos" :method :post
                                              :body '("<i>1</i>" "<i>2</i>"))))))
      (ok (< (search "<i>1</i>" html) (search "<i>2</i>" html))))
    (let ((html (render-in-stack (lambda () (form-with :url "/todos" :method :post)))))
      (ok (search "</form>" html))))

  (testing "END-FORM-TAG closes a form opened with FORM-TAG"
    (ok (string= (end-form-tag) "</form>"))))


;;; ------------------------------------------------------------------
;;; The override field (M7-T3)
;;; ------------------------------------------------------------------

(deftest method-override-tag-test
  (testing "the accepted methods are exactly M9's allow-list plus GET and POST"
    ;; The completion condition that the two lists cannot drift apart.  The
    ;; helper derives its list from the stage's, so this asserts the
    ;; derivation as well as the result.
    ;; SET-DIFFERENCE does not promise an order, so the comparison is a set
    ;; comparison -- the order of +FORM-METHODS+ is not part of the contract.
    (ok (null (set-exclusive-or (set-difference +form-methods+ '(:get :post))
                                +method-override-methods+))
        "nothing but GET and POST is accepted beyond the stage's allow-list")
    (ok (null (set-difference +method-override-methods+ +form-methods+))
        "and every method the stage accepts can be asked for")
    (ok (null (set-exclusive-or +form-methods+
                                (append '(:get :post) +method-override-methods+)))
        "the two sets agree exactly")
    (dolist (method +method-override-methods+)
      (ok (form-method-override-p method)
          (format nil "~S needs the override field" method)))
    (ok (not (form-method-override-p :get)))
    (ok (not (form-method-override-p :post))))

  (testing "every method of the allow-list produces a value the stage accepts"
    ;; Not just "the same list": the value written into the field is read back
    ;; through the stage's own parser, so a spelling the helper writes and the
    ;; stage would not recognise is a failure here.
    (dolist (method +method-override-methods+)
      (let ((tag (method-override-tag method)))
        (ok (string= (form-hidden-field tag +method-override-default-parameter+)
                     (string-downcase (symbol-name method))))
        (ok (eq (method-override-value
                 (form-hidden-field tag +method-override-default-parameter+))
                method)
            (format nil "the stage reads the written value back as ~S" method)))))

  (testing "GET and POST produce no field"
    (ok (string= (method-override-tag :get) ""))
    (ok (string= (method-override-tag :post) "")))

  (testing "a form declaring an overridden method is still sent as a POST"
    ;; The important half: the REAL method is POST, so the CSRF stage verifies
    ;; the submission before anything is rewritten.
    (dolist (method +method-override-methods+)
      (let ((html (render-in-stack (lambda () (form-tag :url "/todos/7" :method method)))))
        (ok (string= (form-method-attribute html) "post")
            (format nil "a ~S form declares method=\"post\"" method))
        (ok (string= (form-hidden-field html *method-override-parameter*)
                     (string-downcase (symbol-name method))))
        (ok (stringp (form-hidden-field html *csrf-form-token*))
            "and it carries the token as well"))))

  (testing "the forged value goes in the body, never in the query string"
    ;; The stage reads the body only, on purpose: a value in a query string
    ;; would turn any link into a deletion.  So the helper must not put it in
    ;; the action URL.
    (let ((html (render-in-stack (lambda () (form-tag :url "/todos/7" :method :delete)))))
      (ok (string= (form-action html) "/todos/7")
          "the action is the plain resource path")
      (ok (not (find #\? (form-action html)))
          "no query string at all")
      (ok (not (search *method-override-parameter* (form-action html)))
          "and the field name is nowhere in the action")
      (ok (string= (form-hidden-field html *method-override-parameter*) "delete")
          "it is a hidden field of the body instead")))

  (testing "the field name follows the configured one"
    (let ((html (render-in-stack (lambda () (form-tag :url "/x" :method :put))
                                 :parameter "_verb")))
      (ok (string= (form-hidden-field html "_verb") "put"))
      (ok (null (form-hidden-field html "_method")))))

  (testing "the override field comes before the token field"
    ;; Nothing depends on the order, but both have to be inside the form and
    ;; immediately after its opening tag, which is what this fixes in place.
    (let* ((html (render-in-stack (lambda () (form-tag :url "/x" :method :put))))
           (names (mapcar #'car (form-hidden-fields html))))
      (ok (equal names (list *method-override-parameter* *csrf-form-token*)))))

  (testing "a method no form can declare is refused"
    (ok (signals (method-override-tag :head) 'form-helper-error))
    (ok (search "not a method a form can declare"
                (handler-case (method-override-tag :head)
                  (form-helper-error (c) (princ-to-string c)))))))


;;; ------------------------------------------------------------------
;;; The small pieces
;;; ------------------------------------------------------------------

(deftest escaping-test
  (testing "the five HTML-significant characters are escaped"
    (ok (string= (escape-html "<a href=\"x\">&'") "&lt;a href=&quot;x&quot;&gt;&amp;&#39;")))

  (testing "a non-string is printed first"
    (ok (string= (escape-html 7) "7"))
    (ok (string= (escape-html :put) "PUT")))

  (testing "a hidden field escapes both its name and its value"
    (let ((tag (hidden-field-tag "a\"b" "<script>")))
      (ok (string= tag
                   "<input type=\"hidden\" name=\"a&quot;b\" value=\"&lt;script&gt;\">"))
      (ok (string= (form-hidden-field tag "a\"b") "<script>")
          "and reads back as what was passed"))))
