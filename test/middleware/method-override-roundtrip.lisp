(in-package #:cl-user)
(defpackage #:clails-test/middleware/method-override-roundtrip
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf
        #:clails-test/support/method-override
        #:clails-test/support/routing)
  (:import-from #:clails/middleware/method-override-middleware
                #:+method-override-methods+
                #:*enable-method-override*
                #:*lack-middleware-clails-method-override*)
  (:import-from #:clails/middleware/csrf-middleware
                #:csrf-token)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+)
  (:import-from #:clails/controller/base-controller
                #:<rest-controller>
                #:set-response
                #:session
                #:env
                #:param
                #:resources
                #:do-get
                #:do-post
                #:do-put
                #:do-delete))
(in-package #:clails-test/middleware/method-override-roundtrip)


;;; The override stage seen from the outside: requests go through a real
;;; stack -- session, CSRF verification, the override stage, and for the
;;; routing tests the controller stage as well -- and only the response is
;;; looked at.
;;;
;;; Two things are judged here.  The safety of the mechanism, with a matrix of
;;; method x where the forged value sits (body or query string) x whether a
;;; CSRF token is carried; and the purpose of the mechanism, namely that a
;;; form's POST reaches the update and destroy routes of a resource and that
;;; the action it reaches can read the id out of the path.


;;; ------------------------------------------------------------------
;;; An application that reports what reached it
;;; ------------------------------------------------------------------

(defun make-probe-app (&key (token t))
  "An application answering \"probe <method> <token>\".

   The method is what makes a rewrite visible from outside the stack: the
   body says POST or DELETE, whichever method reached the bottom.  The token
   is the session's CSRF token, which only an action below an ENABLED CSRF
   stage can ask for -- hence :TOKEN NIL for the stacks that have no CSRF
   stage.

   The \"probe \" prefix is what tells this answer apart from a rejection's
   body, so that a test cannot read a method out of a 403."
  (lambda (env)
    (list 200
          (list :content-type "text/plain")
          (list (format nil "probe ~A ~A"
                        (getf env :request-method)
                        (if token
                            (csrf-token (getf env :lack.session))
                            ""))))))

(defparameter +probe-prefix+ "probe ")

(defun probe-fields (response)
  "The method and the token the probe application reported, as
   (values METHOD TOKEN).

   Both are NIL when RESPONSE did not come from the probe application -- a
   rejection, or a response from the routing stage."
  (let ((body (response-body-string response)))
    (when (and (stringp body)
               (>= (length body) (length +probe-prefix+))
               (string= +probe-prefix+ body :end2 (length +probe-prefix+)))
      (let* ((rest (subseq body (length +probe-prefix+)))
             (space (position #\Space rest))
             (token (subseq rest (1+ space))))
        (values (intern (string-upcase (subseq rest 0 space)) :keyword)
                (when (plusp (length token)) token))))))

(defun probe-method (response)
  "The request method that reached the bottom of the stack, or NIL."
  (values (probe-fields response)))

(defun probe-token (response)
  "The session's CSRF token, as the probe application read it, or NIL."
  (nth-value 1 (probe-fields response)))

(defmacro with-probe-client ((client token &key (csrf t) (override t) (strict nil)
                                                (parameter nil))
                             &body body)
  "Open a session against a session -> CSRF -> override -> probe stack, bind
   CLIENT and the token its first request minted, and run BODY.

   The loggers are rebound to none, so the audit records the rejections leave
   do not print into the middle of the test report."
  `(with-no-loggers
     (with-override-stack (app :csrf ,csrf
                               :override ,override
                               :strict ,strict
                               :parameter ,parameter
                               :inner (make-probe-app :token ,csrf))
       (let* ((,client (open-session app))
              (,token (probe-token (client-request ,client "/"))))
         ,@body))))


;;; ------------------------------------------------------------------
;;; The matrix: method x where the value sits x token
;;; ------------------------------------------------------------------

(deftest method-override-matrix-test
  ;; Each row is (REAL-METHOD WHERE TOKEN? EXPECTED-STATUS EXPECTED-METHOD),
  ;; with WHERE saying whether "delete" is put in the body or in the query
  ;; string.  EXPECTED-METHOD is what the application at the bottom of the
  ;; stack must report, or NIL when it must not be reached at all.
  (testing "the whole matrix, once for each forgeable method"
    ;; EXPECTED is :REAL when the request must keep the method it was sent
    ;; with, :FORGED when the forgery must have been honoured, and NIL when
    ;; the application must not be reached at all.
    (with-probe-client (client token)
      (dolist (forged +method-override-methods+)
        (let ((spelling (string-downcase (symbol-name forged))))
          (dolist (row '((:get  :body  t   200 :real)
                         (:get  :body  nil 200 :real)
                         (:get  :query t   200 :real)
                         (:get  :query nil 200 :real)
                         (:post :body  t   200 :forged)
                         (:post :body  nil 403 nil)
                         (:post :query t   200 :real)
                         (:post :query nil 403 nil)))
            (destructuring-bind (method where carry-token status expected) row
              (let* ((label (format nil "~A forging ~A in the ~(~A~) with ~:[no ~;a ~]token"
                                    method forged where carry-token))
                     (wanted (case expected
                               (:real method)
                               (:forged forged)
                               (t nil)))
                     (response (client-request
                                client
                                (if (eq where :query) (query-uri "/" spelling) "/")
                                :method method
                                :content (form-body :method (when (eq where :body) spelling)
                                                    :token (when carry-token token)))))
                (ok (eql (response-status response) status) label)
                (ok (eq (probe-method response) wanted)
                    (format nil "~A: the application saw ~A" label wanted)))))))))

  (testing "a forged value in the query string is ignored even on a verified POST"
    ;; The pre-M9 reading of the value was over the request's PARAMETERS,
    ;; which are the query string and the body added together.  Reading the
    ;; body alone is the same rule the CSRF token follows.
    (with-probe-client (client token)
      (let ((response (client-request client (query-uri "/" "delete")
                                      :method :post
                                      :content (form-body :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (probe-method response) :post)))))

  (testing "the body wins over the query string when the two disagree"
    (with-probe-client (client token)
      (let ((response (client-request client (query-uri "/" "put")
                                      :method :post
                                      :content (form-body :method "delete"
                                                          :token token))))
        (ok (eq (probe-method response) :delete))))))


(deftest method-override-bypass-regression-test
  ;; THE regression test of M9: a request that forges a SAFE method must not
  ;; be able to skip verification.  With the stage outside the CSRF stage
  ;; this was measured to work -- the CSRF stage saw GET, did not verify, and
  ;; served the request.  Inside, the token is checked against the real
  ;; method (POST) before anything is rewritten, and GET is not in the
  ;; allow-list in the first place.
  (testing "forging GET without a token is refused"
    (with-probe-client (client token)
      (declare (ignore token))
      (dolist (spelling '("get" "GET" "Get"))
        (let ((response (client-request client "/"
                                        :method :post
                                        :content (form-body :method spelling))))
          (ok (eql (response-status response) +forbidden-status+)
              (format nil "POST forging ~S with no token is a 403" spelling))
          (ok (null (probe-method response))
              "and nothing below the CSRF stage ran")))))

  (testing "forging a state-changing method without a token is refused too"
    ;; The other half of the same statement: verification happens against the
    ;; request's real method, so no forged value can avoid it.
    (with-probe-client (client token)
      (declare (ignore token))
      (dolist (spelling '("put" "patch" "delete"))
        (let ((response (client-request client "/"
                                        :method :post
                                        :content (form-body :method spelling))))
          (ok (eql (response-status response) +forbidden-status+)
              (format nil "POST forging ~S with no token is a 403" spelling))))))

  (testing "forging GET WITH a valid token does not rewrite anything either"
    ;; Not a bypass any more (the token was checked), but still refused by the
    ;; allow-list: the request stays a POST.
    (with-probe-client (client token)
      (let ((response (client-request client "/"
                                      :method :post
                                      :content (form-body :method "get" :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (probe-method response) :post)))))

  (testing "a GET carrying a forged value is never rewritten, body or query"
    ;; Condition 1 of the four.  Without it, appending ?_method=delete to any
    ;; link would delete the resource it points at -- measured.
    (with-probe-client (client token)
      (dolist (method '("delete" "put" "patch"))
        (let ((from-query (client-request client (query-uri "/" method)))
              (from-body (client-request client "/"
                                         :method :get
                                         :content (form-body :method method
                                                             :token token))))
          (ok (eq (probe-method from-query) :get)
              (format nil "?_method=~A does not rewrite a GET" method))
          (ok (eq (probe-method from-body) :get)
              (format nil "_method=~A in a GET body does not rewrite it" method)))))))


(deftest method-override-accepted-values-test
  (testing "every method of the allow-list can be forged by a verified POST"
    (with-probe-client (client token)
      (dolist (method +method-override-methods+)
        (let* ((spelling (string-downcase (symbol-name method)))
               (response (client-request client "/"
                                         :method :post
                                         :content (form-body :method spelling
                                                             :token token))))
          (ok (eql (response-status response) 200))
          (ok (eq (probe-method response) method)
              (format nil "~S becomes ~A" spelling method))))))

  (testing "the spelling is case-insensitive"
    (with-probe-client (client token)
      (dolist (spelling '("PUT" "Put" "pAtCh" "DELETE"))
        (ok (eq (probe-method (client-request client "/"
                                              :method :post
                                              :content (form-body :method spelling
                                                                  :token token)))
                (intern (string-upcase spelling) :keyword))
            (format nil "~S is recognised" spelling)))))

  (testing "an unknown value, an empty one or no value at all passes through"
    ;; Condition 4 of the four: nothing signals and nothing is refused on
    ;; account of the field.  The request stays the POST it was.
    (with-probe-client (client token)
      (dolist (value '("" " " "putt" "destroy" "head" "options" "post" "0" "DELETE "))
        (let ((response (client-request client "/"
                                        :method :post
                                        :content (form-body :method value
                                                            :token token))))
          (ok (eql (response-status response) 200)
              (format nil "~S is not refused" value))
          (ok (eq (probe-method response) :post)
              (format nil "~S does not rewrite anything" value))))
      ;; no field at all
      (ok (eq (probe-method (client-request client "/"
                                            :method :post
                                            :content (form-body :token token)))
              :post))))

  (testing "a multipart body's value is read as well"
    ;; A multipart parameter arrives as a list (the value plus the part's
    ;; metadata), which is why the stage takes the first element.  The case
    ;; worth proving is a form that also uploads a file.
    (with-probe-client (client token)
      (let ((response (client-request client "/"
                                      :method :post
                                      :content (multipart-content
                                                token
                                                :extra '(("_method" . "delete")))
                                      :headers (multipart-headers))))
        (ok (eql (response-status response) 200))
        (ok (eq (probe-method response) :delete)))))

  (testing "a JSON body's top-level value is read as well"
    ;; Not what a form sends, but it is a body, and the stage reads bodies
    ;; rather than form bodies.  The token is read from the same place.
    (with-probe-client (client token)
      (let ((response (client-request client "/"
                                      :method :post
                                      :content (json-content token
                                                             :extra '(("_method" . "put")))
                                      :headers (json-headers))))
        (ok (eql (response-status response) 200))
        (ok (eq (probe-method response) :put)))))

  (testing "the field name is configurable, and only that name is read"
    (with-probe-client (client token :parameter "_verb")
      (ok (eq (probe-method (client-request client "/"
                                            :method :post
                                            :content (form-body :method "delete"
                                                                :parameter "_verb"
                                                                :token token)))
              :delete))
      (ok (eq (probe-method (client-request client "/"
                                            :method :post
                                            :content (form-body :method "delete"
                                                                :parameter "_method"
                                                                :token token)))
              :post)
          "the default name is not read when another one is configured"))))


(deftest method-override-disabled-test
  (testing "the stage can be switched off, and then nothing is rewritten"
    (with-probe-client (client token :override nil)
      (let ((response (client-request client "/"
                                      :method :post
                                      :content (form-body :method "delete"
                                                          :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (probe-method response) :post)))))

  (testing "a disabled stage is not a stage at all: it returns the application untouched"
    ;; Nothing is parsed, nothing is judged, and no configuration check is
    ;; made, because there is no wrapper left to make one.
    (let ((*enable-method-override* nil))
      (ok (eq (funcall *lack-middleware-clails-method-override* #'ok-app) #'ok-app))))

  (testing "an unparseable body does not become a server failure"
    ;; With CSRF enabled such a request is refused one stage further out (the
    ;; CSRF body guard).  With CSRF off, nothing has looked at the body yet
    ;; when this stage reads it, and the stage must not turn a broken body
    ;; into a failed request on its own -- it simply finds no value.
    ;; (The session stage parses the body too, which is why that stage is off
    ;; here; closing that hole belongs to the goal that owns request-wide
    ;; error handling, see src/middleware/csrf-middleware.lisp.)
    (with-no-loggers
      (with-override-stack (app :session nil :csrf nil :inner (make-probe-app :token nil))
        (let ((response (call-app app "/" :method :post
                                          :content "{\"_method\": \"delete\""
                                          :headers (json-headers))))
          (ok (eql (response-status response) 200))
          (ok (eq (probe-method response) :post)))))))


;;; ------------------------------------------------------------------
;;; What the stage is for: a form reaching a resource's routes
;;; ------------------------------------------------------------------

(defclass <todo-controller> (<rest-controller>)
  ())

(defgeneric index (controller))
(defmethod index ((controller <todo-controller>))
  ;; "The page with the form on it": the action asks the CSRF stage for the
  ;; token, which is only possible because that stage is outside routing.
  (set-response controller (list (cons "action" "index")
                                 (cons "token" (csrf-token (session controller))))))

(defgeneric create (controller))
(defmethod create ((controller <todo-controller>))
  (set-response controller (list (cons "action" "create"))))

(defgeneric show (controller))
(defmethod show ((controller <todo-controller>))
  (set-response controller (list (cons "action" "show")
                                 (cons "id" (param controller "id")))))

(defgeneric update (controller))
(defmethod update ((controller <todo-controller>))
  (set-response controller (list (cons "action" "update")
                                 (cons "id" (param controller "id"))
                                 (cons "method" (princ-to-string
                                                 (getf (env controller)
                                                       :request-method))))))

(defgeneric destroy (controller))
(defmethod destroy ((controller <todo-controller>))
  (set-response controller (list (cons "action" "destroy")
                                 (cons "id" (param controller "id")))))

(defparameter +todo-controller+
  "clails-test/middleware/method-override-roundtrip::<todo-controller>")

(defun action-of (response)
  "The action name the controller reported in its JSON response."
  (let ((body (response-body-string response)))
    (cond ((search "\"index\"" body) :index)
          ((search "\"create\"" body) :create)
          ((search "\"show\"" body) :show)
          ((search "\"update\"" body) :update)
          ((search "\"destroy\"" body) :destroy)
          (t nil))))

(defun token-from-listing (response)
  "The token the index action rendered into its JSON response."
  (let* ((body (response-body-string response))
         (start (search "\"token\":\"" body)))
    (when start
      (let* ((from (+ start (length "\"token\":\"")))
             (to (position #\" body :start from)))
        (subseq body from to)))))

(defmacro with-routed-client ((client token routes &key (override t)) &body body)
  "Install ROUTES, build session -> CSRF -> override -> routing, open a
   session by asking for the listing, and bind CLIENT and the token that
   listing rendered."
  `(with-routing-table (,routes)
     (with-no-loggers
       (with-override-stack (app :override ,override
                                 :inner #'not-found-app
                                 :inner-middleware (list *lack-middleware-clails-controller*))
         (let* ((,client (open-session app))
                (,token (token-from-listing (client-request ,client "/todos"))))
           ,@body)))))

(deftest method-override-resource-routes-test
  (testing "a form's POST reaches the update route of a resource"
    ;; G13, the point of the whole milestone: before M9 this POST matched no
    ;; route at all (route matching looks at the real method, which is POST,
    ;; and /todos/:id has no POST route), so the form answered 404.
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (ok (stringp token) "the listing rendered a token")
      (let ((response (client-request client "/todos/7"
                                      :method :post
                                      :content (form-body :method "put" :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (action-of response) :update))
        (ok (search "\"id\":\"7\"" (response-body-string response))
            "and the action read the id out of the path")
        (ok (search "\"method\":\"PUT\"" (response-body-string response))
            "the action sees the rewritten method"))))

  (testing "a form's POST reaches the destroy route of a resource"
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (let ((response (client-request client "/todos/9"
                                      :method :post
                                      :content (form-body :method "delete" :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (action-of response) :destroy))
        (ok (search "\"id\":\"9\"" (response-body-string response))))))

  (testing "a form's POST reaches the partial-update route of a resource"
    ;; M9-T2: PATCH was verified by the CSRF stage but no generated route
    ;; declared it, so it was a method that could be checked and never
    ;; reached.  It is the eighth route RESOURCES now generates.
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (let ((response (client-request client "/todos/3"
                                      :method :post
                                      :content (form-body :method "patch" :token token))))
        (ok (eql (response-status response) 200))
        (ok (eq (action-of response) :update))
        (ok (search "\"id\":\"3\"" (response-body-string response)))
        (ok (search "\"method\":\"PATCH\"" (response-body-string response))
            "the action can tell a partial update from a replacement"))))

  (testing "without the override the same form answers 404"
    ;; What the defect looked like, kept as the contrast: nothing else about
    ;; the request changes.
    (with-routed-client (client token (resources "todos" +todo-controller+) :override nil)
      (ok (eql (response-status (client-request client "/todos/7"
                                                :method :post
                                                :content (form-body :method "put"
                                                                    :token token)))
               404))))

  (testing "a forged form POST with no token never reaches an action"
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (declare (ignore token))
      (let ((response (client-request client "/todos/7"
                                      :method :post
                                      :content (form-body :method "delete"))))
        (ok (eql (response-status response) +forbidden-status+))
        (ok (null (action-of response))))))

  (testing "the real methods still reach the same routes"
    ;; Nothing about an API client sending a real PUT / PATCH / DELETE changes.
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (ok (eq (action-of (client-request client "/todos/7" :method :put
                                                           :content (form-body :token token)))
              :update))
      (ok (eq (action-of (client-request client "/todos/7" :method :patch
                                                           :content (form-body :token token)))
              :update))
      (ok (eq (action-of (client-request client "/todos/7" :method :delete
                                                           :content (form-body :token token)))
              :destroy))
      (ok (eq (action-of (client-request client "/todos" :method :post
                                                        :content (form-body :token token)))
              :create)
          "and a plain POST still reaches create")))

  (testing "a GET with a forged value still reaches the GET route"
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (declare (ignore token))
      (ok (eq (action-of (client-request client (query-uri "/todos/7" "delete"))) :show)
          "the query string does not turn a page view into a deletion"))))


(deftest method-override-generated-route-shape-test
  (testing "RESOURCES generates eight entries for the seven actions"
    ;; M9-T2 extends the public specification: the member path gets a PATCH
    ;; entry beside its PUT one, both answered by the update action, which is
    ;; what makes every method the CSRF stage verifies reachable.
    (let ((routes (resources "todos" +todo-controller+)))
      (ok (= (length routes) 8))
      (ok (equal (mapcar (lambda (r) (list (getf r :path) (getf r :action) (getf r :method)))
                         routes)
                 '(("/todos"          "index"   :get)
                   ("/todos/new"      "new"     :get)
                   ("/todos"          "create"  :post)
                   ("/todos/:id"      "show"    :get)
                   ("/todos/:id/edit" "edit"    :get)
                   ("/todos/:id"      "update"  :put)
                   ("/todos/:id"      "destroy" :delete)
                   ("/todos/:id"      "update"  :patch))))))

  (testing "the selectors name the action, so both update entries move together"
    (ok (equal (mapcar (lambda (r) (getf r :method))
                       (resources "todos" +todo-controller+ :only '(:update)))
               '(:put :patch)))
    (ok (null (find "update" (resources "todos" +todo-controller+ :except '(:update))
                    :key (lambda (r) (getf r :action))
                    :test #'string=))))

  (testing "every generated route declares a method, so none of them is ambiguous"
    (ok (every (lambda (r) (getf r :method)) (resources "todos" +todo-controller+)))))


;;; ------------------------------------------------------------------
;;; Backward compatibility
;;; ------------------------------------------------------------------

(defclass <legacy-controller> (<rest-controller>)
  ())

(defmethod do-get ((controller <legacy-controller>))
  ;; The GET also renders the session's token, which is how the suite gets
  ;; hold of one: the token is minted by asking for it, and only an action
  ;; below the CSRF stage can ask.
  (set-response controller (list (cons "receiver" "do-get")
                                 (cons "token" (csrf-token (session controller))))))

(defmethod do-post ((controller <legacy-controller>))
  (set-response controller (list (cons "receiver" "do-post"))))

(defmethod do-put ((controller <legacy-controller>))
  (set-response controller (list (cons "receiver" "do-put")
                                 (cons "method" (princ-to-string
                                                 (getf (env controller)
                                                       :request-method))))))

(defmethod do-delete ((controller <legacy-controller>))
  (set-response controller (list (cons "receiver" "do-delete"))))

(defparameter +legacy-controller+
  "clails-test/middleware/method-override-roundtrip::<legacy-controller>")

(defparameter +legacy-routes+
  ;; A route with no :action and no :method -- the shape `clails generate'
  ;; emits, and the shape that dispatches through do-get / do-post / do-put /
  ;; do-delete.
  (list (list :path "/legacy" :controller +legacy-controller+)))

(defun receiver-of (response)
  "The do-* receiver the legacy controller reported."
  (let ((body (response-body-string response)))
    (cond ((search "do-get" body) :do-get)
          ((search "do-post" body) :do-post)
          ((search "do-put" body) :do-put)
          ((search "do-delete" body) :do-delete)
          (t nil))))

(deftest method-override-legacy-dispatch-test
  ;; D5 / the backward-compatibility half of M9: the legacy branch tests "is
  ;; this a PUT", which a rewritten request now is, so it lands in the same
  ;; receiver it always did.  Every assertion below is made twice, with the
  ;; override stage on and off, and both columns must agree: whether the
  ;; declared method was rewritten one stage further out or the branch read
  ;; the field itself, the legacy receiver is the same.
  (testing "a rewritten POST reaches the same legacy receiver as before"
    (dolist (override '(t nil))
      (with-routing-table (+legacy-routes+)
        (with-no-loggers
          (with-override-stack (app :override override
                                    :inner #'not-found-app
                                    :inner-middleware (list *lack-middleware-clails-controller*))
            (let* ((client (open-session app))
                   (listing (client-request client "/legacy"))
                   (token (token-from-listing listing)))
              (ok (eq (receiver-of listing) :do-get)
                  (format nil "a GET still reaches do-get (override ~A)" override))
              (ok (stringp token) "the session carries a token")
              (ok (eq (receiver-of (client-request client "/legacy"
                                                   :method :post
                                                   :content (form-body :method "put"
                                                                       :token token)))
                      :do-put)
                  (format nil "_method=put reaches do-put (override ~A)" override))
              (ok (eq (receiver-of (client-request client "/legacy"
                                                   :method :post
                                                   :content (form-body :method "delete"
                                                                       :token token)))
                      :do-delete)
                  (format nil "_method=delete reaches do-delete (override ~A)" override))
              (ok (eq (receiver-of (client-request client "/legacy"
                                                   :method :post
                                                   :content (form-body :token token)))
                      :do-post)
                  (format nil "a plain POST reaches do-post (override ~A)" override))
              (ok (eq (receiver-of (client-request client "/legacy" :method :put
                                                                    :content (form-body :token token)))
                      :do-put)
                  (format nil "a real PUT reaches do-put (override ~A)" override))
              (ok (eq (receiver-of (client-request client "/legacy" :method :delete
                                                                    :content (form-body :token token)))
                      :do-delete)
                  (format nil "a real DELETE reaches do-delete (override ~A)" override))
              ;; M9-T2 (ii): before the allow-list accepted "patch", such a
              ;; POST fell through to do-post, because the legacy branch
              ;; tested only for "put" and "delete"; accepting the value in
              ;; the override without naming a receiver would have called
              ;; nothing at all.  A partial update is therefore dispatched to
              ;; the replacement receiver -- and the legacy branch does that
              ;; on its own too, so the answer is the same whether the
              ;; override stage rewrote the request or the branch read the
              ;; field itself.
              (ok (eq (receiver-of (client-request client "/legacy"
                                                   :method :post
                                                   :content (form-body :method "patch"
                                                                       :token token)))
                      :do-put)
                  (format nil "_method=patch reaches do-put (override ~A)" override))
              (ok (eq (receiver-of (client-request client "/legacy" :method :patch
                                                                    :content (form-body :token token)))
                      :do-put)
                  "a real PATCH reaches do-put, whatever the stage does")))))))

  (testing "a route without :method is matched on the path alone, so a rewrite cannot divert it"
    ;; The routes `clails generate' emits declare no :method, which is why
    ;; generated code is unaffected by the rewrite: whatever the method ends
    ;; up being, the same route matches.
    (with-routing-table (+legacy-routes+)
      (with-no-loggers
        (with-override-stack (app :inner #'not-found-app
                                  :inner-middleware (list *lack-middleware-clails-controller*))
          (let* ((client (open-session app))
                 (token (token-from-listing (client-request client "/legacy"))))
            (dolist (spelling '("put" "patch" "delete"))
              (ok (eql (response-status (client-request client "/legacy"
                                                        :method :post
                                                        :content (form-body :method spelling
                                                                            :token token)))
                       200)
                  (format nil "_method=~A still matches the path-only route" spelling)))))))))


(deftest method-override-hand-written-table-test
  ;; R34, judged as the intended change it is: a hand-written table that
  ;; declares BOTH a POST route and a PUT route on one path behaves
  ;; differently now.  Before M9 the forged value was invisible to route
  ;; matching, so such a POST reached the POST route; now it reaches the PUT
  ;; route, which is exactly what lets a form reach an update action.
  (let ((routes (list (list :path "/items/:id"
                            :controller +todo-controller+
                            :action "create"
                            :method :post)
                      (list :path "/items/:id"
                            :controller +todo-controller+
                            :action "update"
                            :method :put))))
    (testing "with the stage on, the forged POST reaches the PUT route"
      (with-routing-table ((append routes (resources "todos" +todo-controller+ :only '(:index))))
        (with-no-loggers
          (with-override-stack (app :inner #'not-found-app
                                    :inner-middleware (list *lack-middleware-clails-controller*))
            (let* ((client (open-session app))
                   (token (token-from-listing (client-request client "/todos"))))
              (ok (eq (action-of (client-request client "/items/5"
                                                 :method :post
                                                 :content (form-body :method "put"
                                                                     :token token)))
                      :update))
              (ok (eq (action-of (client-request client "/items/5"
                                                 :method :post
                                                 :content (form-body :token token)))
                      :create)
                  "a POST that forges nothing still reaches the POST route"))))))

    (testing "with the stage off, the same request reaches the POST route"
      (with-routing-table ((append routes (resources "todos" +todo-controller+ :only '(:index))))
        (with-no-loggers
          (with-override-stack (app :override nil
                                    :inner #'not-found-app
                                    :inner-middleware (list *lack-middleware-clails-controller*))
            (let* ((client (open-session app))
                   (token (token-from-listing (client-request client "/todos"))))
              (ok (eq (action-of (client-request client "/items/5"
                                                 :method :post
                                                 :content (form-body :method "put"
                                                                     :token token)))
                      :create)
                  "the behaviour the setting brings back"))))))))


;;; ------------------------------------------------------------------
;;; Position in the stack, seen from the outside
;;; ------------------------------------------------------------------

(deftest method-override-stage-order-roundtrip-test
  (testing "the stage runs inside the CSRF stage and outside routing"
    (let ((trace '()))
      (flet ((tracer (name)
               (lambda (inner)
                 (lambda (env)
                   (push (cons name (getf env :request-method)) trace)
                   (funcall inner env)))))
        (with-no-loggers
          (with-override-stack (app :inner (make-probe-app)
                                    :inner-middleware (list (tracer :inside-override)))
            (let* ((client (open-session app))
                   (token (probe-token (client-request client "/"))))
              ;; A rewritten request: the stage before it (the CSRF stage)
              ;; saw a POST, the stage after it sees the rewritten method.
              (setf trace '())
              (ok (eql (response-status
                        (client-request client "/" :method :post
                                                   :content (form-body :method "delete"
                                                                       :token token)))
                       200))
              (ok (equal (reverse trace) '((:inside-override . :delete))))

              ;; A refused request never reaches the stage at all, which is
              ;; the round-trip evidence that verification happens first.
              (setf trace '())
              (ok (eql (response-status
                        (client-request client "/" :method :post
                                                   :content (form-body :method "delete")))
                       +forbidden-status+))
              (ok (null trace))))))))

  (testing "the rewritten method is what routing matches, in one and the same request"
    ;; The mechanism in one line: one value in the request environment is
    ;; rewritten, and both route matching and the legacy dispatch branch read
    ;; that value.  Nothing in the routing code had to change for either.
    (with-routed-client (client token (resources "todos" +todo-controller+))
      (let ((response (client-request client "/todos/42"
                                      :method :post
                                      :content (form-body :method "delete" :token token))))
        (ok (eq (action-of response) :destroy))
        (ok (search "\"id\":\"42\"" (response-body-string response)))))))
