(in-package #:cl-user)
(defpackage #:clails-test/middleware/csrf-http-roundtrip
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf
        #:clails-test/support/routing)
  (:import-from #:clails/middleware/csrf-middleware
                #:*csrf-session-key*
                #:*csrf-form-token*
                #:+csrf-marker-key+
                #:*lack-middleware-clails-csrf-body-guard*
                #:*lack-middleware-clails-csrf*
                #:csrf-token)
  (:import-from #:clails/middleware/session-middleware
                #:*lack-middleware-clails-session*)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+)
  (:import-from #:clails/controller/base-controller
                #:<rest-controller>
                #:set-response
                #:session
                #:resources))
(in-package #:clails-test/middleware/csrf-http-roundtrip)


;;; The CSRF stage seen from the outside: requests go through a real stack and
;;; only the response is looked at.  The matrix below is method x token state x
;;; body format, which is what the milestone asks to be covered, plus the
;;; bodies that used to kill the request instead of being refused.


;;; ------------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------------

(defparameter +unsafe-methods+ '(:post :put :patch :delete))
(defparameter +safe-methods+ '(:get :head :options))

(defmacro with-csrf-client ((client token &key (one-time nil) (store nil)
                                               (header-name nil) (message nil)
                                               (after-csrf nil))
                            &body body)
  "Open a session against a stack carrying the CSRF stage, bind CLIENT to the
   client and TOKEN to the token its first (safe) request minted, then run
   BODY.

   The loggers are rebound to none, so the audit records the rejections leave
   do not print into the middle of the test report; the suite's record
   assertions capture them on purpose instead."
  `(with-no-loggers
     (with-csrf-stack (app :inner (make-token-app)
                           :store (or ,store (make-memory-session-store))
                           :one-time ,one-time
                           :header-name ,header-name
                           :message ,message
                           :after-csrf ,after-csrf)
       (let* ((,client (open-session app))
              (,token (fetch-token ,client)))
         ,@body))))

(defun post-status (client method content &key headers)
  "Send one request through CLIENT and return its status."
  (response-status (client-request client "/" :method method
                                              :content content
                                              :headers headers)))


;;; ------------------------------------------------------------------
;;; The token itself
;;; ------------------------------------------------------------------

(deftest csrf-token-minting-test
  (testing "asking for the token mints one and keeps it in the session"
    (let ((store (make-memory-session-store)))
      (with-csrf-stack (app :inner (make-token-app) :store store)
        (let* ((client (open-session app))
               (token (fetch-token client))
               (session (fetch-stored-session
                         store
                         (set-cookie-value (session-client-last-set-cookie client)))))
          (ok (stringp token))
          (ok (eql (length token) 40) "lack mints a 40 character random id")
          (ok (equal (gethash *csrf-session-key* session) token))))))

  (testing "the token belongs to the session, not to the request"
    (with-csrf-stack (app :inner (make-token-app) :store (make-memory-session-store))
      (let* ((client (open-session app))
               (first-token (fetch-token client))
               (second-token (fetch-token client)))
        (ok (equal first-token second-token)
            "a second page load returns the same token"))))

  (testing "two sessions get two different tokens"
    (with-csrf-stack (app :inner (make-token-app) :store (make-memory-session-store))
      (let ((one (fetch-token (open-session app)))
            (other (fetch-token (open-session app))))
        (ok (not (equal one other))))))

  (testing "one session's token is not accepted for another session"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :store (make-memory-session-store))
        (let* ((victim (open-session app))
               (victim-token (fetch-token victim))
               (attacker (open-session app)))
          (fetch-token attacker)
          (ok (eql (post-status attacker :post (form-content victim-token)) 403)))))))


(deftest csrf-lost-session-test
  ;; What happens to verification when the session the token lived in is gone.
  ;; M5-T3 makes that happen on purpose (an idle session expires and the id is
  ;; swapped), and the behaviour is the same whatever removed it: the contents
  ;; are gone, so the token is gone with them, and the next state-changing
  ;; request is refused until the form is fetched again.  Removing the session
  ;; from the store is the stand-in for the expiry here, so that the behaviour
  ;; is pinned down independently of M5.
  (testing "a request whose session has been dropped is refused"
    (with-no-loggers
      (let ((store (make-memory-session-store)))
        (with-csrf-stack (app :inner (make-token-app) :store store)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (eql (post-status client :post (form-content token)) 200)
                "the token works while the session is there")
            (remove-stored-session store session-id)
            (ok (eql (post-status client :post (form-content token)) 403)
                "and is refused once the session is gone")
            ;; The cookie still names a session, so a safe request opens an
            ;; empty one and mints a fresh token -- fetching the form again is
            ;; what recovers, which is what the rejection body tells the user.
            (let ((new-token (fetch-token client)))
              (ok (not (equal new-token token)))
              (ok (eql (post-status client :post (form-content new-token)) 200))))))))

  (testing "the rejection says the session held no token"
    (let ((audit (with-captured-logger (:audit)
                   (let ((store (make-memory-session-store)))
                     (with-csrf-stack (app :inner (make-token-app) :store store)
                       (let* ((client (open-session app))
                              (token (fetch-token client))
                              (session-id (set-cookie-value
                                           (session-client-last-set-cookie client))))
                         (remove-stored-session store session-id)
                         (client-request client "/" :method :post
                                         :content (form-content token))))))))
      (ok (search "no token in session" audit)))))


;;; ------------------------------------------------------------------
;;; Method x token state (the core of the matrix)
;;; ------------------------------------------------------------------

(deftest csrf-method-matrix-test
  (testing "POST / PUT / PATCH / DELETE with no token at all are refused"
    (with-csrf-client (client token)
      (declare (ignore token))
      (dolist (method +unsafe-methods+)
        (ok (eql (post-status client method (form-content nil)) 403)
            (format nil "~A without a token" method))
        (ok (eql (post-status client method nil) 403)
            (format nil "~A with no body at all" method)))))

  (testing "the same four with a token that does not match are refused"
    (with-csrf-client (client token)
      (dolist (method +unsafe-methods+)
        (ok (eql (post-status client method (form-content "0123456789")) 403)
            (format nil "~A with a short wrong token" method))
        (ok (eql (post-status client method (form-content (make-string (length token)
                                                                      :initial-element #\0)))
                 403)
            (format nil "~A with a wrong token of the right length" method))
        (ok (eql (post-status client method (form-content (concatenate 'string token "x")))
                 403)
            (format nil "~A with the token plus one character" method))
        (ok (eql (post-status client method (form-content (subseq token 0 (1- (length token)))))
                 403)
            (format nil "~A with a prefix of the token" method)))))

  (testing "the same four with the right token are served"
    (with-csrf-client (client token)
      (dolist (method +unsafe-methods+)
        (ok (eql (post-status client method (form-content token)) 200)
            (format nil "~A with the token" method)))))

  (testing "GET / HEAD / OPTIONS are not verified"
    (with-csrf-client (client token)
      (declare (ignore token))
      (dolist (method +safe-methods+)
        (ok (eql (post-status client method nil) 200)
            (format nil "~A without a token" method)))))

  (testing "a safe method does not even need a session to exist yet"
    (with-no-loggers
      (with-csrf-stack (app :inner #'ok-app :store (make-memory-session-store))
        (ok (eql (response-status (call-app app "/")) 200)))))

  (testing "the request never reaches the application when it is refused"
    (let ((reached 0))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-recording-app (lambda (env)
                                                           (declare (ignore env))
                                                           (incf reached)))
                              :store (make-memory-session-store))
          (dolist (method +unsafe-methods+)
            (call-app app "/" :method method))
          (ok (eql reached 0)))))))


;;; ------------------------------------------------------------------
;;; Body formats (M6-T1, M6-T4)
;;; ------------------------------------------------------------------

(deftest csrf-body-format-test
  (testing "urlencoded: a top-level field carries the token"
    (with-csrf-client (client token)
      (ok (eql (post-status client :post (form-content token)) 200))
      (ok (eql (post-status client :post (form-content "wrong")) 403))))

  (testing "multipart with a file attached: a top-level field carries the token"
    ;; The value of a multipart parameter arrives as a list rather than a
    ;; string, which is why this is worth asserting separately.
    (with-csrf-client (client token)
      (ok (eql (post-status client :post (multipart-content token)
                            :headers (multipart-headers))
               200))
      (ok (eql (post-status client :post (multipart-content token :file nil)
                            :headers (multipart-headers))
               200)
          "and without a file too")
      (ok (eql (post-status client :post (multipart-content "wrong")
                            :headers (multipart-headers))
               403))
      (ok (eql (post-status client :post (multipart-content nil)
                            :headers (multipart-headers))
               403))))

  (testing "JSON: a top-level member carries the token"
    (with-csrf-client (client token)
      (ok (eql (post-status client :post (json-content token) :headers (json-headers)) 200))
      (ok (eql (post-status client :post (json-content "wrong") :headers (json-headers)) 403))
      (ok (eql (post-status client :post (json-content nil) :headers (json-headers)) 403))))

  (testing "JSON: a token nested inside the body is NOT read"
    ;; Deliberate: the stage's decision must not depend on an application's
    ;; JSON schema.  Such a request has to carry the token at the top level or
    ;; in the header.
    (with-csrf-client (client token)
      (ok (eql (post-status client :post
                            (format nil "{\"payload\":{\"~A\":\"~A\"}}" *csrf-form-token* token)
                            :headers (json-headers))
               403))))

  (testing "JSON: a token sent as a number is NOT read"
    (with-csrf-client (client token)
      (declare (ignore token))
      (ok (eql (post-status client :post
                            (format nil "{\"~A\":12345}" *csrf-form-token*)
                            :headers (json-headers))
               403))))

  (testing "a token in the query string is NOT read"
    ;; A token in a URL ends up in access logs, proxy histories and Referer
    ;; headers, so the query string is out of scope by decision.
    (with-csrf-client (client token)
      (with-no-loggers
        (ok (eql (response-status
                  (client-request client (format nil "/?~A=~A" *csrf-form-token* token)
                                  :method :post))
                 403)))))

  (testing "the header carries the token (M6-T4)"
    (with-csrf-client (client token)
      (ok (eql (post-status client :post nil :headers (token-header token)) 200)
          "no body at all, token in the header")
      (ok (eql (post-status client :post (form-content nil) :headers (token-header token)) 200)
          "urlencoded body without the field, token in the header")
      (ok (eql (post-status client :post (json-content nil)
                            :headers (append (json-headers) (token-header token)))
               200)
          "JSON body, token in the header")
      (ok (eql (post-status client :post (json-content nil)
                            :headers (append (json-headers) (token-header "wrong")))
               403)
          "JSON body, wrong token in the header")
      (ok (eql (post-status client :post (json-content nil) :headers (json-headers)) 403)
          "JSON body, no token anywhere")
      (dolist (method +unsafe-methods+)
        (ok (eql (post-status client method nil :headers (token-header token)) 200)
            (format nil "~A with the token in the header" method)))))

  (testing "the header name is matched case-insensitively"
    (with-csrf-client (client token)
      (ok (eql (post-status client :post nil :headers (token-header token :name "x-csrf-token")) 200))
      (ok (eql (post-status client :post nil :headers (token-header token :name "X-CSRF-TOKEN")) 200))))

  (testing "the header name is configurable"
    (with-csrf-client (client token :header-name "X-App-Csrf")
      (ok (eql (post-status client :post nil :headers (token-header token :name "X-App-Csrf")) 200))
      (ok (eql (post-status client :post nil :headers (token-header token :name "X-CSRF-Token")) 403)
          "the default name is no longer read")))

  (testing "the body field wins when both are present"
    ;; Decided precedence: the visible, explicit place for the token is the
    ;; form field, so a header cannot override it.
    (with-csrf-client (client token)
      (ok (eql (post-status client :post (form-content token)
                            :headers (token-header "wrong"))
               200)
          "a good field is not spoiled by a bad header")
      (ok (eql (post-status client :post (form-content "wrong")
                            :headers (token-header token))
               403)
          "and a bad field is not rescued by a good header"))))


;;; ------------------------------------------------------------------
;;; Unexpected bodies (M6-T3, G10)
;;; ------------------------------------------------------------------

(defparameter +unexpected-bodies+
  (list (cons "broken JSON" "{\"a\":")
        (cons "a JSON array at the top level" "[1,2,3]")
        (cons "a JSON array of objects" "[{\"a\":1}]")
        (cons "an empty body declared as JSON" "")
        (cons "a urlencoded body declared as JSON" "a=b&c=d")
        (cons "a JSON scalar" "42")
        (cons "a body cut off mid-string" "{\"a\": \"unterminated"))
  "Bodies that are not what their Content-Type says they are.

   Every one of these used to let a condition escape the whole middleware
   stack -- not from the CSRF stage but from the session stage, whose cookie
   handling parses the body on the way to the Cookie header.  Each must come
   back as an ordinary rejection instead.")

(deftest csrf-unexpected-body-test
  (testing "every unexpected body is refused rather than killing the request"
    (with-csrf-client (client token)
      (declare (ignore token))
      (dolist (body +unexpected-bodies+)
        (dolist (method +unsafe-methods+)
          ;; The assertion is simply that a response came back: a condition
          ;; escaping the stack would abort the test instead.
          (let ((status (post-status client method (cdr body) :headers (json-headers))))
            (ok (eql status 403)
                (format nil "~A sent with ~A" (car body) method)))))))

  (testing "a multipart body with no boundary is refused as well"
    (with-csrf-client (client token)
      (declare (ignore token))
      (ok (eql (post-status client :post "--nope--"
                            :headers (list (cons "content-type" "multipart/form-data")))
               403))))

  (testing "the process goes on serving normal requests afterwards"
    (with-csrf-client (client token)
      (dolist (body +unexpected-bodies+)
        (post-status client :post (cdr body) :headers (json-headers)))
      (ok (eql (post-status client :post (form-content token)) 200)
          "a legitimate POST still works")
      (ok (eql (post-status client :get nil) 200)
          "and so does a safe request")
      (ok (eql (post-status client :post (json-content token) :headers (json-headers)) 200)
          "and so does a legitimate JSON POST")))

  (testing "a body whose Content-Type the parser does not know is not refused for that"
    ;; text/plain is not parsed into parameters at all, which is not an error:
    ;; such a request simply has to carry its token in the header.
    (with-csrf-client (client token)
      (ok (eql (post-status client :post "some plain text"
                            :headers (append (list (cons "content-type" "text/plain"))
                                             (token-header token)))
               200))
      (ok (eql (post-status client :post "some plain text"
                            :headers (list (cons "content-type" "text/plain")))
               403)
          "but it is still refused when no token is anywhere")))

  (testing "a safe method carrying an unparseable body is refused too"
    ;; Decided behaviour: the parse that signals is the session stage's, and
    ;; it does not care about the method, so a GET with a broken body would
    ;; kill the request just as reliably.  It is refused, uniformly.
    (with-csrf-client (client token)
      (declare (ignore token))
      (dolist (method +safe-methods+)
        (ok (eql (post-status client method "{\"a\":" :headers (json-headers)) 403)
            (format nil "~A with a broken body" method)))))

  (testing "without the body guard the condition really does escape the stack"
    ;; The counter-evidence for where the guard has to sit: with the guard
    ;; left out, the body is parsed by the session stage, one stage further
    ;; out than the CSRF stage, so no wrapper of the CSRF stage could ever
    ;; catch it.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app)
                            :body-guard nil
                            :store (make-memory-session-store))
        (ok (signals (call-app app "/" :method :post
                                       :content "{\"a\":"
                                       :headers (json-headers))
                     'error))))))


;;; ------------------------------------------------------------------
;;; The rejection response, and the audit record (M6-T2)
;;; ------------------------------------------------------------------

(deftest csrf-rejection-response-test
  (testing "the rejection is the 403 body format M2 decided"
    (with-csrf-client (client token)
      (declare (ignore token))
      (with-no-loggers
        (let ((response (client-request client "/" :method :post :content (form-content nil))))
          (ok (eql (response-status response) 403))
          (ok (eql (response-status response) +forbidden-status+)
              "the same code the rejection module carries")
          (ok (equal (response-header response :content-type) "text/plain; charset=utf-8"))
          (ok (eql (response-header response :content-length)
                   (length (response-body-string response)))
              "Content-Length matches the body (which is pure ASCII here)")
          (let ((body (response-body-string response)))
            (ok (null (find #\Newline body)) "one line")
            (ok (search "Forbidden" body))
            (ok (search "Reload" body) "and it tells the user what to do"))))))

  (testing "the rejection body is configurable"
    (with-csrf-client (client token :message "Nope. Reload.")
      (declare (ignore token))
      (with-no-loggers
        (let ((response (client-request client "/" :method :post)))
          (ok (equal (response-body-string response) "Nope. Reload."))
          (ok (eql (response-header response :content-length) 13))))))

  (testing "a missing token and a wrong token are both recorded on the audit logger"
    (let* ((client nil)
           (token nil)
           (store (make-memory-session-store))
           (audit (with-captured-logger (:audit)
                    (with-csrf-stack (app :inner (make-token-app) :store store)
                      (setf client (open-session app))
                      (setf token (fetch-token client))
                      (client-request client "/todos" :method :post :content (form-content nil))
                      (client-request client "/todos/7" :method :delete
                                      :content (form-content "wrong"))))))
      (ok (search "token missing from request" audit))
      (ok (search "token mismatch" audit))
      (ok (search "status=403" audit))
      (ok (search "request-method=:POST" audit))
      (ok (search "request-method=:DELETE" audit))
      (ok (search "path=\"/todos\"" audit))
      (ok (search "path=\"/todos/7\"" audit))
      (ok (search "stage=:CSRF" audit) "the record says which stage refused")
      (ok (search "ip-address=" audit))))

  (testing "a refused unexpected body is recorded too"
    (let ((audit (with-captured-logger (:audit)
                   (with-csrf-stack (app :inner (make-token-app)
                                         :store (make-memory-session-store))
                     (call-app app "/todos" :method :post
                               :content "{\"a\":"
                               :headers (json-headers))))))
      (ok (search "request body could not be parsed" audit))
      (ok (search "status=403" audit))
      (ok (search "path=\"/todos\"" audit))))

  (testing "nothing is recorded when the request is served"
    (let* ((client nil)
           (token nil)
           (audit (with-captured-logger (:audit)
                    (with-csrf-stack (app :inner (make-token-app)
                                          :store (make-memory-session-store))
                      (setf client (open-session app))
                      (setf token (fetch-token client))
                      (client-request client "/" :method :post :content (form-content token))))))
      (ok (equal audit ""))))

  (testing "the rejection still travels through the stages above it"
    ;; The session stage is outside the CSRF stage, so it finalises the 403
    ;; exactly as it would a served response -- the client keeps its session.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :store (make-memory-session-store))
        (let ((response (call-app app "/" :method :post)))
          (ok (eql (response-status response) 403))
          (ok (response-set-cookie response) "Set-Cookie is still emitted"))))))


;;; ------------------------------------------------------------------
;;; Where the stage sits (M6-T1; the ground M9 stands on)
;;; ------------------------------------------------------------------

(defclass <todo-controller> (<rest-controller>)
  ())

(defgeneric create (controller))
(defmethod create ((controller <todo-controller>))
  (set-response controller (list (cons "action" "create"))))

(defgeneric index (controller))
(defmethod index ((controller <todo-controller>))
  ;; This is "the page with the form on it": an action asks for the token,
  ;; which is only possible because the CSRF stage is outside the routing
  ;; stage -- the dynamic state CSRF-TOKEN reads is bound by the stage, so an
  ;; action can only call it from inside.
  (set-response controller (list (cons "action" "index")
                                 (cons "token" (csrf-token (session controller))))))

(deftest csrf-stage-position-test
  (testing "the stage runs inside the session stage and outside the routing stage"
    (let ((trace '()))
      (flet ((tracer (name)
               (lambda (inner)
                 (lambda (env)
                   (push (cons name (and (getf env :lack.session) t)) trace)
                   (funcall inner env)))))
        (with-no-loggers
          (with-csrf-stack (app :inner (make-token-app)
                                :store (make-memory-session-store)
                                :before-csrf (list (tracer :inside-session))
                                :after-csrf (list (tracer :inside-csrf)))
            (let* ((client (open-session app))
                   (token (fetch-token client)))
              (setf trace '())
              (ok (eql (post-status client :post (form-content token)) 200))
              (ok (equal (reverse trace)
                         '((:inside-session . t) (:inside-csrf . t)))
                  "the session is already there when the CSRF stage runs")

              ;; A refused request stops at the CSRF stage: the stage that
              ;; comes after it never runs, which is what makes "the rewriting
              ;; stage M9 adds is inside the verification" verifiable from
              ;; outside.
              (setf trace '())
              (ok (eql (post-status client :post (form-content "wrong")) 403))
              (ok (equal (reverse trace) '((:inside-session . t))))))))))

  (testing "a refused request never reaches routing"
    (with-routing-table ((resources "todos"
                                    "clails-test/middleware/csrf-http-roundtrip::<todo-controller>"))
      (with-no-loggers
        (with-csrf-stack (app :inner #'not-found-app
                              :store (make-memory-session-store)
                              :after-csrf (list *lack-middleware-clails-controller*))
          (let* ((client (open-session app))
                 ;; the token has to be minted by the stage, so ask a page
                 ;; that renders it -- here the resource's index action is
                 ;; enough to open the session, and the token is read from the
                 ;; store directly below
                 (listing (client-request client "/todos")))
            (ok (eql (response-status listing) 200))
            (ok (search "index" (response-body-string listing)))
            ;; create is a POST: refused without the token, served with it
            (ok (eql (post-status client :post (form-content nil)) 403)
                "POST /todos without a token never reaches the action"))))))

  (testing "the action runs when the token is right, through the routing stage"
    ;; The whole round trip a form takes: GET the listing (whose action asks
    ;; the stage for a token), then POST with that token and reach the create
    ;; action.  That the index action can call CSRF-TOKEN at all is the
    ;; evidence that the stage is outside routing.
    (let ((store (make-memory-session-store)))
      (with-routing-table ((resources "todos"
                                      "clails-test/middleware/csrf-http-roundtrip::<todo-controller>"))
        (with-no-loggers
          (with-csrf-stack (app :inner #'not-found-app
                                :store store
                                :after-csrf (list *lack-middleware-clails-controller*))
            (let* ((client (open-session app))
                   (listing (client-request client "/todos"))
                   (session-id (set-cookie-value (session-client-last-set-cookie client)))
                   (token (gethash *csrf-session-key*
                                   (fetch-stored-session store session-id))))
              (ok (eql (response-status listing) 200))
              (ok (search "index" (response-body-string listing)))
              (ok (stringp token) "the action minted a token")
              (ok (search token (response-body-string listing))
                  "and it is the token the page rendered")
              (ok (eql (post-status client :post (form-content nil)) 403)
                  "POST /todos without the token never reaches the action")
              (let ((created (client-request client "/todos" :method :post
                                                             :content (form-content token))))
                (ok (eql (response-status created) 200))
                (ok (search "create" (response-body-string created))
                    "with the token, the create action ran"))))))))

  (testing "the marker is in place for the stages M9 will add inside"
    (let ((marker :unset))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-token-app)
                              :store (make-memory-session-store)
                              :after-csrf (list (lambda (inner)
                                                  (lambda (env)
                                                    (setf marker (getf env +csrf-marker-key+))
                                                    (funcall inner env)))))
          (let* ((client (open-session app))
                 (token (fetch-token client)))
            (setf marker :unset)
            (post-status client :post (form-content token))
            (ok (eq marker t))))))))
