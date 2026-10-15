(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-http-roundtrip
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:lack/request
                #:make-request
                #:request-method
                #:request-body-parameters
                #:request-query-parameters))
(in-package #:clails-test/middleware/session-http-roundtrip)


;;; HTTP round trips over the clails middleware stack, without starting a
;;; server and without touching a database.  The stack is rebuilt per test by
;;; clails-test/support/http:with-middleware-stack.


(deftest http-roundtrip-smoke-test
  (testing "a GET returns a status code, headers and a body that can be read back"
    (with-middleware-stack (app :session nil)
      (let ((response (call-app app "/")))
        (ok (= (response-status response) 200))
        (ok (string= (response-header response :content-type) "text/plain"))
        (ok (string= (response-body-string response) "ok")))))

  (testing "lack-test's TESTING-APP / REQUEST are usable too"
    (with-middleware-stack (app :session nil)
      (with-http-client (req app)
        (multiple-value-bind (body status headers) (req "/healthz")
          (ok (= status 200))
          (ok (string= body "ok"))
          (ok (string= (gethash "content-type" headers) "text/plain"))))))

  (testing "the path really reaches the application"
    (let ((seen-path nil))
      (with-middleware-stack (app :session nil
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen-path (getf env :path-info)))))
        (call-app app "/deep/path")
        (ok (string= seen-path "/deep/path")))))

  (testing "no Set-Cookie is emitted while the session stage is disabled"
    (with-middleware-stack (app :session nil)
      (let ((response (call-app app "/")))
        (ok (null (response-header response :set-cookie)))))))


(deftest session-cookie-carry-over-test
  (testing "a value written on the first request is visible on the next ones"
    (with-middleware-stack (app :inner (make-session-counter-app))
      (let ((client (make-session-client app)))
        (ok (string= (response-body-string (client-request client "/")) "1"))
        (ok (string= (response-body-string (client-request client "/")) "2"))
        (ok (string= (response-body-string (client-request client "/")) "3")))))

  (testing "a client that sends no cookie gets a fresh session every time"
    (with-middleware-stack (app :inner (make-session-counter-app))
      (ok (string= (response-body-string (call-app app "/")) "1"))
      (ok (string= (response-body-string (call-app app "/")) "1"))))

  (testing "the cookie carries a session id only, never the session contents"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack (app :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf (gethash "secret" (getf env :lack.session))
                                                  "top-secret-value"))))
        (let* ((response (call-app app "/"))
               (raw (response-header response :set-cookie))
               (cookie (response-set-cookie response))
               (session-id (set-cookie-value cookie)))
          (ok (string= (set-cookie-name cookie) "lack.session"))
          ;; lack session ids are 40 hex characters
          ;; (lack/middleware/session/state L18-L23)
          (ok (= (length session-id) 40))
          (ok (every (lambda (char) (find char "0123456789abcdef")) session-id))
          ;; nothing of the session payload leaks into the header
          (ok (null (search "top-secret-value" raw)))
          (ok (null (search "secret" raw)))
          ;; the payload lives in the store, keyed by that id
          (ok (string= (gethash "secret" (fetch-stored-session store session-id))
                       "top-secret-value")))))))


(deftest session-set-cookie-attribute-test
  ;; The response examined here is deliberately the FIRST, session-creating
  ;; response.  lack emits Set-Cookie only for a new session, a changed id or
  ;; an expiry (lack/middleware/session/state/cookie finalize-state
  ;; L45-L51), so a session-creating response is the only kind of response on
  ;; which cookie attributes can be asserted at all.
  (testing "current defaults on the session-creating response"
    (with-middleware-stack (app)
      (let* ((response (call-app app "/"))
             (cookie (response-set-cookie response)))
        (ok cookie "the session-creating response carries a Set-Cookie header")
        (ok (string= (set-cookie-name cookie) "lack.session"))
        ;; attributes that ARE emitted today
        (ok (set-cookie-attribute-present-p cookie "path"))
        (ok (string= (set-cookie-attribute cookie "path") "/"))
        (ok (set-cookie-attribute-present-p cookie "expires"))
        (ok (set-cookie-attribute-present-p cookie "samesite"))
        (ok (string= (set-cookie-attribute cookie "samesite") "Lax"))
        ;; attributes that are NOT emitted today; these are the current
        ;; defaults, recorded as they are -- M5-T4 is where they change
        (ok (not (set-cookie-attribute-present-p cookie "httponly")))
        (ok (not (set-cookie-attribute-present-p cookie "secure")))
        (ok (not (set-cookie-attribute-present-p cookie "domain")))
        (ok (not (set-cookie-attribute-present-p cookie "max-age"))))))

  (testing "attributes configured on the cookie state are emitted and detected"
    (with-middleware-stack (app :state (make-test-cookie-state
                                        :cookie-key "_clails_test_session"
                                        :path "/app"
                                        :httponly t
                                        :secure t
                                        :samesite :strict))
      (let* ((response (call-app app "/"))
             (cookie (response-set-cookie response)))
        (ok (string= (set-cookie-name cookie) "_clails_test_session"))
        (ok (string= (set-cookie-attribute cookie "path") "/app"))
        ;; valueless attributes are reported as present with the value T,
        ;; which is how "present without a value" is told apart from "absent"
        (ok (set-cookie-attribute-present-p cookie "httponly"))
        (ok (eq (set-cookie-attribute cookie "httponly") t))
        (ok (set-cookie-attribute-present-p cookie "secure"))
        (ok (eq (set-cookie-attribute cookie "secure") t))
        (ok (string= (set-cookie-attribute cookie "samesite") "Strict"))
        ;; still absent, even with everything else turned on
        (ok (not (set-cookie-attribute-present-p cookie "domain")))))))


(deftest session-cookie-not-reissued-test
  ;; Fixes the measured behaviour as a specification: the cookie is attached
  ;; only when the session is created, when its id changes, or when it is
  ;; destroyed.  A continuing ordinary request gets no Set-Cookie, which is
  ;; why an "expires" attribute would never be refreshed during a visit --
  ;; the reasoning M5-T4 relies on when it decides not to carry one.
  (testing "a continuing request carries no Set-Cookie, yet keeps the session"
    (with-middleware-stack (app :inner (make-session-counter-app))
      (let ((client (make-session-client app)))
        (let ((first (client-request client "/")))
          (ok (response-header first :set-cookie))
          (ok (session-client-last-set-cookie client)))
        (let ((second (client-request client "/")))
          (ok (null (response-header second :set-cookie)))
          (ok (null (session-client-last-set-cookie client)))
          (ok (string= (response-body-string second) "2")))
        (let ((third (client-request client "/")))
          (ok (null (response-header third :set-cookie)))
          (ok (string= (response-body-string third) "3")))))))


(deftest request-shape-test
  ;; Body, query string and request method can all be chosen freely.  This is
  ;; the ground M9 stands on: it has to send a POST carrying a forged method
  ;; in the body, and a GET carrying a forged method in the query string.
  (testing "a POST body reaches the application, query parameters stay empty"
    (let ((seen nil))
      (with-middleware-stack (app :session nil
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (let ((req (make-request env)))
                                              (setf seen
                                                    (list :method (request-method req)
                                                          :body (request-body-parameters req)
                                                          :query (request-query-parameters req)))))))
        (call-app app "/todos/7"
                  :method :post
                  :content '(("_method" . "delete") ("title" . "spoofed")))
        (ok (eq (getf seen :method) :post))
        (ok (string= (cdr (assoc "_method" (getf seen :body) :test #'string=)) "delete"))
        (ok (string= (cdr (assoc "title" (getf seen :body) :test #'string=)) "spoofed"))
        (ok (null (getf seen :query))))))

  (testing "a query string reaches the application, the body stays empty"
    (let ((seen nil))
      (with-middleware-stack (app :session nil
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (let ((req (make-request env)))
                                              (setf seen
                                                    (list :method (request-method req)
                                                          :body (request-body-parameters req)
                                                          :query (request-query-parameters req)))))))
        (call-app app "/todos/7?_method=delete&title=spoofed" :method :get)
        (ok (eq (getf seen :method) :get))
        (ok (string= (cdr (assoc "_method" (getf seen :query) :test #'string=)) "delete"))
        (ok (string= (cdr (assoc "title" (getf seen :query) :test #'string=)) "spoofed"))
        (ok (null (getf seen :body))))))

  (testing "any request method can be sent"
    (let ((seen '()))
      (with-middleware-stack (app :session nil
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (push (getf env :request-method) seen))))
        (dolist (method '(:get :post :put :delete :patch :head))
          (call-app app "/" :method method))
        (ok (equal (reverse seen) '(:get :post :put :delete :patch :head)))))))
