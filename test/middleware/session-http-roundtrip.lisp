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
          (ok (string= (set-cookie-name cookie) +default-session-cookie-name+))
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
  ;; UPDATED BY M5-T4.  This block used to record the defaults clails
  ;; inherited from the underlying library -- cookie named "lack.session",
  ;; no HttpOnly, and an "expires" roughly 127 years out (the library adds
  ;; the current time to a slot it initialised to the current time).  M5-T4
  ;; replaced those defaults with safe ones and gave clails a cookie state
  ;; of its own, so what the baseline records changed with them.  The
  ;; detailed assertions about the new defaults -- including the absence of
  ;; "expires", which is the one that needed the new state -- live in
  ;; clails-test/middleware/session-cookie; what is kept here is the shape
  ;; of the session-creating response.
  (testing "current defaults on the session-creating response"
    (with-middleware-stack (app)
      (let* ((response (call-app app "/"))
             (cookie (response-set-cookie response)))
        (ok cookie "the session-creating response carries a Set-Cookie header")
        (ok (string= (set-cookie-name cookie) +default-session-cookie-name+))
        (ok (string/= (set-cookie-name cookie) "lack.session")
            "the default cookie name no longer names the library underneath")
        ;; attributes that ARE emitted
        (ok (set-cookie-attribute-present-p cookie "path"))
        (ok (string= (set-cookie-attribute cookie "path") "/"))
        (ok (set-cookie-attribute-present-p cookie "httponly"))
        (ok (set-cookie-attribute-present-p cookie "samesite"))
        (ok (string= (set-cookie-attribute cookie "samesite") "Lax"))
        ;; attributes that are NOT emitted.  "expires" is the interesting
        ;; one: the deadline is the server-side idle timeout, and a date on
        ;; the cookie would turn it into an absolute lifetime (the cookie is
        ;; not re-sent on a continuing request -- see
        ;; SESSION-COOKIE-NOT-REISSUED-TEST below).
        (ok (not (set-cookie-attribute-present-p cookie "expires")))
        (ok (not (set-cookie-attribute-present-p cookie "max-age")))
        (ok (not (set-cookie-attribute-present-p cookie "secure"))
            "Secure is off by default, so sessions work over plain HTTP")
        (ok (not (set-cookie-attribute-present-p cookie "domain"))))))

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


;;; ------------------------------------------------------------------
;;; Regression tests for the harness itself
;;; ------------------------------------------------------------------

(defun make-cookie-setting-app (inner raw-set-cookie)
  "Wrap INNER so that its response also carries RAW-SET-COOKIE.

   The extra header is appended by the application, i.e. INSIDE the session
   stage, so lack/response:finalize-cookies appends the session cookie after
   it -- which puts the application's cookie first, exactly where a reader
   that stops at the first :set-cookie entry would find it."
  (lambda (env)
    (let ((response (funcall inner env)))
      (list (first response)
            (append (second response) (list :set-cookie raw-set-cookie))
            (third response)))))

(deftest session-cookie-tracking-test
  ;; Regression test for a defect of this harness, not of the framework:
  ;; a Lack response plist may carry :set-cookie more than once, because
  ;; lack/response:finalize-cookies APPENDS one entry per baked cookie.  The
  ;; helpers used to read it with GETF, so an application that set a cookie
  ;; of its own made SESSION-CLIENT store that cookie as "the" session
  ;; cookie: the counter below answered "1" on every request while every
  ;; assertion stayed green.
  (testing "an application cookie does not hide the session cookie"
    (with-middleware-stack (app :inner (make-cookie-setting-app
                                        (make-session-counter-app)
                                        "flash=hello; path=/"))
      (let* ((client (make-session-client app))
             (first-response (client-request client "/")))
        ;; both cookies really are on the response, application's one first
        (ok (equal (mapcar #'set-cookie-name (response-set-cookies first-response))
                   (list "flash" +default-session-cookie-name+))
            "the response sets the application cookie first, then the session cookie")
        (ok (string= (response-body-string first-response) "1"))
        ;; the client learned the session cookie, not the first one it saw
        (ok (string= (set-cookie-name (session-client-last-set-cookie client))
                     +default-session-cookie-name+))
        (ok (= (length (set-cookie-value (session-client-last-set-cookie client))) 40))
        ;; ... so the session continues instead of starting over
        (ok (string= (response-body-string (client-request client "/")) "2"))
        (ok (string= (response-body-string (client-request client "/")) "3"))
        ;; and the application cookie is carried back as well, as a browser would
        (let ((header (session-client-cookie client)))
          (ok (search "flash=hello" header) "the application cookie is sent back too")
          (ok (search (concatenate 'string +default-session-cookie-name+ "=") header)
              "and so is the session cookie")))))

  (testing "a cookie can be picked out of a multi-cookie response by name"
    (with-middleware-stack (app :inner (make-cookie-setting-app
                                        (make-session-counter-app)
                                        "flash=hello; path=/"))
      (let ((response (call-app app "/")))
        (ok (= (length (response-set-cookies response)) 2))
        (ok (string= (set-cookie-value (response-set-cookie response "flash")) "hello"))
        (ok (= (length (response-session-id response +default-session-cookie-name+)) 40))
        ;; asking without a name would be a guess, so it is refused rather
        ;; than answered about the wrong cookie
        (ok (handler-case (progn (response-set-cookie response) nil)
              (error () t))
            "an ambiguous RESPONSE-SET-COOKIE signals instead of guessing")))))


(deftest session-cookie-expiry-test
  ;; Regression test for a defect of this harness: SESSION-CLIENT used to
  ;; keep and resend a cookie whose Set-Cookie had already expired it.  A
  ;; browser drops such a cookie, so a test could observe a session id being
  ;; reused -- and a session continuing -- after the server told the client
  ;; to forget it.  That is exactly the illusion M2's fixation tests and M5's
  ;; expiry tests must not be built on.
  (testing "a cookie the response expires is not sent again"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack
          (app :store store
               :inner (let ((counter (make-session-counter-app)))
                        (lambda (env)
                          (let ((response (funcall counter env)))
                            ;; /logout asks lack to expire the session: the
                            ;; store entry is dropped and the Set-Cookie
                            ;; carries expires=<now>
                            (when (string= (getf env :path-info) "/logout")
                              (setf (getf (getf env :lack.session.options) :expire) t))
                            response))))
        (let* ((client (make-session-client app))
               (first-id (progn (client-request client "/")
                                (set-cookie-value
                                 (session-client-last-set-cookie client)))))
          (ok (= (length first-id) 40))
          (ok (string= (response-body-string (client-request client "/")) "2"))
          (let* ((response (client-request client "/logout"))
                 (cookie (response-set-cookie response +default-session-cookie-name+)))
            (ok cookie "expiring the session re-emits the cookie")
            (ok (string= (set-cookie-value cookie) first-id)
                "the expiring Set-Cookie still names the old session id")
            (ok (set-cookie-expired-p cookie)
                "and it is expired: expires is not in the future")
            (ok (null (session-client-jar client))
                "so the client drops it instead of remembering it")
            (ok (null (session-client-cookie client))
                "and sends no Cookie header at all on the next request"))
          ;; a browser would start a brand new session here, and so does the
          ;; client: no resurrection of the id the server just retired
          (let* ((response (client-request client "/"))
                 (second-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (string= (response-body-string response) "1")
                "the counter starts over, so the old session was not continued")
            (ok (= (length second-id) 40))
            (ok (string/= second-id first-id)
                "and the new session got a different id, not the retired one")
            (ok (null (fetch-stored-session store first-id))
                "the retired session is gone from the store"))))))

  (testing "Max-Age and Expires are both understood"
    (ok (set-cookie-expired-p (parse-set-cookie "a=b; Max-Age=0")))
    (ok (set-cookie-expired-p (parse-set-cookie "a=b; Max-Age=-1")))
    (ok (not (set-cookie-expired-p (parse-set-cookie "a=b; Max-Age=60"))))
    (ok (set-cookie-expired-p
         (parse-set-cookie "a=b; expires=Thu, 01 Jan 1970 00:00:00 GMT")))
    (ok (not (set-cookie-expired-p
              (parse-set-cookie "a=b; expires=Mon, 30 Jul 2153 21:39:38 GMT"))))
    ;; Max-Age wins over Expires (RFC 6265 5.3)
    (ok (set-cookie-expired-p
         (parse-set-cookie "a=b; Max-Age=0; expires=Mon, 30 Jul 2153 21:39:38 GMT")))
    ;; a plain session cookie never expires on its own
    (ok (not (set-cookie-expired-p (parse-set-cookie "a=b; path=/"))))
    ;; an Expires nobody can read means "unknown", never "expired"
    (ok (not (set-cookie-expired-p (parse-set-cookie "a=b; expires=whenever"))))
    ;; all three HTTP-date formats of RFC 7231
    (ok (= (parse-http-date "Mon, 30 Jul 2153 21:39:38 GMT")
           (parse-http-date "Mon Jul 30 21:39:38 2153")
           (encode-universal-time 38 39 21 30 7 2153 0)))
    (ok (= (parse-http-date "Monday, 30-Jul-53 21:39:38 GMT")
           (encode-universal-time 38 39 21 30 7 2053 0)))
    (ok (null (parse-http-date "not a date")))))


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
