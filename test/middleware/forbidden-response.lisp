(in-package #:cl-user)
(defpackage #:clails-test/middleware/forbidden-response
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+
                #:*forbidden-message*
                #:*forbidden-content-type*
                #:forbidden-response
                #:log-rejection
                #:reject-forbidden
                #:forbidden-app)
  (:import-from #:clails/condition
                #:403/forbidden
                #:code)
  (:import-from #:clails/logger
                #:*logger-registry*
                #:register-logger
                #:get-logger
                #:make-console-appender
                #:<text-formatter>))
(in-package #:clails-test/middleware/forbidden-response)


;;; ------------------------------------------------------------------
;;; The minimal rejecting stage, and logger plumbing
;;; ------------------------------------------------------------------

(defun make-rejecting-middleware (&key (reason "test rejection") context)
  "The minimal stage M2 establishes: it answers every request with the 403
   triple built by REJECT-FORBIDDEN and never calls the application it
   wraps.

   A real stage (M6's CSRF stage, for one) decides per request whether to
   call APP or to reject; this one always rejects, which is what makes the
   \"the application was never reached\" assertions below meaningful."
  (lambda (app)
    (declare (ignore app))
    (lambda (env)
      (reject-forbidden env :reason reason :context context))))

(defmacro with-no-loggers (&body body)
  "Run BODY with an empty logger registry.

   The registry is rebound, not cleared, so the logger configuration of the
   rest of the suite survives -- and no audit record written by BODY can
   print into the middle of the test report."
  `(let ((*logger-registry* (make-hash-table :test 'equal)))
     ,@body))

(defun call-with-captured-logger (logger-name thunk)
  "Call THUNK with LOGGER-NAME as the only registered logger, writing to a
   console appender, and return everything that logger printed.

   Assertions must be made on the returned string AFTER this call, never
   inside THUNK: rove's own report stream is a synonym stream for
   *standard-output*, so an assertion made inside would be captured here
   instead of being reported."
  (let ((*logger-registry* (make-hash-table :test 'equal)))
    (with-output-to-string (*standard-output*)
      (register-logger logger-name
                       :appender (make-console-appender
                                  :formatter (make-instance '<text-formatter>))
                       :level :info)
      (funcall thunk))))

(defmacro with-captured-logger ((logger-name) &body body)
  "Run BODY with LOGGER-NAME as the only registered logger and return what
   that logger printed.  See CALL-WITH-CAPTURED-LOGGER."
  `(call-with-captured-logger ,logger-name (lambda () ,@body)))


;;; ------------------------------------------------------------------
;;; The response triple itself
;;; ------------------------------------------------------------------

(deftest forbidden-response-test
  (testing "the triple is 403 / plain text / a one-line body"
    (let ((response (forbidden-response)))
      (ok (eql (first response) 403))
      (ok (eql (first response) +forbidden-status+))
      (ok (equal (getf (second response) :content-type) "text/plain; charset=utf-8"))
      (ok (eql (getf (second response) :content-length) 9))
      (ok (equal (third response) (list "Forbidden")))))

  (testing "the status code is the one 403/forbidden carries"
    ;; The condition class stays the single description of "403" even though
    ;; the rejection path does not build a condition: if the two ever drift
    ;; apart, this fails.
    (ok (eql +forbidden-status+ (code (make-condition '403/forbidden :path "/")))))

  (testing "a stage can pass its own body and extra headers"
    (let ((response (forbidden-response :message "Forbidden: no token"
                                        :content-type "text/html"
                                        :headers (list :x-clails-rejected "1"))))
      (ok (eql (first response) 403))
      (ok (equal (getf (second response) :content-type) "text/html"))
      (ok (eql (getf (second response) :content-length) 19))
      (ok (equal (getf (second response) :x-clails-rejected) "1"))
      (ok (equal (third response) (list "Forbidden: no token")))))

  (testing "Content-Length counts UTF-8 bytes, not characters"
    ;; Built with CODE-CHAR so that this file stays pure ASCII: two
    ;; characters, four UTF-8 bytes.
    (let* ((message (coerce (list (code-char 233) (code-char 233)) 'string))
           (response (forbidden-response :message message)))
      (ok (eql (length message) 2))
      (ok (eql (getf (second response) :content-length) 4))))

  (testing "the default body is configurable"
    (let ((*forbidden-message* "Nope"))
      (let ((response (forbidden-response)))
        (ok (equal (third response) (list "Nope")))
        (ok (eql (getf (second response) :content-length) 4)))))

  (testing "the default content type is configurable"
    (let ((*forbidden-content-type* "application/json"))
      (ok (equal (getf (second (forbidden-response)) :content-type)
                 "application/json")))))


;;; ------------------------------------------------------------------
;;; The round trip through a middleware stack (the M1 harness)
;;; ------------------------------------------------------------------

(deftest forbidden-round-trip-test
  (testing "a rejecting stage answers 403 and the application is never reached"
    (let ((reached nil))
      (with-no-loggers
        (with-middleware-stack (app
                                :inner (make-recording-app
                                        (lambda (env)
                                          (declare (ignore env))
                                          (setf reached t)))
                                :inner-middleware (list (make-rejecting-middleware)))
          (let ((response (call-app app "/todos" :method :post)))
            (ok (eql (response-status response) 403))
            (ok (equal (response-header response :content-type) "text/plain; charset=utf-8"))
            (ok (eql (response-header response :content-length) 9))
            (ok (equal (response-body-string response) "Forbidden"))
            (ok (null reached) "the innermost application never ran"))))))

  (testing "the rejection travels through the stages above it untouched"
    ;; The session stage is outside the rejecting stage, so it still gets to
    ;; finalize the response: the 403 carries the session cookie exactly as a
    ;; served response would.  This is what makes returning a triple from a
    ;; stage the natural way to reject -- nothing above has to know.
    (with-no-loggers
      (with-middleware-stack (app :inner-middleware (list (make-rejecting-middleware)))
        (let ((response (call-app app "/todos" :method :post)))
          (ok (eql (response-status response) 403))
          (ok (response-set-cookie response) "Set-Cookie is still emitted")))))

  (testing "a stage that only rejects some requests lets the others through"
    (with-no-loggers
      (with-middleware-stack (app
                              :inner-middleware
                              (list (lambda (app)
                                      (lambda (env)
                                        (if (eq (getf env :request-method) :post)
                                            (reject-forbidden env :reason "unsafe method")
                                            (funcall app env))))))
        (let ((rejected (call-app app "/todos" :method :post))
              (served (call-app app "/todos" :method :get)))
          (ok (eql (response-status rejected) 403))
          (ok (equal (response-body-string rejected) "Forbidden"))
          (ok (eql (response-status served) 200))
          (ok (equal (response-body-string served) "ok"))))))

  (testing "FORBIDDEN-APP is usable as the application a stage blocks with"
    ;; lack/middleware/csrf takes such an application as :block-app; M6 will
    ;; hand it this one.
    (with-no-loggers
      (with-middleware-stack (app
                              :inner-middleware
                              (list (lambda (app)
                                      (declare (ignore app))
                                      #'forbidden-app)))
        (let ((response (call-app app "/todos" :method :post)))
          (ok (eql (response-status response) 403))
          (ok (equal (response-body-string response) "Forbidden")))))))


;;; ------------------------------------------------------------------
;;; Where the rejection is recorded
;;; ------------------------------------------------------------------

(deftest forbidden-audit-record-test
  (testing "the rejection is recorded on the audit logger, with the request"
    (let ((audit (with-captured-logger (:audit)
                   (with-middleware-stack (app
                                           :inner-middleware
                                           (list (make-rejecting-middleware
                                                  :reason "CSRF token missing"
                                                  :context '(:user-id 42
                                                             :action "delete"))))
                     (call-app app "/todos/7" :method :post)))))
      (ok (search "CSRF token missing" audit) "the reason is the record's message")
      (ok (search "INFO" audit) "recorded at :info, like every audit record")
      (ok (search "status=403" audit))
      (ok (search "request-method=:POST" audit))
      (ok (search "path=\"/todos/7\"" audit))
      (ok (search "user-id=42" audit) "the stage's own context is kept")
      (ok (search "action=\"delete\"" audit))))

  (testing "nothing is recorded on the web-access logger"
    ;; A6: the generated project's production configuration leaves
    ;; :web-access unregistered (its block registers :sql by mistake), and a
    ;; record sent to an unregistered logger is silently dropped.  The
    ;; rejection path must therefore never use it -- here the ONLY registered
    ;; logger is :web-access, so anything printed would be a record that went
    ;; to the wrong destination.
    (let ((web-access (with-captured-logger (:web-access)
                        (with-middleware-stack (app
                                                :inner-middleware
                                                (list (make-rejecting-middleware)))
                          (call-app app "/todos" :method :post)))))
      (ok (equal web-access "") "the rejection did not reach :web-access")))

  (testing "an unregistered audit logger drops the record but still answers 403"
    ;; The framework must not depend on the application having configured a
    ;; logger: a project with no :audit logger (and no :root to fall back to)
    ;; still gets its 403, and nothing is printed anywhere.
    (let (status)
      (let ((output (with-no-loggers
                      (with-output-to-string (*standard-output*)
                        (with-middleware-stack (app
                                                :inner-middleware
                                                (list (make-rejecting-middleware)))
                          (setf status (response-status
                                        (call-app app "/todos" :method :post))))))))
        (ok (null (with-no-loggers (get-logger :audit))))
        (ok (eql status 403))
        (ok (equal output "")))))

  (testing "LOG-REJECTION leaves out what the request does not say"
    (let ((audit (with-captured-logger (:audit)
                   (log-rejection nil :reason "no request at all"))))
      (ok (search "no request at all" audit))
      (ok (search "status=403" audit))
      (ok (not (search "path=" audit)) "no key with a NIL value")
      (ok (not (search "request-method=" audit)))
      (ok (not (search "ip-address=" audit)))))

  (testing "the client address is recorded when the request carries one"
    (let ((audit (with-captured-logger (:audit)
                   (log-rejection (list :request-method :get
                                        :path-info "/admin"
                                        :remote-addr "192.168.1.1")))))
      (ok (search "Request rejected" audit) "the default audit message")
      (ok (search "request-method=:GET" audit))
      (ok (search "path=\"/admin\"" audit))
      (ok (search "ip-address=\"192.168.1.1\"" audit)))))
