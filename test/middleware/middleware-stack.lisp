(in-package #:cl-user)
(defpackage #:clails-test/middleware/middleware-stack
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:lack/request
                #:make-request
                #:request-body-parameters)
  ;; The CSRF stage used below is lack's, imported through the "lack/middleware/csrf"
  ;; system alias declared in lack.asd.  M6 is the milestone that adds a CSRF
  ;; stage to the framework itself; here it only has to prove that a stack
  ;; carrying such a stage can be assembled from a test.
  (:import-from #:lack/middleware/csrf
                #:*lack-middleware-csrf*
                #:csrf-token)
  (:import-from #:clails/middleware/session-middleware
                #:*enable-session-middleware*
                #:*session-store*
                #:*session-state*))
(in-package #:clails-test/middleware/middleware-stack)


;;; ------------------------------------------------------------------
;;; Stages used by the tests below
;;; ------------------------------------------------------------------

(defun make-trace-middleware (name trace)
  "A middleware that records NAME, in TRACE (a function of one argument),
   when the request passes through it.  Used to show where in the stack a
   stage ended up."
  (lambda (app)
    (lambda (env)
      (funcall trace name)
      (funcall app env))))

(defparameter +override-methods+
  '(("put" . :put) ("patch" . :patch) ("delete" . :delete))
  "Allow-list of methods a POST may be rewritten to.")

(defun make-method-override-middleware (&key (parameter "_method"))
  "A stand-in for the request-method rewriting stage that M9 will add to the
   framework.  It is not the real thing -- M9 owns that -- but it behaves the
   way M9 requires (POST only, body only, allow-list only, rewrites env's
   declared request method), which is enough to prove such a stage can be
   placed inside a stack built by a test."
  (lambda (app)
    (lambda (env)
      (when (eq (getf env :request-method) :post)
        (let* ((req (make-request env))
               (raw (cdr (assoc parameter (request-body-parameters req) :test #'string=)))
               (override (and (stringp raw)
                              (cdr (assoc (string-downcase raw) +override-methods+
                                          :test #'string=)))))
          (when override
            (setf (getf env :request-method) override))))
      (funcall app env))))

(defun csrf-stage (&rest args)
  "The lack CSRF stage as a one-argument middleware."
  (lambda (app) (apply *lack-middleware-csrf* app args)))

(defun forbidden (env)
  (declare (ignore env))
  (list 403 (list :content-type "text/plain") (list "forbidden")))


;;; ------------------------------------------------------------------
;;; Configuring the stack per test
;;; ------------------------------------------------------------------

(deftest middleware-stack-configuration-test
  (testing "session disabled: the stage is a pass-through"
    (let ((seen-session :unset))
      (with-middleware-stack (app :session nil
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen-session (getf env :lack.session)))))
        (let ((response (call-app app "/")))
          (ok (= (response-status response) 200))
          (ok (null seen-session))
          (ok (null (response-header response :set-cookie)))))))

  (testing "session enabled with an explicit store"
    (let ((store (make-memory-session-store))
          (seen-session :unset))
      (with-middleware-stack (app :session t
                                  :store store
                                  :inner (make-recording-app
                                          (lambda (env)
                                            (setf seen-session (getf env :lack.session))
                                            (setf (gethash "k" (getf env :lack.session)) "v"))))
        (let* ((response (call-app app "/"))
               (session-id (response-session-id response)))
          (ok (hash-table-p seen-session))
          (ok session-id)
          ;; the store we handed in is the store that was used
          (ok (eq (fetch-stored-session store session-id) seen-session))
          (ok (string= (gethash "k" (fetch-stored-session store session-id)) "v"))))))

  (testing "session enabled with an explicit cookie state"
    (with-middleware-stack (app :session t
                                :state (make-test-cookie-state
                                        :cookie-key "_other_session"
                                        :path "/scoped"))
      (let ((cookie (response-set-cookie (call-app app "/"))))
        (ok (string= (set-cookie-name cookie) "_other_session"))
        (ok (string= (set-cookie-attribute cookie "path") "/scoped")))))

  (testing "a cookie minted by one configuration is not accepted by another"
    (let ((cookie nil))
      (with-middleware-stack (app :inner (make-session-counter-app))
        (let ((response (call-app app "/")))
          (setf cookie (response-cookie-pair response))
          (ok (string= (response-body-string response) "1"))))
      ;; a different store means a different (empty) session
      (with-middleware-stack (app :inner (make-session-counter-app))
        (ok (string= (response-body-string (call-app app "/" :cookie cookie)) "1"))))))


(deftest middleware-stack-isolation-test
  (testing "the configuration variables are left as they were"
    (with-middleware-stack (app :session t
                                :store (make-memory-session-store)
                                :state (make-test-cookie-state :cookie-key "_leaky"))
      (call-app app "/"))
    (ok (null *enable-session-middleware*))
    (ok (null *session-store*))
    (ok (null *session-state*)))

  (testing "the order in which configurations run does not change the results"
    (labels ((enabled-case ()
               (with-middleware-stack (app :session t :inner (make-session-counter-app))
                 (let ((response (call-app app "/")))
                   (list (response-status response)
                         (response-body-string response)
                         (and (response-header response :set-cookie) t)))))
             (disabled-case ()
               (with-middleware-stack (app :session nil)
                 (let ((response (call-app app "/")))
                   (list (response-status response)
                         (response-body-string response)
                         (and (response-header response :set-cookie) t))))))
      (let* ((forwards (list (enabled-case) (disabled-case)))
             (backwards (reverse (list (disabled-case) (enabled-case)))))
        (ok (equal forwards backwards))
        (ok (equal (first forwards) (list 200 "1" t)))
        (ok (equal (second forwards) (list 200 "ok" nil)))))))


;;; ------------------------------------------------------------------
;;; Stacks with extra stages (the ground M6 and M9 stand on)
;;; ------------------------------------------------------------------

(deftest middleware-stack-extra-stages-test
  (testing "a stack of session -> CSRF -> method override -> application"
    (let* ((trace '())
           (record (lambda (name) (push name trace)))
           (seen-method nil)
           (inner (lambda (env)
                    (setf seen-method (getf env :request-method))
                    (list 200
                          (list :content-type "text/plain")
                          ;; CSRF-TOKEN may only be called while the CSRF
                          ;; stage is on the stack: it reads the dynamic
                          ;; session-key the stage binds per request.
                          (list (csrf-token (getf env :lack.session)))))))
      (with-middleware-stack (app
                              :inner inner
                              :inner-middleware (list (make-trace-middleware :inside-session record)
                                                      (csrf-stage)
                                                      (make-trace-middleware :inside-csrf record)
                                                      (make-method-override-middleware)
                                                      (make-trace-middleware :inside-override record)))
        (let* ((client (make-session-client app))
               (token (response-body-string (client-request client "/"))))
          ;; a safe method goes all the way through, and the session is there
          (ok (eq seen-method :get))
          (ok (= (length token) 40))
          ;; every stage really is on the stack, in the requested order
          (ok (equal (reverse trace) '(:inside-session :inside-csrf :inside-override)))

          ;; a POST without the token is stopped by the CSRF stage: it never
          ;; reaches the override stage nor the application
          (setf trace '())
          (setf seen-method nil)
          (let ((response (client-request client "/"
                                          :method :post
                                          :content '(("_method" . "delete")))))
            (ok (= (response-status response) 400))
            (ok (null seen-method))
            (ok (equal (reverse trace) '(:inside-session))))

          ;; with the token, the request passes the CSRF stage and the
          ;; override stage rewrites the method before the application runs
          (setf trace '())
          (let ((response (client-request client "/"
                                          :method :post
                                          :content (list (cons "_csrf_token" token)
                                                         (cons "_method" "delete")))))
            (ok (= (response-status response) 200))
            (ok (eq seen-method :delete))
            (ok (equal (reverse trace) '(:inside-session :inside-csrf :inside-override))))

          ;; a GET carrying a forged method is not rewritten: the stand-in
          ;; stage only looks at POST bodies
          (setf seen-method nil)
          (client-request client "/?_method=delete")
          (ok (eq seen-method :get))))))

  (testing "the CSRF rejection response is replaceable (the hook M2 needs)"
    (with-middleware-stack (app :inner-middleware (list (csrf-stage :block-app #'forbidden)))
      (let ((response (call-app app "/" :method :post)))
        (ok (= (response-status response) 403))
        (ok (string= (response-body-string response) "forbidden")))))

  (testing "a stage can also be placed outside the session stage"
    (let ((trace '()))
      (with-middleware-stack (app
                              :outer-middleware (list (make-trace-middleware
                                                       :outside-session
                                                       (lambda (name) (push name trace))))
                              :inner-middleware (list (make-trace-middleware
                                                       :inside-session
                                                       (lambda (name) (push name trace)))))
        (call-app app "/")
        (ok (equal (reverse trace) '(:outside-session :inside-session)))))))
