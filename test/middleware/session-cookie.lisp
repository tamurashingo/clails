(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-cookie
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  ;; MAKE-CLAILS-COOKIE-STATE and +DEFAULT-SESSION-COOKIE-NAME+ are not
  ;; listed here: clails-test/support/http already re-exports both (the
  ;; constant by reading the framework's own value), and importing them twice
  ;; would be a name conflict.  The framework's constant is referred to
  ;; fully-qualified where the point is that the two agree.
  (:import-from #:clails/middleware/session/state/cookie
                #:clails-cookie-state
                #:clails-cookie-state-path
                #:clails-cookie-state-domain
                #:clails-cookie-state-secure
                #:clails-cookie-state-httponly
                #:clails-cookie-state-cookie-key
                #:clails-cookie-state-samesite)
  (:import-from #:clails/middleware/session-middleware
                #:*session-cookie-secure*
                #:*session-state*
                #:*enable-session-middleware*)
  (:import-from #:clails/environment
                #:*project-environment*))
(in-package #:clails-test/middleware/session-cookie)


;;; M5-T4: the session cookie clails emits, and the one configuration
;;; variable that goes with it.
;;;
;;; Three of the defaults clails inherited from the underlying library were
;;; unsafe or wrong, and the fourth could not be expressed through that
;;; library's constructor at all -- which is why clails now has a cookie state
;;; of its own (clails/middleware/session/state/cookie).  This suite is the
;;; record of what that state emits:
;;;
;;;   HttpOnly    on, so a cross-site script cannot read the session id
;;;   SameSite    stated explicitly (Lax); leaving it out makes the response
;;;               builder default to None AND warn on every response
;;;   name        says nothing about the implementation
;;;   expires     NOT EMITTED -- the deadline is the server-side idle
;;;               timeout, and a date on the cookie would contradict it
;;;   Secure      off by default, read from one configuration variable
;;;
;;; The baseline this replaces is recorded in
;;; clails-test/middleware/session-http-roundtrip, whose "current defaults"
;;; block was updated by this task.


;;; ------------------------------------------------------------------
;;; The state the framework builds by default
;;; ------------------------------------------------------------------

(deftest session-cookie-default-state-test
  (testing "the default state is clails' own, not the library's"
    ;; The one place the default is made is the session stage, and what it
    ;; makes has to be the state defined by clails: the library's state
    ;; cannot express a cookie without an expiry date (its slot is typed as
    ;; an integer, so NIL fails at construction, and 0 means "expires now").
    (let ((seen-state :unset))
      (with-middleware-stack (app :inner (make-recording-app
                                          (lambda (env) (declare (ignore env)))))
        (declare (ignore app))
        ;; the stack above resolved *session-state*; build the default the
        ;; same way the stage does and check its type
        (setf seen-state (make-clails-cookie-state :secure *session-cookie-secure*)))
      (ok (typep seen-state 'clails-cookie-state))))

  (testing "its defaults are the safe ones"
    (let ((state (make-clails-cookie-state)))
      (ok (string= (clails-cookie-state-path state) "/"))
      (ok (null (clails-cookie-state-domain state)))
      (ok (eq (clails-cookie-state-httponly state) t)
          "HttpOnly is on by default")
      (ok (eq (clails-cookie-state-secure state) nil)
          "Secure is off by default")
      (ok (eq (clails-cookie-state-samesite state) :lax)
          "SameSite is stated, not left to the response builder's default")
      (ok (string= (clails-cookie-state-cookie-key state)
                   +default-session-cookie-name+))
      (ok (string= +default-session-cookie-name+
                   clails/middleware/session/state/cookie:+default-session-cookie-name+)
          "the test helpers' notion of the default name is the framework's")))

  (testing "the default cookie name does not give the implementation away"
    (ok (string/= +default-session-cookie-name+ "lack.session")
        "the library's name is gone")
    (ok (null (search "lack" (string-downcase +default-session-cookie-name+)))
        "and so is any mention of the library")
    (ok (null (search "clails" (string-downcase +default-session-cookie-name+)))
        "the framework is not named either")
    (ok (plusp (length +default-session-cookie-name+))))

  (testing "there is no way to ask this state for an expiry date"
    ;; Not an accident: the absence of the slot is the mechanism.  A caller
    ;; that really wants a persistent cookie uses the library's state
    ;; instead (MAKE-TEST-COOKIE-STATE here), which is still accepted in
    ;; *SESSION-STATE*.
    (ok (handler-case (progn (make-clails-cookie-state :expires 60) nil)
          (error () t))
        "passing :EXPIRES is refused rather than silently ignored")))


;;; ------------------------------------------------------------------
;;; What reaches the client
;;; ------------------------------------------------------------------

(defun creating-response-cookie (&key state)
  "The Set-Cookie of a session-creating response, parsed.

   The session-creating response is the only kind on which cookie attributes
   can be asserted: the stage attaches Set-Cookie only for a new session, a
   changed id or a destroyed session."
  (with-middleware-stack (app :state state)
    (response-set-cookie (call-app app "/"))))

(deftest session-cookie-attributes-test
  (testing "the default response carries HttpOnly, SameSite and path"
    (let ((cookie (creating-response-cookie)))
      (ok cookie)
      (ok (string= (set-cookie-name cookie) +default-session-cookie-name+))
      (ok (= (length (set-cookie-value cookie)) 40)
          "the value is the 40 hexadecimal character session id")
      (ok (set-cookie-attribute-present-p cookie "httponly"))
      (ok (eq (set-cookie-attribute cookie "httponly") t))
      (ok (string= (set-cookie-attribute cookie "samesite") "Lax"))
      (ok (string= (set-cookie-attribute cookie "path") "/"))))

  (testing "the default response carries NO expiry date"
    ;; The condition this whole file exists for.  A cookie date would become
    ;; an absolute deadline, because the cookie is not re-sent on a
    ;; continuing request -- so the 30 minute idle policy has to live on the
    ;; server alone.
    (let ((cookie (creating-response-cookie)))
      (ok (not (set-cookie-attribute-present-p cookie "expires"))
          "no expires attribute at all")
      (ok (not (set-cookie-attribute-present-p cookie "max-age"))
          "and no Max-Age either")
      (ok (null (set-cookie-expires-at cookie)))
      (ok (not (set-cookie-expired-p cookie))
          "so the cookie is a session cookie, alive until the browser closes")))

  (testing "the 2153 expiry of the old default is gone"
    ;; The library's state initialises its expiry slot to the current
    ;; universal time and then emits (+ (current time) that slot), i.e. a
    ;; date about 127 years out.  Asserted as absent rather than described,
    ;; so the defect cannot come back unnoticed.
    (let* ((cookie (creating-response-cookie))
           (raw (set-cookie-attribute cookie "expires")))
      (ok (null raw))))

  (testing "Secure is absent by default, so a plain-HTTP setup works"
    (let ((cookie (creating-response-cookie)))
      (ok (not (set-cookie-attribute-present-p cookie "secure")))))

  (testing "domain is absent unless it is configured"
    (ok (not (set-cookie-attribute-present-p (creating-response-cookie) "domain")))
    (let ((cookie (creating-response-cookie
                   :state (make-clails-cookie-state :domain "example.test"))))
      (ok (string= (set-cookie-attribute cookie "domain") "example.test"))))

  (testing "every attribute can be configured, and the response shows it"
    (let ((cookie (creating-response-cookie
                   :state (make-clails-cookie-state
                           :cookie-key "_app_sid"
                           :path "/app"
                           :domain "example.test"
                           :secure t
                           :httponly nil
                           :samesite :strict))))
      (ok (string= (set-cookie-name cookie) "_app_sid"))
      (ok (string= (set-cookie-attribute cookie "path") "/app"))
      (ok (string= (set-cookie-attribute cookie "domain") "example.test"))
      (ok (set-cookie-attribute-present-p cookie "secure"))
      (ok (not (set-cookie-attribute-present-p cookie "httponly"))
          "HttpOnly can be turned off, for the application that needs it")
      (ok (string= (set-cookie-attribute cookie "samesite") "Strict"))
      ;; still no date, whatever else is configured
      (ok (not (set-cookie-attribute-present-p cookie "expires"))))))


;;; ------------------------------------------------------------------
;;; The Secure configuration variable (Q13)
;;; ------------------------------------------------------------------

(deftest session-cookie-secure-setting-test
  (testing "the framework default is false"
    ;; Deliberately NOT the safe value: a Secure cookie is not sent over
    ;; plain HTTP, so defaulting it to true would stop sessions working in
    ;; every development environment without TLS.  Production gets the true
    ;; value from the generated project's configuration (M8-T1).
    (ok (null *session-cookie-secure*)))

  (testing "it is declared so that re-reading the configuration keeps the value"
    ;; B5's rule: DEFVAR, not DEFPARAMETER.  An application sets the variable
    ;; in app/config/environment.lisp, and loading the framework again (a
    ;; reload in a running image, a second ASDF load) must not put the
    ;; framework default back.
    (let ((symbol 'clails/middleware/session-middleware::*session-cookie-secure*))
      (unwind-protect
           (progn
             (setf (symbol-value symbol) t)
             (eval (list 'defvar symbol nil))
             (ok (eq (symbol-value symbol) t)
                 "the value the application set survived the re-declaration"))
        (setf (symbol-value symbol) nil))))

  (testing "the framework reads the variable and never the environment name"
    ;; The same configuration has to produce the same cookie in every
    ;; environment: clails does not look at *PROJECT-ENVIRONMENT*.
    (let ((original *project-environment*))
      (unwind-protect
           (dolist (environment '(:development :test :production))
             (setf *project-environment* environment)
             (let ((cookie (creating-response-cookie)))
               (ok (not (set-cookie-attribute-present-p cookie "secure"))
                   (format nil "~A: Secure stays off while the variable is false"
                           environment)))
             (let ((clails/middleware/session-middleware:*session-cookie-secure* t))
               (let ((cookie (creating-response-cookie)))
                 (ok (set-cookie-attribute-present-p cookie "secure")
                     (format nil "~A: Secure follows the variable, not the environment"
                             environment)))))
        (setf *project-environment* original))))

  (testing "the stage puts the variable's value into the state it builds"
    (let ((clails/middleware/session-middleware:*session-cookie-secure* t))
      (let ((cookie (creating-response-cookie)))
        (ok (set-cookie-attribute-present-p cookie "secure"))
        (ok (eq (set-cookie-attribute cookie "secure") t))
        ;; the rest of the defaults are unchanged by it
        (ok (set-cookie-attribute-present-p cookie "httponly"))
        (ok (string= (set-cookie-attribute cookie "samesite") "Lax"))
        (ok (not (set-cookie-attribute-present-p cookie "expires")))))))


(deftest session-cookie-roundtrip-under-both-settings-test
  ;; The point of the default is that sessions WORK without TLS.  Both
  ;; configurations are driven through a full round trip, because "the
  ;; attribute is present" is not the same claim as "the session still
  ;; carries over".
  (testing "a session carries over with Secure off (a plain-HTTP setup)"
    (let ((clails/middleware/session-middleware:*session-cookie-secure* nil))
      (with-middleware-stack (app :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/")))
          (ok (string= (response-body-string first-response) "1"))
          (ok (not (set-cookie-attribute-present-p
                    (response-set-cookie first-response) "secure"))
              "the cookie that was issued never asked for HTTPS")
          (ok (string= (response-body-string (client-request client "/")) "2"))
          (ok (string= (response-body-string (client-request client "/")) "3"))
          (ok (null (session-client-last-set-cookie client))
              "and the continuing requests issued no cookie at all")))))

  (testing "a session carries over with Secure on as well"
    ;; The test client is not a browser and sends the cookie back regardless
    ;; of the attribute, which is exactly what makes this assertion about the
    ;; framework rather than about the client: everything else in the stage
    ;; has to keep working when the flag is set.
    (let ((clails/middleware/session-middleware:*session-cookie-secure* t))
      (with-middleware-stack (app :inner (make-session-counter-app))
        (let* ((client (make-session-client app))
               (first-response (client-request client "/")))
          (ok (string= (response-body-string first-response) "1"))
          (ok (set-cookie-attribute-present-p
               (response-set-cookie first-response) "secure"))
          (ok (string= (response-body-string (client-request client "/")) "2"))
          (ok (string= (response-body-string (client-request client "/")) "3"))))))

  (testing "a cookie name of the application's choosing carries over too"
    (with-middleware-stack (app :state (make-clails-cookie-state
                                        :cookie-key "_app_sid")
                                :inner (make-session-counter-app))
      (let ((client (make-session-client app :session-cookie-name "_app_sid")))
        (ok (string= (response-body-string (client-request client "/")) "1"))
        (ok (string= (set-cookie-name (session-client-last-set-cookie client))
                     "_app_sid")
            "the session-creating response named the configured cookie")
        (ok (string= (response-body-string (client-request client "/")) "2")
            "and the client's cookie carried the session over")))))


(deftest session-cookie-library-state-still-accepted-test
  ;; *SESSION-STATE* keeps accepting the library's own state: it is how an
  ;; application asks for a persistent cookie, which the clails state cannot
  ;; express.  Nothing about that path changed in M5-T4.
  (testing "the library's cookie state still works, expiry date and all"
    (with-middleware-stack (app :state (make-test-cookie-state
                                        :cookie-key "_legacy_session"
                                        :expires 3600)
                                :inner (make-session-counter-app))
      (let* ((client (make-session-client app :session-cookie-name "_legacy_session"))
             (first-response (client-request client "/"))
             (cookie (response-set-cookie first-response)))
        (ok (string= (set-cookie-name cookie) "_legacy_session"))
        (ok (set-cookie-attribute-present-p cookie "expires")
            "the library's state does emit a date -- that is why it is still available")
        (ok (string= (response-body-string (client-request client "/")) "2")
            "and the session carries over")))))
