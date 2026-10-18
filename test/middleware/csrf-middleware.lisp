(in-package #:cl-user)
(defpackage #:clails-test/middleware/csrf-middleware
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf)
  (:import-from #:clails/environment
                #:*strict-configuration*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:*csrf-session-key*
                #:*csrf-form-token*
                #:*csrf-header-name*
                #:*csrf-one-time-token*
                #:*csrf-forbidden-message*
                #:+csrf-marker-key+
                #:+csrf-default-session-key+
                #:*lack-middleware-clails-csrf*
                #:*lack-middleware-clails-csrf-body-guard*
                #:csrf-configuration-error
                #:csrf-configuration-error-message
                #:check-csrf-configuration
                #:constant-time-string=)
  (:import-from #:clails/middleware/session-middleware
                #:*enable-session-middleware*
                #:*lack-middleware-clails-session*)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/core
                #:*clails-middleware-stack*)
  (:import-from #:clails/task
                #:register-task
                #:run-task))
(in-package #:clails-test/middleware/csrf-middleware)


;;; The CSRF stage seen from the inside: its configuration, the comparison it
;;; uses, the session key it writes, the marker it leaves and the
;;; configuration check it performs when a stack is built.  The behaviour of
;;; the stage against real requests lives in
;;; clails-test/middleware/csrf-http-roundtrip.


;;; ------------------------------------------------------------------
;;; Configuration (M6-T1, M6-T6, M6-T7)
;;; ------------------------------------------------------------------

(deftest csrf-configuration-defaults-test
  (testing "the stage is disabled by default"
    ;; An existing project must behave exactly as it did before: the
    ;; framework default is off, and a generated project opts in.
    (ok (null *enable-csrf-middleware*)))

  (testing "the token is kept under a session key carrying the clails prefix"
    ;; Policy: every session key the framework itself uses is prefixed, the
    ;; existing "clails.current-user" being the precedent.  lack's own default
    ;; ("_csrf_token") carries no prefix and would collide with an
    ;; application using that name.
    (ok (equal *csrf-session-key* "clails.csrf-token"))
    (ok (equal *csrf-session-key* +csrf-default-session-key+))
    (ok (eql (search "clails." *csrf-session-key*) 0)))

  (testing "the form field keeps lack's name"
    ;; This one is part of the HTML a form emits, not a session key.
    (ok (equal *csrf-form-token* "_csrf_token")))

  (testing "the request header read for a token is X-CSRF-Token"
    (ok (equal *csrf-header-name* "X-CSRF-Token")))

  (testing "tokens are reusable by default, not one-time"
    (ok (null *csrf-one-time-token*)))

  (testing "the rejection body is one plain line telling the user to reload"
    (ok (stringp *csrf-forbidden-message*))
    (ok (null (find #\Newline *csrf-forbidden-message*)))
    (ok (search "Reload" *csrf-forbidden-message*)))

  (testing "configuration strictness defaults to strict"
    (ok (eq *strict-configuration* t)))

  (testing "the marker key carries the clails prefix too"
    (ok (keywordp +csrf-marker-key+))
    (ok (eql (search "CLAILS." (symbol-name +csrf-marker-key+)) 0))))


(defmacro with-restored-value ((place) &body body)
  "Run BODY and put back whatever PLACE held, whatever BODY did to it.

   The declaration tests below have to assign to the global value rather than
   rebind it: rebinding is exactly what they are NOT testing."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original ,place))
       (unwind-protect (progn ,@body)
         (setf ,place ,original)))))

(deftest csrf-configuration-declaration-test
  ;; R28: the settings are declared with DEFVAR, not DEFPARAMETER, so that a
  ;; value an application set in app/config/environment.lisp survives the
  ;; declaration being evaluated again -- which happens whenever the system is
  ;; reloaded into a running image.
  (testing "re-evaluating the declaration does not reset *enable-csrf-middleware*"
    (with-restored-value (*enable-csrf-middleware*)
      (setf *enable-csrf-middleware* t)
      (eval '(defvar clails/middleware/csrf-middleware:*enable-csrf-middleware* nil))
      (ok (eq *enable-csrf-middleware* t))))

  (testing "re-evaluating the declaration does not reset *csrf-session-key*"
    (with-restored-value (*csrf-session-key*)
      (setf *csrf-session-key* "app.token")
      (eval '(defvar clails/middleware/csrf-middleware:*csrf-session-key* "clails.csrf-token"))
      (ok (equal *csrf-session-key* "app.token"))))

  (testing "re-evaluating the declaration does not reset *csrf-one-time-token*"
    (with-restored-value (*csrf-one-time-token*)
      (setf *csrf-one-time-token* t)
      (eval '(defvar clails/middleware/csrf-middleware:*csrf-one-time-token* nil))
      (ok (eq *csrf-one-time-token* t))))

  (testing "re-evaluating the declaration does not reset *csrf-header-name*"
    (with-restored-value (*csrf-header-name*)
      (setf *csrf-header-name* "X-App-Token")
      (eval '(defvar clails/middleware/csrf-middleware:*csrf-header-name* "X-CSRF-Token"))
      (ok (equal *csrf-header-name* "X-App-Token"))))

  (testing "re-evaluating the declaration does not reset *strict-configuration*"
    (with-restored-value (*strict-configuration*)
      (setf *strict-configuration* nil)
      (eval '(defvar clails/environment:*strict-configuration* t))
      (ok (null *strict-configuration*)))))


;;; ------------------------------------------------------------------
;;; Comparing tokens (M6-T5)
;;; ------------------------------------------------------------------

(deftest constant-time-string-test
  (testing "equal strings compare equal"
    (ok (constant-time-string= "abc" "abc"))
    (ok (constant-time-string= "abc" (copy-seq "abc")))
    (ok (constant-time-string= (make-string 40 :initial-element #\a)
                               (make-string 40 :initial-element #\a))))

  (testing "strings of the same length that differ do not"
    (ok (not (constant-time-string= "abc" "abd")))
    (ok (not (constant-time-string= "abc" "bbc")))
    (ok (not (constant-time-string= "abc" "aBc")) "the comparison is case sensitive"))

  (testing "strings of different lengths do not"
    (ok (not (constant-time-string= "abc" "abcd")))
    (ok (not (constant-time-string= "abcd" "abc")))
    (ok (not (constant-time-string= "a" (make-string 40 :initial-element #\a)))))

  (testing "the empty string compares equal only to itself"
    (ok (constant-time-string= "" ""))
    (ok (not (constant-time-string= "" "a")))
    (ok (not (constant-time-string= "a" ""))))

  (testing "NIL is not equal to anything, and does not signal"
    (ok (not (constant-time-string= nil nil)))
    (ok (not (constant-time-string= nil "abc")))
    (ok (not (constant-time-string= "abc" nil))))

  (testing "a value that is not a string is not equal either"
    ;; A JSON body may well carry the token as a number; it must come out as
    ;; "not the token" rather than as an error.
    (ok (not (constant-time-string= 12345 "12345")))
    (ok (not (constant-time-string= "12345" 12345)))
    (ok (not (constant-time-string= :abc "abc")))
    (ok (not (constant-time-string= '("abc") "abc"))))

  (testing "the result is a true boolean, not a leftover accumulator"
    (ok (eq t (constant-time-string= "abc" "abc")))
    (ok (eq nil (constant-time-string= "abc" "abd")))))


;;; ------------------------------------------------------------------
;;; Enabled / disabled (M6-T1)
;;; ------------------------------------------------------------------

(deftest csrf-enable-switch-test
  (testing "enabled: a state-changing request without a token is refused"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :store (make-memory-session-store))
        (ok (eql (response-status (call-app app "/" :method :post)) 403)))))

  (testing "disabled: the stage is a pass-through"
    (let ((reached 0))
      (with-no-loggers
        (with-csrf-stack (app :csrf nil
                              :inner (make-recording-app (lambda (env)
                                                           (declare (ignore env))
                                                           (incf reached))))
          (ok (eql (response-status (call-app app "/" :method :post)) 200))
          (ok (eql (response-status (call-app app "/" :method :delete)) 200))
          (ok (eql reached 2) "the application ran for both requests")))))

  (testing "disabled: nothing is written into the session and no marker is left"
    (let ((store (make-memory-session-store))
          (marker :unset))
      (with-csrf-stack (app :csrf nil
                            :store store
                            :inner (make-recording-app
                                    (lambda (env)
                                      (setf marker (getf env +csrf-marker-key+))
                                      (setf (gethash "k" (getf env :lack.session)) "v"))))
        (let* ((response (call-app app "/"))
               (session (fetch-stored-session store (response-session-id response))))
          (ok (null marker) "no marker when the stage is a pass-through")
          (ok (null (gethash *csrf-session-key* session)) "no token was minted")
          (ok (equal (gethash "k" session) "v") "the session itself still works")))))

  (testing "disabled: the body guard is a pass-through as well"
    ;; With the CSRF stage off, a malformed body is left to whoever reads it,
    ;; i.e. the behaviour of an existing project is untouched.
    (with-no-loggers
      (let ((guarded (funcall *lack-middleware-clails-csrf-body-guard* #'ok-app)))
        (ok (eq guarded #'ok-app)
            "the guard returns the application unchanged when CSRF is disabled")))))


;;; ------------------------------------------------------------------
;;; The session key the token is stored under (M6-T1, policy 13)
;;; ------------------------------------------------------------------

(deftest csrf-session-key-test
  (testing "the token is stored under \"clails.csrf-token\""
    (let ((store (make-memory-session-store)))
      (with-csrf-stack (app :inner (make-token-app) :store store)
        (let* ((client (open-session app))
               (token (fetch-token client))
               (session-id (set-cookie-value (session-client-last-set-cookie client)))
               (session (fetch-stored-session store session-id)))
          (ok (equal (gethash "clails.csrf-token" session) token)
              "the session holds the token under the prefixed key")
          (ok (null (gethash "_csrf_token" session))
              "and nothing under lack's unprefixed default")))))

  (testing "an application key spelled like lack's default does not disturb the stage"
    ;; This is the collision the prefix avoids, asserted from the outside: an
    ;; application that keeps its own value under "_csrf_token" neither
    ;; satisfies nor breaks the verification.
    (let ((store (make-memory-session-store)))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-token-app
                                      :observer (lambda (env)
                                                  (setf (gethash "_csrf_token" (getf env :lack.session))
                                                        "an application value")))
                              :store store)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 ;; Read the session id now: a later request does not set a
                 ;; cookie again, the session already being open.
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (eql (response-status
                      (client-request client "/" :method :post
                                                 :content (form-content token)))
                     200)
                "the real token is still accepted")
            (ok (eql (response-status
                      (client-request client "/" :method :post
                                                 :content (form-content "an application value")))
                     403)
                "the application's value is not accepted as a token")
            (let ((session (fetch-stored-session store session-id)))
              (ok (equal (gethash "_csrf_token" session) "an application value")
                  "and the application's own value is untouched")))))))

  (testing "the session key is configurable"
    (let ((store (make-memory-session-store)))
      (with-csrf-stack (app :inner (make-token-app)
                            :store store
                            :session-key "clails.other-token")
        (let* ((client (open-session app))
               (token (fetch-token client))
               (session (fetch-stored-session
                         store
                         (set-cookie-value (session-client-last-set-cookie client)))))
          (ok (equal (gethash "clails.other-token" session) token))
          (ok (null (gethash "clails.csrf-token" session))))))))


;;; ------------------------------------------------------------------
;;; The marker the stage leaves (M6-T7, checked configuration 2)
;;; ------------------------------------------------------------------

(deftest csrf-marker-test
  (testing "a request the stage lets through carries the marker"
    (let ((markers '()))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-token-app
                                      :observer (lambda (env)
                                                  (push (getf env +csrf-marker-key+) markers)))
                              :store (make-memory-session-store))
          (let* ((client (open-session app))
                 (token (fetch-token client)))
            (client-request client "/" :method :post :content (form-content token))
            (ok (equal markers '(t t))
                "both the safe request and the verified one were marked"))))))

  (testing "a request the stage refuses never reaches the marking"
    (let ((reached '()))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-recording-app
                                      (lambda (env) (push (getf env +csrf-marker-key+) reached)))
                              :store (make-memory-session-store))
          (ok (eql (response-status (call-app app "/" :method :post)) 403))
          (ok (null reached) "the application below the stage never ran")))))

  (testing "a stage placed inside the CSRF stage sees the marker"
    ;; This is the mechanism M9-T1 reads: the request-method rewriting stage
    ;; goes inside this one and checks for the marker, so that a stack whose
    ;; order has been broken is detected from the order the request actually
    ;; took rather than from the shape of the middleware list.
    (let ((seen :unset))
      (with-no-loggers
        (with-csrf-stack (app
                          :store (make-memory-session-store)
                          :after-csrf (list (lambda (inner)
                                              (lambda (env)
                                                (setf seen (getf env +csrf-marker-key+))
                                                (funcall inner env)))))
          (call-app app "/")
          (ok (eq seen t))))))

  (testing "a stage placed outside the CSRF stage does not see it"
    (let ((seen :unset))
      (with-no-loggers
        (with-csrf-stack (app
                          :store (make-memory-session-store)
                          :before-csrf (list (lambda (inner)
                                               (lambda (env)
                                                 (setf seen (getf env +csrf-marker-key+))
                                                 (funcall inner env)))))
          (call-app app "/")
          (ok (null seen)))))))


;;; ------------------------------------------------------------------
;;; The configuration check (M6-T7, checked configuration 1)
;;; ------------------------------------------------------------------
;;;
;;; "CSRF enabled, session disabled" cannot serve a single state-changing
;;; request: lack's stage signals when :lack.session is missing and nothing in
;;; the stack turns a condition into a response.  The check runs where the
;;; stack is assembled -- the stage's own builder -- which is the one place
;;; that runs exactly once per stack and never runs for a task or the console.

(defun build-stack (&key session csrf strict)
  "Assemble a session/CSRF stack with the given configuration, and return the
   resulting application.  Nothing is requested; building is the point."
  (let ((*enable-session-middleware* session)
         (*enable-csrf-middleware* csrf)
         (*strict-configuration* strict))
    (compose-middleware (list *lack-middleware-clails-session*
                              *lack-middleware-clails-csrf*)
                        #'ok-app)))

(deftest csrf-configuration-check-strict-test
  (testing "strict: a CSRF stage without a session stage aborts the build"
    (with-no-loggers
      (let ((condition (handler-case (progn (build-stack :session nil :csrf t :strict t)
                                            nil)
                         (csrf-configuration-error (c) c))))
        (ok (typep condition 'csrf-configuration-error)
            "building the stack signalled, so startup cannot continue")
        (let ((message (csrf-configuration-error-message condition)))
          (ok (search "CSRF" message) "the message names the cause")
          (ok (search "session middleware is not" message))
          (ok (search "*enable-session-middleware*" message)
              "and tells the reader how to fix it")
          (ok (search "*enable-csrf-middleware*" message))
          (ok (search "*strict-configuration*" message)
              "including how to downgrade the check")
          (ok (equal (princ-to-string condition) message)
              "the report is the message itself")))))

  (testing "not strict: the same configuration warns and the build succeeds"
    (let (app)
      (let ((audit (with-captured-logger (:audit)
                     (setf app (build-stack :session nil :csrf t :strict nil)))))
        (ok (functionp app) "the stack was built")
        (ok (search "WARN" audit) "the warning was recorded")
        (ok (search "session middleware is not" audit))
        (ok (search "stage=:CSRF" audit)))))

  (testing "the warning goes to the audit logger, not to the package logger"
    ;; A6: the generated configuration sets the framework's package logger to
    ;; :error in development and test, so a warning recorded there would never
    ;; be seen.  Here the only registered logger is the package logger's
    ;; name, so anything printed would have gone to the wrong place.
    (let ((package-log (with-captured-logger (:clails/middleware/csrf-middleware)
                         (build-stack :session nil :csrf t :strict nil))))
      (ok (equal package-log ""))))

  (testing "a working configuration never trips the check"
    (dolist (strict '(t nil))
      (with-no-loggers
        (ok (functionp (build-stack :session t :csrf t :strict strict))))
      (let ((audit (with-captured-logger (:audit)
                     (build-stack :session t :csrf t :strict strict))))
        (ok (equal audit "") "and records nothing"))))

  (testing "a disabled CSRF stage never trips it either, session or no session"
    (dolist (strict '(t nil))
      (dolist (session '(t nil))
        (with-no-loggers
          (ok (functionp (build-stack :session session :csrf nil :strict strict))))
        (let ((audit (with-captured-logger (:audit)
                       (build-stack :session session :csrf nil :strict strict))))
          (ok (equal audit ""))))))

  (testing "the configuration the check is about really is unserveable"
    ;; Why aborting the build is proportionate: with the session stage
    ;; missing, every state-changing request signals, and nothing in the
    ;; stack turns that into a response -- so the condition escapes the stack
    ;; and the request dies rather than being answered.  A safe request still
    ;; works, which is exactly what makes the broken configuration easy to
    ;; miss without the check.
    (with-no-loggers
      (let ((app (build-stack :session nil :csrf t :strict nil)))
        (ok (eql (response-status (call-app app "/")) 200)
            "a safe request is served, so nothing looks wrong")
        (ok (signals (call-app app "/" :method :post) 'error)
            "and a state-changing request lets a condition out of the stack"))))

  (testing "the framework does not decide strictness from the environment"
    ;; The value is read from the setting and from nowhere else: the framework
    ;; has no branch on *project-environment*, so the same configuration
    ;; behaves the same way in every environment.  A generated project is what
    ;; puts a different value in development and in production.
    (dolist (environment '(:develop :test :production))
      (let ((clails/environment:*project-environment* environment))
        (with-no-loggers
          (ok (signals (build-stack :session nil :csrf t :strict t)
                       'csrf-configuration-error)
              (format nil "strict is still strict in ~A" environment)))
        (let ((audit (with-captured-logger (:audit)
                       (build-stack :session nil :csrf t :strict nil))))
          (ok (search "session middleware is not" audit)
              (format nil "and lenient is still lenient in ~A" environment))))))

  (testing "CHECK-CSRF-CONFIGURATION can also be called on its own"
    (with-no-loggers
      (let ((*enable-session-middleware* t)
            (*strict-configuration* t))
        (ok (null (check-csrf-configuration))))
      (let ((*enable-session-middleware* nil)
            (*strict-configuration* t))
        (ok (signals (check-csrf-configuration) 'csrf-configuration-error))))))


(deftest csrf-configuration-check-reach-test
  (testing "the default middleware stack of an existing project passes the check"
    ;; Both stages are off by default, so a project that has opted in to
    ;; neither is unaffected by the check even though it is strict.
    (ok (null *enable-session-middleware*))
    (ok (null *enable-csrf-middleware*))
    (ok (eq *strict-configuration* t))
    (with-no-loggers
      (ok (functionp (compose-middleware *clails-middleware-stack* #'not-found-app))
          "the framework's own stack assembles with the check in force")))

  (testing "the framework stack puts the stages in the required order"
    ;; Session outside CSRF (the token lives in the session), CSRF outside
    ;; routing (a request must be refused before it can reach an action), and
    ;; the body guard outside the session stage (that stage parses the body
    ;; too).  The round-trip evidence for the same ordering is in
    ;; clails-test/middleware/csrf-http-roundtrip.
    (let ((guard (position *lack-middleware-clails-csrf-body-guard* *clails-middleware-stack*))
          (session (position *lack-middleware-clails-session* *clails-middleware-stack*))
          (csrf (position *lack-middleware-clails-csrf* *clails-middleware-stack*))
          (controller (position *lack-middleware-clails-controller* *clails-middleware-stack*)))
      (ok guard)
      (ok session)
      (ok csrf)
      (ok controller)
      (ok (< guard session))
      (ok (< session csrf))
      (ok (< csrf controller))))

  (testing "a task is unaffected: it never assembles a middleware stack"
    ;; The console takes the same path -- it starts the connection pool and
    ;; reads forms, and never builds a stack -- but it cannot be driven from a
    ;; test without a database, so the task runner stands in for both.
    (let ((clails/task/registry::*task-registry* (make-hash-table :test 'equal))
          (*enable-session-middleware* nil)
          (*enable-csrf-middleware* t)
          (*strict-configuration* t)
          (ran nil))
      (register-task :csrf-check-probe
                     :function (lambda () (setf ran t)))
      (with-no-loggers
        (run-task :csrf-check-probe))
      (ok ran "the task ran in a configuration that would abort a server start"))))
