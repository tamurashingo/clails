(in-package #:cl-user)
(defpackage #:clails-test/middleware/method-override
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf
        #:clails-test/support/method-override)
  (:import-from #:clails/environment
                #:*strict-configuration*)
  (:import-from #:clails/middleware/method-override-middleware
                #:*enable-method-override*
                #:*method-override-parameter*
                #:*method-override-forbidden-message*
                #:+method-override-methods+
                #:+method-override-default-parameter+
                #:method-override-value
                #:method-override-warning-issued-p
                #:reset-method-override-warning
                #:*lack-middleware-clails-method-override*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:+csrf-marker-key+
                #:*lack-middleware-clails-csrf*
                #:*lack-middleware-clails-csrf-body-guard*)
  (:import-from #:clails/middleware/session-middleware
                #:*lack-middleware-clails-session*)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/transaction-middleware
                #:*lack-middleware-transaction*)
  (:import-from #:clails/middleware/core
                #:*clails-middleware-stack*)
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+))
(in-package #:clails-test/middleware/method-override)


;;; The request-method override stage (M9) seen from the inside: its
;;; configuration, the values it accepts, its place in the framework's own
;;; stack and the configuration check it makes on every request (the "confirm
;;; the marker" half of decision 14, whose other half is the CSRF stage's
;;; marker).  What the stage does to real requests -- the method x where the
;;; forged value sits x token matrix, and whether a form reaches a resource's
;;; update and destroy routes -- lives in
;;; clails-test/middleware/method-override-roundtrip.


;;; ------------------------------------------------------------------
;;; Configuration
;;; ------------------------------------------------------------------

(deftest method-override-configuration-defaults-test
  (testing "the stage is enabled by default"
    ;; Unlike the session and CSRF stages, this one is on by default: it acts
    ;; only on a request that asks for it (a POST carrying the field), and
    ;; what it then does is the documented purpose of that field.  A project
    ;; that does not use the convention has nothing to opt into.
    (ok (eq *enable-method-override* t)))

  (testing "the field read is the _method convention"
    (ok (equal *method-override-parameter* "_method"))
    (ok (equal *method-override-parameter* +method-override-default-parameter+)))

  (testing "the allow-list is exactly the three state-changing methods a form cannot send"
    (ok (equal +method-override-methods+ '(:put :patch :delete)))
    (ok (every #'keywordp +method-override-methods+)))

  (testing "no safe method is in the allow-list"
    ;; The safety property: a request can never forge its way into a method
    ;; that is exempt from CSRF verification, which is what the measured
    ;; "_method=GET and no token" bypass relied on.
    (dolist (method '(:get :head :options :trace :connect))
      (ok (not (member method +method-override-methods+))
          (format nil "~A cannot be forged" method))))

  (testing "POST itself is not in the allow-list either"
    ;; Nothing would break if it were, but the list says \"methods a form
    ;; cannot send\", and a form can send POST.
    (ok (not (member :post +method-override-methods+))))

  (testing "the rejection body is one plain line naming the mistake"
    (ok (stringp *method-override-forbidden-message*))
    (ok (null (find #\Newline *method-override-forbidden-message*)))
    (ok (search "CSRF" *method-override-forbidden-message*))))


(defmacro with-restored-value ((place) &body body)
  "Run BODY and put back whatever PLACE held, whatever BODY did to it.

   The declaration tests below have to assign to the global value rather than
   rebind it: rebinding is exactly what they are NOT testing."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original ,place))
       (unwind-protect (progn ,@body)
         (setf ,place ,original)))))

(deftest method-override-configuration-declaration-test
  ;; B5 / R28: the settings are declared with DEFVAR, not DEFPARAMETER, so
  ;; that a value an application set in app/config/environment.lisp survives
  ;; the declaration being evaluated again -- which happens whenever the
  ;; system is reloaded into a running image.
  (testing "re-evaluating the declaration does not reset *enable-method-override*"
    (with-restored-value (*enable-method-override*)
      (setf *enable-method-override* nil)
      (eval '(defvar clails/middleware/method-override-middleware:*enable-method-override* t))
      (ok (null *enable-method-override*))))

  (testing "re-evaluating the declaration does not reset *method-override-parameter*"
    (with-restored-value (*method-override-parameter*)
      (setf *method-override-parameter* "_verb")
      (eval '(defvar clails/middleware/method-override-middleware:*method-override-parameter* "_method"))
      (ok (equal *method-override-parameter* "_verb"))))

  (testing "re-evaluating the declaration does not reset *method-override-forbidden-message*"
    (with-restored-value (*method-override-forbidden-message*)
      (setf *method-override-forbidden-message* "nope")
      (eval '(defvar clails/middleware/method-override-middleware:*method-override-forbidden-message* "other"))
      (ok (equal *method-override-forbidden-message* "nope")))))


;;; ------------------------------------------------------------------
;;; The values the stage accepts
;;; ------------------------------------------------------------------

(deftest method-override-value-test
  (testing "every allowed method is recognised"
    (ok (eq (method-override-value "put") :put))
    (ok (eq (method-override-value "patch") :patch))
    (ok (eq (method-override-value "delete") :delete)))

  (testing "the comparison ignores case"
    ;; The legacy dispatch branch has always compared with STRING-EQUAL, so a
    ;; form written against the old behaviour keeps working.
    (dolist (spelling '("PUT" "Put" "pUt"))
      (ok (eq (method-override-value spelling) :put)
          (format nil "~S names PUT" spelling)))
    (ok (eq (method-override-value "DELETE") :delete))
    (ok (eq (method-override-value "Patch") :patch)))

  (testing "a safe method is not accepted, however it is spelled"
    (dolist (spelling '("get" "GET" "Get" "head" "options"))
      (ok (null (method-override-value spelling))
          (format nil "~S is refused" spelling))))

  (testing "an unknown value, an empty one or no value at all is simply ignored"
    ;; Condition 4 of the four: nothing signals, nothing is rejected, the
    ;; request keeps its real method.
    (dolist (value (list "" " " "putt" "pu" "destroy" "DELETE " "0" nil :delete 7
                         (list "delete")))
      (ok (null (method-override-value value))
          (format nil "~S does not rewrite anything" value)))))


(deftest method-override-allow-list-agreement-test
  ;; The allow-list may not drift from the set of methods the CSRF stage
  ;; verifies: a forgeable method that is not verified is a bypass, which is
  ;; the whole reason the list exists.
  (testing "every forgeable method is one the CSRF stage verifies"
    (dolist (method +method-override-methods+)
      (ok (member method clails/middleware/csrf-middleware::+unsafe-methods+)
          (format nil "~A is verified before it can be forged" method))))

  (testing "the only verified method missing from the allow-list is POST"
    ;; POST is the method doing the forging, so it is not in the list; every
    ;; other state-changing method is, which is what M9-T2 settled (PATCH was
    ;; verified but reachable from nowhere).
    (ok (equal (set-difference clails/middleware/csrf-middleware::+unsafe-methods+
                               +method-override-methods+)
               '(:post)))))


;;; ------------------------------------------------------------------
;;; Position in the framework's own stack
;;; ------------------------------------------------------------------

(deftest method-override-stack-position-test
  (testing "the framework stack puts the override stage inside CSRF and outside routing"
    ;; The order is a safety requirement, not a preference: outside the CSRF
    ;; stage the forgery would be honoured before the token is checked, and
    ;; outside the routing stage is where it has to be for the rewritten
    ;; method to decide the route.  The round-trip evidence for the same
    ;; ordering is in clails-test/middleware/method-override-roundtrip.
    (let ((session (position *lack-middleware-clails-session* *clails-middleware-stack*))
          (csrf (position *lack-middleware-clails-csrf* *clails-middleware-stack*))
          (override (position *lack-middleware-clails-method-override*
                              *clails-middleware-stack*))
          (controller (position *lack-middleware-clails-controller* *clails-middleware-stack*)))
      (ok session)
      (ok csrf)
      (ok override)
      (ok controller)
      (ok (< session csrf))
      (ok (< csrf override))
      (ok (< override controller))))

  (testing "the stage is in the list once"
    (ok (= (count *lack-middleware-clails-method-override* *clails-middleware-stack*) 1)))

  (testing "the default stack still assembles"
    (with-no-loggers
      (ok (functionp (compose-middleware *clails-middleware-stack* #'not-found-app))))))


;;; ------------------------------------------------------------------
;;; The marker check (decision 14, checked configuration 2)
;;; ------------------------------------------------------------------
;;;
;;; A stack with the override stage OUTSIDE the CSRF stage does not fail: every
;;; request is answered, and the only difference is that a forgery is honoured
;;; before the token is verified.  There is no "let it fail at run time"
;;; option, so the stage asks the CSRF stage whether it ran: the CSRF stage
;;; marks every request it lets through, and a missing mark (with CSRF
;;; enabled) means the order is wrong.

(defun misordered-post (&key (strict nil) (csrf t))
  "Round-trip a POST through a stack whose override stage is OUTSIDE the CSRF
   stage, and report (values STATUS METHOD-SEEN).

   The request carries the CSRF token when the CSRF stage is enabled, so
   nothing but the stage order can refuse it.  With the stage disabled there
   is no token to carry -- and no application that could ask for one, since
   CSRF-TOKEN reads what only an enabled stage binds."
  (let ((seen :unset))
    (flet ((observe (env) (setf seen (getf env :request-method))))
      (with-override-stack (app :csrf csrf
                                :strict strict
                                :order :outside-csrf
                                :inner (if csrf
                                           (make-token-app :observer #'observe)
                                           (make-recording-app #'observe)))
        (let* ((client (open-session app))
               (token (when csrf
                        (response-body-string (client-request client "/")))))
          (setf seen :unset)
          (let ((response (client-request client "/"
                                          :method :post
                                          :content (form-body :method "delete"
                                                              :token token))))
            (values (response-status response) seen)))))))

(deftest method-override-marker-check-test
  (testing "strict: a misordered stack has its requests rejected, and nothing is rewritten"
    (with-no-loggers
      (multiple-value-bind (status seen) (misordered-post :strict t)
        (ok (eql status +forbidden-status+))
        (ok (eq seen :unset) "the application below never ran"))))

  (testing "strict: the rejection is recorded on the audit logger, like every other 403"
    (reset-method-override-warning)
    (let ((audit (with-captured-logger (:audit)
                   (misordered-post :strict t))))
      (ok (search "method override stage is outside the CSRF stage" audit))
      (ok (search "status=403" audit))
      (ok (search "stage=:METHOD-OVERRIDE" audit))))

  (testing "strict: even a safe request is refused -- the configuration is wrong for all of them"
    ;; A GET that was answered normally would hide the mistake until the first
    ;; form was submitted.
    (with-no-loggers
      (with-override-stack (app :strict t :order :outside-csrf)
        (ok (eql (response-status (call-app app "/")) +forbidden-status+)))))

  (testing "lenient: the request is served, the forgery is not honoured"
    ;; The decided non-strict behaviour, pinned here: warn and carry on means
    ;; the request is served (it is a development environment), but the one
    ;; thing the broken order makes unsafe -- honouring the forgery -- is not
    ;; done.  A form that needs the override therefore fails loudly instead of
    ;; silently taking the unsafe path.
    (reset-method-override-warning)
    (with-no-loggers
      (multiple-value-bind (status seen) (misordered-post :strict nil)
        (ok (eql status 200))
        (ok (eq seen :post) "the method reached the application unrewritten"))))

  (testing "lenient: the warning reaches the audit logger"
    (reset-method-override-warning)
    (let ((audit (with-captured-logger (:audit)
                   (misordered-post :strict nil))))
      (ok (search "WARN" audit) "the warning was recorded")
      (ok (search "running OUTSIDE the CSRF verification stage" audit))
      (ok (search "stage=:METHOD-OVERRIDE" audit))
      (ok (search "check=:CSRF-MARKER" audit))))

  (testing "lenient: the warning goes to the audit logger, not to the package logger"
    ;; A6: the generated configuration sets the framework's package logger to
    ;; :error in development and test, so a warning recorded there would never
    ;; be seen.  Here the package logger's name is the only registered logger,
    ;; so anything printed would have gone to the wrong place.
    (reset-method-override-warning)
    (let ((package-log (with-captured-logger (:clails/middleware/method-override-middleware)
                         (misordered-post :strict nil))))
      (ok (equal package-log ""))))

  (testing "lenient: the warning is written once per process, not once per request"
    (reset-method-override-warning)
    (ok (null (method-override-warning-issued-p)))
    (let ((audit (with-captured-logger (:audit)
                   (dotimes (i 5)
                     (misordered-post :strict nil)))))
      (ok (= (count-substring "running OUTSIDE the CSRF verification stage" audit) 1)
          "five round trips, one record"))
    (ok (method-override-warning-issued-p))
    ;; ... and a stack built afterwards does not start the count again
    (let ((audit (with-captured-logger (:audit)
                   (misordered-post :strict nil))))
      (ok (equal audit "")))))


(deftest method-override-marker-check-scope-test
  (testing "CSRF disabled: no marker is required and the check never fires"
    ;; A project that has not opted into CSRF must see neither a warning nor a
    ;; rejection, whatever strict says -- there is no marker to expect.
    (dolist (strict '(t nil))
      (reset-method-override-warning)
      (with-no-loggers
        (multiple-value-bind (status seen) (misordered-post :strict strict :csrf nil)
          (ok (eql status 200)
              (format nil "served with strict ~A" strict))
          (ok (eq seen :delete)
              "and the override still works, because nothing is unsafe about it here")))
      (let ((audit (with-captured-logger (:audit)
                     (misordered-post :strict strict :csrf nil))))
        (ok (equal audit "") (format nil "nothing recorded with strict ~A" strict)))))

  (testing "the required order never fires the check, strict or not"
    (dolist (strict '(t nil))
      (reset-method-override-warning)
      (with-no-loggers
        (let ((seen :unset))
          (with-override-stack (app :strict strict
                                    :order :inside-csrf
                                    :inner (make-token-app
                                            :observer (lambda (env)
                                                        (setf seen (getf env :request-method)))))
            (let* ((client (open-session app))
                   (token (fetch-token client))
                   (response (client-request client "/"
                                             :method :post
                                             :content (form-body :method "delete"
                                                                 :token token))))
              (ok (eql (response-status response) 200)
                  (format nil "served with strict ~A" strict))
              (ok (eq seen :delete) "and the forgery was honoured")))))
      (ok (null (method-override-warning-issued-p))
          (format nil "nothing to warn about with strict ~A" strict))))

  (testing "the stage disabled: the check is not made either"
    ;; A pass-through stage reads nothing and judges nothing, so a project
    ;; that switched the override off is never told about the stack order.
    (dolist (strict '(t nil))
      (reset-method-override-warning)
      (with-no-loggers
        (with-override-stack (app :override nil :strict strict :order :outside-csrf)
          (ok (eql (response-status (call-app app "/")) 200))))
      (ok (null (method-override-warning-issued-p)))))

  (testing "the marker the check looks for is the one the CSRF stage places"
    ;; The two halves of the mechanism, named in one place: M6 places the
    ;; mark, M9 reads it.
    (let ((marker :unset))
      (with-no-loggers
        (with-override-stack (app :inner (make-token-app
                                          :observer (lambda (env)
                                                      (setf marker (getf env +csrf-marker-key+)))))
          (fetch-token (open-session app))))
      (ok (eq marker t)))))
