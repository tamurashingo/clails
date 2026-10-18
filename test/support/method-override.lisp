(in-package #:cl-user)
(defpackage #:clails-test/support/method-override
  (:use #:cl)
  (:import-from #:clails-test/support/http
                #:ok-app
                #:make-memory-session-store)
  (:import-from #:clails-test/support/csrf
                #:with-csrf-stack
                #:+form-token-name+)
  (:import-from #:clails/middleware/method-override-middleware
                #:*enable-method-override*
                #:*method-override-parameter*
                #:*lack-middleware-clails-method-override*)
  (:export ;; building a stack that carries the override stage
           #:with-override-stack
           ;; spelling the request a form sends
           #:form-body
           #:query-uri
           ;; reading what a logger was told
           #:count-substring))
(in-package #:clails-test/support/method-override)


;;; Shared ground for the two M9 suites
;;; (clails-test/middleware/method-override, which looks at the stage from the
;;; inside, and .../method-override-roundtrip, which only looks at responses).
;;; Everything here is test scaffolding: assembling a stack with the real
;;; override stage in a chosen position, and spelling the body or query string
;;; a request carries.


;;; ------------------------------------------------------------------
;;; Building a stack with the override stage in it
;;; ------------------------------------------------------------------

(defmacro with-override-stack ((app-var &key (inner '#'ok-app)
                                             (session t)
                                             (csrf t)
                                             (strict nil)
                                             (override t)
                                             (parameter nil)
                                             (store nil)
                                             (state nil)
                                             (one-time nil)
                                             (message nil)
                                             (order :inside-csrf)
                                             (inner-middleware nil))
                               &body body)
  "Build a session -> CSRF -> override -> ... stack and bind APP-VAR to it for
   BODY.

   A thin layer over CLAILS-TEST/SUPPORT/CSRF:WITH-CSRF-STACK that adds the
   real framework override stage and binds its configuration around the stack
   build -- which is when the stage reads it, exactly like the session and
   CSRF stages.

   Keywords:
     :inner       the innermost application (default #'ok-app)
     :session     value bound to *enable-session-middleware*
     :csrf        value bound to *enable-csrf-middleware*
     :strict      value bound to clails/environment:*STRICT-CONFIGURATION*
                  (NIL by default, as in WITH-CSRF-STACK, so that a test
                  which does not care about the configuration check is never
                  stopped by it)
     :override    value bound to *enable-method-override*
     :parameter   value bound to *method-override-parameter*
     :store/:state  session store / cookie state (a fresh in-memory store by
                  default, so that one test's session cannot be another's)
     :one-time    value bound to *csrf-one-time-token*
     :message     value bound to *csrf-forbidden-message*
     :order       :INSIDE-CSRF (the required order, and the default) puts the
                  override stage inside the CSRF stage; :OUTSIDE-CSRF puts it
                  between the session stage and the CSRF stage, which is the
                  misconfiguration the marker check is about
     :inner-middleware  stages inside the override stage -- where the
                  controller (routing) stage goes

   Every configuration variable is rebound with LET, so a configuration
   chosen by one test cannot leak into the next one."
  (let ((order-var (gensym "ORDER")))
    `(let ((*enable-method-override* ,override)
           (*method-override-parameter* (or ,parameter *method-override-parameter*))
           (,order-var ,order))
       (with-csrf-stack (,app-var
                         :inner ,inner
                         :session ,session
                         :csrf ,csrf
                         :strict ,strict
                         :store (or ,store (make-memory-session-store))
                         :state ,state
                         :one-time ,one-time
                         :message ,message
                         :before-csrf (when (eq ,order-var :outside-csrf)
                                        (list *lack-middleware-clails-method-override*))
                         :after-csrf (append
                                      (when (eq ,order-var :inside-csrf)
                                        (list *lack-middleware-clails-method-override*))
                                      ,inner-middleware))
         ,@body))))


;;; ------------------------------------------------------------------
;;; Spelling the request a form sends
;;; ------------------------------------------------------------------

(defun form-body (&key method token (extra '(("title" . "x")))
                       (parameter "_method")
                       (token-name +form-token-name+))
  "The urlencoded body of a form, as an alist for CALL-APP's :content.

   METHOD of NIL leaves the override field out entirely, and TOKEN of NIL
   leaves the CSRF token out -- which is how the two \"not carried at all\"
   cases of the matrix are spelled."
  (append extra
          (when method (list (cons parameter method)))
          (when token (list (cons token-name token)))))

(defun query-uri (path method &key (parameter "_method"))
  "PATH with the override field appended to its QUERY STRING.

   This is the half of the request the stage must never read: a value here
   must not rewrite anything, whatever method the request itself uses."
  (format nil "~A?~A=~A" path parameter method))


;;; ------------------------------------------------------------------
;;; Reading what a logger was told
;;; ------------------------------------------------------------------

(defun count-substring (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK, without overlaps.

   Used to count the records a logger received: \"once per process\" is an
   assertion about a count, not about presence."
  (loop with start = 0
        with count = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))
