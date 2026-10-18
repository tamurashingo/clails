(in-package #:cl-user)
(defpackage #:clails-test/support/csrf
  (:use #:cl)
  (:import-from #:clails-test/support/http
                #:ok-app
                #:call-app
                #:client-request
                #:make-session-client
                #:response-body-string
                #:with-middleware-stack)
  (:import-from #:clails/environment
                #:*strict-configuration*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*enable-csrf-middleware*
                #:*csrf-session-key*
                #:*csrf-form-token*
                #:*csrf-header-name*
                #:*csrf-one-time-token*
                #:*csrf-forbidden-message*
                #:*lack-middleware-clails-csrf-body-guard*
                #:*lack-middleware-clails-csrf*
                #:csrf-token
                #:csrf-html-tag)
  (:import-from #:clails/logger
                #:*logger-registry*
                #:register-logger
                #:make-console-appender
                #:<text-formatter>)
  (:export ;; building a stack that carries the CSRF stage
           #:with-csrf-stack
           ;; applications to put at the bottom of such a stack
           #:make-token-app
           #:make-token-tag-app
           #:make-echo-token-app
           ;; obtaining a token the way a browser does
           #:open-session
           #:fetch-token
           ;; request bodies in the formats the stage has to cope with
           #:+form-token-name+
           #:form-content
           #:json-content
           #:json-headers
           #:multipart-content
           #:multipart-headers
           #:token-header
           ;; capturing what the loggers were told
           #:with-no-loggers
           #:with-captured-logger))
(in-package #:clails-test/support/csrf)


;;; Shared ground for the three CSRF suites
;;; (clails-test/middleware/csrf-middleware, .../csrf-http-roundtrip and
;;; .../csrf-one-time).  Everything here is test scaffolding: building a stack
;;; that carries the real CSRF stage, getting a token out of it the way a
;;; browser would, and spelling a request body in each of the formats the
;;; stage has to cope with.


;;; ------------------------------------------------------------------
;;; Building a stack with the CSRF stage in it
;;; ------------------------------------------------------------------

(defmacro with-csrf-stack ((app-var &key (inner '#'ok-app)
                                         (session t)
                                         (csrf t)
                                         (store nil)
                                         (state nil)
                                         (strict nil)
                                         (one-time nil)
                                         (session-key nil)
                                         (form-token nil)
                                         (header-name nil)
                                         (message nil)
                                         (body-guard t)
                                         (outer-middleware nil)
                                         (before-csrf nil)
                                         (after-csrf nil))
                           &body body)
  "Build a session -> CSRF -> ... stack and bind APP-VAR to it for BODY.

   A thin layer over CLAILS-TEST/SUPPORT/HTTP:WITH-MIDDLEWARE-STACK that puts
   the real framework CSRF stage inside the session stage and binds the
   framework's CSRF configuration around the stack build -- which is when the
   stage reads it, exactly like the session stage.

   Keywords:
     :inner        the innermost application (default #'ok-app)
     :session      value bound to *enable-session-middleware*
     :csrf         value bound to *enable-csrf-middleware*
     :store/:state session store / cookie state
     :strict       value bound to clails/environment:*STRICT-CONFIGURATION*
                   (NIL here, not the framework default, so that a test which
                   does not care about the configuration check is never
                   stopped by it)
     :one-time     value bound to *csrf-one-time-token*
     :session-key / :form-token / :header-name / :message
                   override the corresponding configuration variable
     :body-guard   whether the CSRF body guard is placed outside the session
                   stage, as the framework's own stack does (default T).
                   Pass NIL to assemble a stack without it -- which is what
                   shows why it is needed: the session stage parses the body
                   itself, so an unparseable body signals there
     :outer-middleware  stages outside the body guard
     :before-csrf       stages between the session stage and the CSRF stage
     :after-csrf        stages inside the CSRF stage -- where M9's rewriting
                        stage and the routing stage go"
  `(let ((*enable-csrf-middleware* ,csrf)
         (*strict-configuration* ,strict)
         (*csrf-one-time-token* ,one-time)
         (*csrf-session-key* (or ,session-key *csrf-session-key*))
         (*csrf-form-token* (or ,form-token *csrf-form-token*))
         (*csrf-header-name* (or ,header-name *csrf-header-name*))
         (*csrf-forbidden-message* (or ,message *csrf-forbidden-message*)))
     (with-middleware-stack (,app-var :inner ,inner
                                      :session ,session
                                      :store ,store
                                      :state ,state
                                      :outer-middleware (append ,outer-middleware
                                                                (when ,body-guard
                                                                  (list *lack-middleware-clails-csrf-body-guard*)))
                                      :inner-middleware (append ,before-csrf
                                                                (list *lack-middleware-clails-csrf*)
                                                                ,after-csrf))
       ,@body)))


;;; ------------------------------------------------------------------
;;; Applications that know about the token
;;; ------------------------------------------------------------------

(defun make-token-app (&key observer (status 200))
  "An application that answers the session's CSRF token as its body.

   This is \"the page with the form on it\": asking for the token is what
   mints it and puts it in the session.  OBSERVER, when given, is called with
   the request environment first, so a test can also look at what the
   innermost application saw.

   CSRF-TOKEN may only be called while the CSRF stage is on the stack -- it
   reads the dynamic variables the stage binds per request -- so this
   application must not be used below a disabled (pass-through) stage."
  (lambda (env)
    (when observer
      (funcall observer env))
    (list status
          (list :content-type "text/plain")
          (list (csrf-token (getf env :lack.session))))))

(defun make-token-tag-app (&key (calls 2))
  "An application that answers CALLS rendered hidden-input tags, one per line.

   The form helper M7 will add renders exactly such a tag; what matters here
   is that calling it several times within one request returns one and the
   same token, which is what lets a page carry several forms."
  (lambda (env)
    (let ((session (getf env :lack.session)))
      (list 200
            (list :content-type "text/plain")
            (list (format nil "~{~A~^~%~}"
                          (loop repeat calls
                                collect (csrf-html-tag session))))))))

(defun make-echo-token-app (&key observer)
  "An application that answers whatever the session holds under the CSRF
   session key, or the empty string when it holds nothing.

   Unlike MAKE-TOKEN-APP this one does not mint a token, so it can report
   \"there is no token any more\" -- which is what a one-time token looks like
   from inside the action that consumed it."
  (lambda (env)
    (when observer
      (funcall observer env))
    (let ((stored (gethash *csrf-session-key* (getf env :lack.session))))
      (list 200
            (list :content-type "text/plain")
            (list (or stored ""))))))


;;; ------------------------------------------------------------------
;;; Getting a token the way a browser does
;;; ------------------------------------------------------------------

(defun open-session (app)
  "A session client against APP, with no cookies yet."
  (make-session-client app))

(defun fetch-token (client &key (uri "/"))
  "Ask CLIENT to GET URI and return the token the response carries.

   The application below the stage must be a MAKE-TOKEN-APP.  The client
   remembers the session cookie, so every later request through it belongs to
   the session this token was minted in."
  (response-body-string (client-request client uri)))


;;; ------------------------------------------------------------------
;;; Request bodies
;;; ------------------------------------------------------------------

(defparameter +form-token-name+ "_csrf_token"
  "The default name of the body parameter carrying the token, spelled out so
   that a test fails if the default ever changes silently.")

(defun form-content (token &key (name +form-token-name+) (extra '(("title" . "x"))))
  "A urlencoded body carrying TOKEN, as an alist for CALL-APP's :content.

   TOKEN of NIL means \"no token at all\", which is how the missing-token case
   is spelled."
  (if token
      (append extra (list (cons name token)))
      extra))

(defun %json-escape (string)
  (with-output-to-string (out)
    (loop for character across string
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char character out))))))

(defun json-content (token &key (name +form-token-name+) (extra '(("title" . "x"))))
  "A JSON object body carrying TOKEN at the TOP level, as a string.

   The top level is where the stage looks: a token nested inside the object is
   deliberately not read (see *CSRF-HEADER-NAME*'s note on what is out of
   scope).  TOKEN of NIL leaves the token out entirely."
  (let ((pairs (append (mapcar (lambda (entry)
                                 (format nil "\"~A\":\"~A\""
                                         (%json-escape (car entry))
                                         (%json-escape (cdr entry))))
                               extra)
                       (when token
                         (list (format nil "\"~A\":\"~A\""
                                       (%json-escape name)
                                       (%json-escape token)))))))
    (format nil "{~{~A~^,~}}" pairs)))

(defun json-headers ()
  "Headers declaring a JSON body."
  (list (cons "content-type" "application/json")))

(defparameter +multipart-boundary+ "clailsTestBoundary42")

(defun multipart-content (token &key (name +form-token-name+)
                                     (extra '(("title" . "x")))
                                     (file t)
                                     (boundary +multipart-boundary+))
  "A multipart/form-data body carrying TOKEN as a TOP-LEVEL field, as a string.

   With :FILE true (the default) the body also carries a file part, because
   that is the case worth proving: a request that uploads a file still has its
   token read correctly, even though a multipart parameter's value arrives as
   a list rather than a string."
  (with-output-to-string (out)
    (flet ((field (field-name value)
             (format out "--~A~C~C" boundary #\Return #\Newline)
             (format out "Content-Disposition: form-data; name=\"~A\"~C~C~C~C"
                     field-name #\Return #\Newline #\Return #\Newline)
             (format out "~A~C~C" value #\Return #\Newline)))
      (loop for (key . value) in extra do (field key value))
      (when token
        (field name token))
      (when file
        (format out "--~A~C~C" boundary #\Return #\Newline)
        (format out "Content-Disposition: form-data; name=\"attachment\"; filename=\"note.txt\"~C~C"
                #\Return #\Newline)
        (format out "Content-Type: text/plain~C~C~C~C" #\Return #\Newline #\Return #\Newline)
        (format out "file contents~C~C" #\Return #\Newline))
      (format out "--~A--~C~C" boundary #\Return #\Newline))))

(defun multipart-headers (&key (boundary +multipart-boundary+))
  "Headers declaring a multipart body with BOUNDARY."
  (list (cons "content-type"
              (format nil "multipart/form-data; boundary=~A" boundary))))

(defun token-header (token &key (name "X-CSRF-Token"))
  "Headers carrying TOKEN in the CSRF request header."
  (list (cons name token)))


;;; ------------------------------------------------------------------
;;; Capturing what the loggers were told
;;; ------------------------------------------------------------------
;;;
;;; Same mechanism as clails-test/middleware/forbidden-response: the registry
;;; is rebound rather than cleared, so the suite's logger configuration
;;; survives and no audit record prints into the middle of the test report.

(defmacro with-no-loggers (&body body)
  "Run BODY with an empty logger registry."
  `(let ((*logger-registry* (make-hash-table :test 'equal)))
     ,@body))

(defun call-with-captured-logger (logger-name thunk)
  "Call THUNK with LOGGER-NAME as the only registered logger and return
   everything that logger printed.

   Assertions must be made on the returned string AFTER this call, never
   inside THUNK: rove's report stream is a synonym stream for
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
