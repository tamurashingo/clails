(in-package #:cl-user)
(defpackage #:clails-test/support/http
  (:use #:cl)
  ;; NOTE: lack-test is an ordinary (non-package-inferred) ASDF system that
  ;; provides exactly one package, LACK/TEST, and lack.asd registers a
  ;; "lack/test" system alias for it.  A single :import-from #:lack/test is
  ;; therefore resolvable by clails-test's package-inferred-system.
  ;;
  ;; The same is NOT true for the lack session sub-packages
  ;; (lack/middleware/session/store, .../store/memory, .../state/cookie):
  ;; no ASDF system is named after them, so adding :import-from clauses for
  ;; those packages would make the package-inferred-system try (and fail) to
  ;; find matching components -- exactly the trap documented in
  ;; src/middleware/session-middleware.lisp (L4-L12).  They are referenced
  ;; with fully-qualified symbols below instead; they are already loaded as a
  ;; side effect of depending on clails/middleware/session-middleware.
  (:import-from #:lack/test
                #:generate-env
                #:testing-app)
  (:import-from #:cl-cookie
                #:make-cookie-jar)
  (:import-from #:clails/middleware/session-middleware
                #:*enable-session-middleware*
                #:*session-store*
                #:*session-state*
                #:*lack-middleware-clails-session*)
  (:export ;; trivial applications to put at the bottom of a stack
           #:ok-app
           #:not-found-app
           #:make-recording-app
           #:make-session-counter-app
           ;; building a middleware stack inside a test
           #:compose-middleware
           #:with-middleware-stack
           ;; sending a single request
           #:call-app
           #:with-http-client
           ;; reading a response
           #:response-status
           #:response-headers
           #:response-header
           #:response-body-string
           ;; Set-Cookie inspection
           #:set-cookie
           #:set-cookie-p
           #:set-cookie-name
           #:set-cookie-value
           #:set-cookie-attributes
           #:parse-set-cookie
           #:response-set-cookie
           #:set-cookie-attribute
           #:set-cookie-attribute-present-p
           #:response-cookie-pair
           #:response-session-id
           ;; carrying cookies across requests
           #:session-client
           #:make-session-client
           #:session-client-cookie
           #:session-client-last-set-cookie
           #:session-client-last-response
           #:client-request
           ;; session store access
           #:make-memory-session-store
           #:make-test-cookie-state
           #:fetch-stored-session
           ;; concurrency
           #:run-concurrently
           #:run-concurrent-session-requests
           #:hash-table-sane-p))
(in-package #:clails-test/support/http)


;;; ------------------------------------------------------------------
;;; Trivial applications
;;; ------------------------------------------------------------------

(defun ok-app (env)
  "A minimal Lack application: always answers 200 / text/plain / \"ok\"."
  (declare (ignore env))
  (list 200 (list :content-type "text/plain") (list "ok")))

(defun not-found-app (env)
  "A minimal Lack application answering 404.

   Used as the innermost application when the clails controller middleware is
   part of the stack: that middleware delegates to the next application when
   no route matches."
  (declare (ignore env))
  (list 404 (list :content-type "text/plain") (list "not found")))

(defun make-recording-app (observer &key (status 200)
                                         (headers (list :content-type "text/plain"))
                                         (body "ok"))
  "Return an application that calls OBSERVER with the request env and then
   answers a fixed response.  OBSERVER is where a test records what the
   innermost application actually saw (session table, request method, body
   parameters, ...)."
  (lambda (env)
    (funcall observer env)
    (list status (copy-list headers) (list body))))

(defun make-session-counter-app (&key (key "counter"))
  "Return an application that increments KEY in the request session.

   The read-modify-write is deliberately *not* atomic: this is the shape of a
   normal controller action, and it is what makes the lost-update defect
   measurable from a test (see run-concurrent-session-requests)."
  (lambda (env)
    (let* ((session (getf env :lack.session))
           (value (1+ (or (gethash key session) 0))))
      (setf (gethash key session) value)
      (list 200 (list :content-type "text/plain") (list (princ-to-string value))))))


;;; ------------------------------------------------------------------
;;; Building a middleware stack inside a test
;;; ------------------------------------------------------------------

(defun compose-middleware (middleware inner-app)
  "Wrap INNER-APP with the MIDDLEWARE list, outermost first.

   Same composition order as clails/middleware/core:*clails-middleware-stack*,
   i.e. the first element of MIDDLEWARE sees the request first."
  (reduce (lambda (mw app) (funcall mw app))
          middleware
          :from-end t
          :initial-value inner-app))

(defmacro with-middleware-stack ((app-var &key (inner '#'ok-app)
                                               (session t)
                                               (store nil)
                                               (state nil)
                                               (outer-middleware nil)
                                               (inner-middleware nil))
                                 &body body)
  "Build a clails middleware stack for the dynamic extent of BODY and bind
   APP-VAR to the resulting Lack application.

   The clails session stage resolves *session-store* / *session-state* exactly
   once -- when the stack is built, not per request (see
   src/middleware/session-middleware.lisp L78-L81).  A stack built at load
   time can therefore never be reconfigured, which is why every test has to
   build its own stack.  This macro is that shared mechanism.

   Keywords:
     :inner             the innermost application (default #'ok-app)
     :session           value bound to *enable-session-middleware*
     :store             value bound to *session-store*
     :state             value bound to *session-state*
     :outer-middleware  list of middleware placed OUTSIDE the session stage
     :inner-middleware  list of middleware placed INSIDE the session stage,
                        outermost first -- this is where a CSRF stage, a
                        request-method rewriting stage or the clails
                        controller stage go.

   All three configuration variables are rebound with LET, so a configuration
   chosen by one test cannot leak into the next one."
  `(let ((*enable-session-middleware* ,session)
         (*session-store* ,store)
         (*session-state* ,state))
     (let ((,app-var (compose-middleware
                      (append ,outer-middleware
                              (list *lack-middleware-clails-session*)
                              ,inner-middleware)
                      ,inner)))
       ,@body)))


;;; ------------------------------------------------------------------
;;; Sending a single request
;;; ------------------------------------------------------------------

(defun call-app (app uri &key (method :get) content headers cookie)
  "Send one request to APP without starting a server or touching a database.

   URI may carry a query string (\"/todos?_method=delete\").  CONTENT is the
   request body, in lack-test's notation: an alist is form-encoded, a string
   or octet vector is sent as is.  COOKIE is a raw Cookie header value, as
   produced by RESPONSE-COOKIE-PAIR.

   Returns (values RESPONSE ENV), where RESPONSE is the raw Lack response
   list (status headers body) -- raw, so that header presence and absence can
   both be asserted on."
  (let* ((headers (if cookie
                      (append headers (list (cons "cookie" cookie)))
                      headers))
         (env (generate-env uri
                            :method method
                            :content content
                            :headers headers)))
    (values (funcall app env) env)))

(defmacro with-http-client ((request-fn app) &body body)
  "Bind REQUEST-FN, inside BODY, to lack-test's REQUEST against APP with a
   shared cl-cookie jar, so that cookies are carried over automatically.

     (with-http-client (req app)
       (multiple-value-bind (body status headers) (req \"/\")
         ...))

   REQUEST-FN accepts lack-test's keywords (:method :content :headers) and
   returns (values body status headers-hash).  Use CALL-APP instead when the
   test needs the raw response headers."
  (let ((jar (gensym "COOKIE-JAR")))
    `(let ((,jar (make-cookie-jar)))
       (testing-app ,app
         (flet ((,request-fn (uri &rest args)
                  (apply #'lack/test:request uri :cookie-jar ,jar args)))
           ,@body)))))


;;; ------------------------------------------------------------------
;;; Reading a response
;;; ------------------------------------------------------------------

(defun response-status (response)
  (first response))

(defun response-headers (response)
  (second response))

(defun response-header (response name)
  "Return the RESPONSE header NAME (a keyword such as :set-cookie), or NIL
   when the header was not emitted at all."
  (getf (response-headers response) name))

(defun response-body-string (response)
  "Return the response body as one string."
  (let ((body (third response)))
    (etypecase body
      (null "")
      (string body)
      (cons (apply #'concatenate 'string (remove nil body))))))


;;; ------------------------------------------------------------------
;;; Set-Cookie inspection
;;; ------------------------------------------------------------------

(defstruct (set-cookie (:constructor %make-set-cookie))
  "A parsed Set-Cookie header value.

   ATTRIBUTES is an alist of (lower-cased-name . value); a valueless
   attribute such as HttpOnly or secure is recorded with the value T.  An
   attribute that was not emitted is simply absent from the alist, which is
   what lets a test assert the *absence* of an attribute."
  (name nil :type (or null string))
  (value nil :type (or null string))
  (attributes nil :type list))

(defun %trim (string)
  (string-trim '(#\Space #\Tab) string))

(defun %split-attributes (header)
  (loop with start = 0
        for pos = (position #\; header :start start)
        collect (%trim (subseq header start (or pos (length header))))
        while pos
        do (setf start (1+ pos))))

(defun parse-set-cookie (header)
  "Parse a Set-Cookie header value into a SET-COOKIE structure, or NIL when
   HEADER is NIL."
  (when header
    (let* ((parts (%split-attributes header))
           (pair (first parts))
           (eq-pos (position #\= pair)))
      (%make-set-cookie
       :name (if eq-pos (subseq pair 0 eq-pos) pair)
       :value (when eq-pos (subseq pair (1+ eq-pos)))
       :attributes (loop for part in (rest parts)
                         unless (string= part "")
                         collect (let ((pos (position #\= part)))
                                   (if pos
                                       (cons (string-downcase (%trim (subseq part 0 pos)))
                                             (%trim (subseq part (1+ pos))))
                                       (cons (string-downcase part) t))))))))

(defun response-set-cookie (response)
  "Parse the Set-Cookie header of RESPONSE, or NIL when the response carries
   none."
  (parse-set-cookie (response-header response :set-cookie)))

(defun set-cookie-attribute (set-cookie name)
  "Return (values VALUE PRESENT-P) for the attribute NAME of SET-COOKIE.

   PRESENT-P distinguishes \"attribute absent\" from \"attribute present
   without a value\" (the latter yields (values T T))."
  (let ((entry (assoc (string-downcase name)
                      (set-cookie-attributes set-cookie)
                      :test #'string=)))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun set-cookie-attribute-present-p (set-cookie name)
  "True when SET-COOKIE carries the attribute NAME at all."
  (nth-value 1 (set-cookie-attribute set-cookie name)))

(defun response-cookie-pair (response)
  "Return \"name=value\" for the Set-Cookie of RESPONSE -- ready to be handed
   back as a Cookie header on the next request -- or NIL."
  (let ((parsed (response-set-cookie response)))
    (when parsed
      (format nil "~A=~A" (set-cookie-name parsed) (set-cookie-value parsed)))))

(defun response-session-id (response)
  "Return the session id carried by the Set-Cookie of RESPONSE, or NIL."
  (let ((parsed (response-set-cookie response)))
    (when parsed
      (set-cookie-value parsed))))


;;; ------------------------------------------------------------------
;;; Carrying cookies across requests
;;; ------------------------------------------------------------------

(defstruct (session-client (:constructor %make-session-client))
  "A tiny browser: remembers the last Set-Cookie and sends it back."
  app
  (cookie nil :type (or null string))
  (last-set-cookie nil)
  (last-response nil))

(defun make-session-client (app)
  (%make-session-client :app app))

(defun client-request (client uri &key (method :get) content headers)
  "Send one request through CLIENT, carrying the cookie learned from the
   previous responses, and remember any new Set-Cookie.

   Returns (values RESPONSE ENV).  After the call,
   SESSION-CLIENT-LAST-SET-COOKIE is NIL exactly when this response carried
   no Set-Cookie header."
  (multiple-value-bind (response env)
      (call-app (session-client-app client) uri
                :method method
                :content content
                :headers headers
                :cookie (session-client-cookie client))
    (let ((parsed (response-set-cookie response)))
      (setf (session-client-last-set-cookie client) parsed)
      (when parsed
        (setf (session-client-cookie client)
              (format nil "~A=~A"
                      (set-cookie-name parsed)
                      (set-cookie-value parsed)))))
    (setf (session-client-last-response client) response)
    (values response env)))


;;; ------------------------------------------------------------------
;;; Session store / cookie state
;;; ------------------------------------------------------------------

(defun make-memory-session-store ()
  "A fresh lack in-memory session store, for *session-store*."
  (lack/middleware/session/store/memory:make-memory-store))

(defun make-test-cookie-state (&rest args)
  "A lack cookie state, for *session-state*.  ARGS are passed through to
   lack/middleware/session/state/cookie:make-cookie-state (:cookie-key :path
   :domain :expires :secure :httponly :samesite)."
  (apply #'lack/middleware/session/state/cookie:make-cookie-state args))

(defun fetch-stored-session (store session-id)
  "Read the session hash-table that STORE holds for SESSION-ID."
  (lack/middleware/session/store:fetch-session store session-id))


;;; ------------------------------------------------------------------
;;; Concurrency
;;; ------------------------------------------------------------------

(defun run-concurrently (worker &key (threads 2) (iterations 1) timeout)
  "Call (WORKER thread-index iteration-index) ITERATIONS times in each of
   THREADS threads.  All threads spin until they are released together, so
   the calls really overlap.

   TIMEOUT, when given, is the number of seconds to wait for the threads to
   finish.  It is a safety net rather than an assertion: unsynchronised
   concurrent writes can in principle leave a container in a state where a
   worker never finishes, and a hung test is much harder to read than a
   reported timeout.

   Returns a plist:
     :values     list of THREADS lists, each holding that thread's results in
                 iteration order
     :errors     list of (thread-index . condition) for every call that
                 signalled an error -- errors are collected, never
                 resignalled, so that a test can assert on them
     :timed-out  NIL, or the condition that ended the wait."
  (let* ((lock (bt:make-lock "clails-test-run-concurrently"))
         (errors '())
         (results (make-array threads :initial-element nil))
         (released nil)
         (workers
           (loop for i below threads
                 collect (let ((index i))
                           (bt:make-thread
                            (lambda ()
                              (loop until released
                                    do (bt:thread-yield))
                              (let ((acc '()))
                                (dotimes (j iterations)
                                  (handler-case
                                      (push (funcall worker index j) acc)
                                    (error (condition)
                                      (bt:with-lock-held (lock)
                                        (push (cons index condition) errors)))))
                                (setf (aref results index) (nreverse acc))))
                            :name (format nil "clails-test-worker-~D" index))))))
    (setf released t)
    (let ((timed-out nil))
      (flet ((join-all ()
               (dolist (worker-thread workers)
                 (bt:join-thread worker-thread))))
        (if timeout
            (handler-case (bt:with-timeout (timeout) (join-all))
              (serious-condition (condition) (setf timed-out condition)))
            (join-all)))
      (list :values (coerce results 'list)
            :errors (nreverse errors)
            :timed-out timed-out))))

(defun %resolve (spec thread-index iteration-index)
  (if (functionp spec)
      (funcall spec thread-index iteration-index)
      spec))

(defun run-concurrent-session-requests (inner &key (threads 2)
                                                   (iterations 50)
                                                   (uri "/")
                                                   (method :get)
                                                   content
                                                   store
                                                   state
                                                   (timeout 60))
  "Replay THREADS x ITERATIONS concurrent requests that all carry ONE session
   cookie, and report what the session looks like afterwards.

   A session-enabled clails stack is built over INNER, one sequential request
   opens the session, and the resulting cookie is then sent by every
   concurrent request.  No database, no server, no network: the store is
   lack's in-memory store.

   URI and CONTENT may each be a function of (thread-index iteration-index)
   when the requests must differ from each other.

   Returns a plist:
     :store           the session store used (also accepts an existing one)
     :session-id      the session id shared by all concurrent requests
     :cookie          the Cookie header value they all sent
     :session         the session hash-table as the store holds it afterwards
     :setup-response  the response of the session-opening request
     :responses       the responses of the concurrent requests
     :errors          (thread-index . condition) for each failed request
     :timed-out       NIL, or the condition that ended the wait for the
                      worker threads

   Note for M4: this helper only *measures*.  It deliberately makes no claim
   about the final counter value, because concurrent requests on one session
   currently lose updates."
  (let ((store (or store (make-memory-session-store))))
    (with-middleware-stack (app :inner inner :store store :state state)
      (let* ((setup (call-app app (%resolve uri -1 -1)
                              :method method
                              :content (%resolve content -1 -1)))
             (session-id (response-session-id setup))
             (cookie (response-cookie-pair setup))
             (result (run-concurrently
                      (lambda (thread-index iteration-index)
                        (call-app app (%resolve uri thread-index iteration-index)
                                  :method method
                                  :content (%resolve content thread-index iteration-index)
                                  :cookie cookie))
                      :threads threads
                      :iterations iterations
                      :timeout timeout)))
        (list :store store
              :session-id session-id
              :cookie cookie
              :session (when session-id
                         (fetch-stored-session store session-id))
              :setup-response setup
              :responses (apply #'append (getf result :values))
              :errors (getf result :errors)
              :timed-out (getf result :timed-out))))))

(defun hash-table-sane-p (table)
  "Walk TABLE and check that it is still internally consistent: iteration
   works, every iterated key is still reachable with GETHASH, and the number
   of iterated entries matches HASH-TABLE-COUNT.

   Returns (values SANE-P REASON).  This is how a test detects that
   concurrent writes corrupted the session container rather than merely
   losing an update."
  (handler-case
      (let ((seen 0)
            (unreachable '()))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (incf seen)
                   (unless (nth-value 1 (gethash key table))
                     (push key unreachable)))
                 table)
        (cond (unreachable
               (values nil (format nil "~D iterated key(s) not reachable with GETHASH: ~S"
                                   (length unreachable) unreachable)))
              ((/= seen (hash-table-count table))
               (values nil (format nil "iterated ~D entries but HASH-TABLE-COUNT is ~D"
                                   seen (hash-table-count table))))
              (t (values t nil))))
    (error (condition)
      (values nil (format nil "~A: ~A" (type-of condition) condition)))))
