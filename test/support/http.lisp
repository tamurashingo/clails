(in-package #:cl-user)
(defpackage #:clails-test/support/http
  (:use #:cl)
  ;; NOTE: lack-test is an ordinary (non-package-inferred) ASDF system that
  ;; provides exactly one package, LACK/TEST, and lack.asd additionally
  ;; defines a forwarding system named "lack/test" that depends on it.  A
  ;; single :import-from #:lack/test is therefore resolvable by clails-test's
  ;; package-inferred-system: ASDF looks for a system called "lack/test" and
  ;; finds that forwarding definition.
  ;;
  ;; The same is NOT true for the lack session sub-packages
  ;; (lack/middleware/session/store, .../store/memory, .../state/cookie):
  ;; lack.asd defines no forwarding system for those names, so adding
  ;; :import-from clauses for them would make the package-inferred-system try
  ;; (and fail) to find matching components -- exactly the trap documented in
  ;; src/middleware/session-middleware.lisp (L4-L12).  They are referenced
  ;; with fully-qualified symbols below instead; they are already loaded as a
  ;; side effect of depending on clails/middleware/session-middleware.
  (:import-from #:lack/test
                #:generate-env
                #:testing-app)
  (:import-from #:cl-cookie
                #:make-cookie-jar)
  (:import-from #:babel
                #:octets-to-string)
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
           #:response-header-values
           #:response-body-string
           ;; Set-Cookie inspection
           #:set-cookie
           #:set-cookie-p
           #:set-cookie-name
           #:set-cookie-value
           #:set-cookie-attributes
           #:parse-set-cookie
           #:parse-http-date
           #:response-set-cookie
           #:response-set-cookies
           #:set-cookie-attribute
           #:set-cookie-attribute-present-p
           #:set-cookie-max-age
           #:set-cookie-expires-at
           #:set-cookie-expired-p
           #:response-cookie-pair
           #:response-session-id
           ;; carrying cookies across requests
           #:+default-session-cookie-name+
           #:session-client
           #:make-session-client
           #:session-client-jar
           #:session-client-cookie
           #:session-client-session-cookie-name
           #:session-client-last-set-cookie
           #:session-client-last-set-cookies
           #:session-client-last-response
           #:client-request
           ;; session store access
           #:make-memory-session-store
           #:make-lack-memory-session-store
           #:make-test-cookie-state
           #:fetch-stored-session
           #:remove-stored-session
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

(defun response-header-values (response name)
  "Return every value RESPONSE emitted for the header NAME, in the order they
   were emitted.

   Lack response headers are a plist, and that plist may legitimately carry
   the same key more than once: lack/response:finalize-cookies APPENDS one
   :set-cookie entry per baked cookie, so a response whose application set a
   cookie of its own carries several :set-cookie headers.  GETF would see
   only the first of them -- which is why anything looking at Set-Cookie has
   to go through this function."
  (loop for (key value) on (response-headers response) by #'cddr
        when (eq key name)
        collect value))

(defun response-header (response name)
  "Return the FIRST value RESPONSE emitted for the header NAME (a keyword
   such as :content-type), or NIL when the header was not emitted at all.

   Only meaningful for a header that is emitted at most once.  Use
   RESPONSE-HEADER-VALUES for a header that may repeat (:set-cookie does)."
  (first (response-header-values response name)))

(defun %write-body-part (part out)
  "Write one element of a Lack response body to the stream OUT."
  (etypecase part
    (null nil)
    (string (write-string part out))
    (pathname (write-string (uiop:read-file-string part) out))
    ((vector (unsigned-byte 8))
     (write-string (octets-to-string part :encoding :utf-8) out))
    (cons (dolist (element part)
            (%write-body-part element out)))))

(defun response-body-string (response)
  "Return the response body as one string.

   lack/response:finalize-response accepts four shapes of body -- a string, a
   list of strings, an octet vector and a pathname -- and all four turn up in
   practice: lack/middleware/static, which is part of the production stage
   list, answers with a pathname.  All four are decoded here; octets are
   decoded as UTF-8 and a pathname is read from the file system.

   The parts are written into one output stream instead of being handed to
   CONCATENATE as arguments, so a body split into more parts than
   CALL-ARGUMENTS-LIMIT allows is still readable."
  (with-output-to-string (out)
    (%write-body-part (third response) out)))


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

(defun response-set-cookies (response)
  "Parse every Set-Cookie header of RESPONSE, in emission order."
  (mapcar #'parse-set-cookie (response-header-values response :set-cookie)))

(defun response-set-cookie (response &optional name)
  "Parse the Set-Cookie header of RESPONSE, or NIL when the response carries
   none.

   NAME, when given, selects the cookie of that name -- the only reliable way
   to pick one out of a response that sets several cookies.  When NAME is
   omitted the response must carry at most one Set-Cookie header; an
   ambiguous response signals an error rather than silently answering about
   the wrong cookie."
  (let ((cookies (response-set-cookies response)))
    (cond (name (find name cookies :key #'set-cookie-name :test #'string=))
          ((null (rest cookies)) (first cookies))
          (t (error "RESPONSE carries ~D Set-Cookie headers (~{~S~^, ~}); ~
                     pass the cookie name to say which one is meant."
                    (length cookies)
                    (mapcar #'set-cookie-name cookies))))))

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

(defparameter +http-month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Month abbreviations as they appear in an HTTP-date.")

(defparameter +http-weekday-prefixes+
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Weekday abbreviations as they appear in an HTTP-date; the RFC 850 format
   spells them out in full, so only the prefix is matched.")

(defun %split-on (characters string)
  "Split STRING at every occurrence of any of CHARACTERS, dropping the empty
   parts."
  (let ((tokens '())
        (start 0)
        (length (length string)))
    (loop
      (let ((pos (position-if (lambda (char) (member char characters))
                              string :start start)))
        (let ((token (subseq string start (or pos length))))
          (unless (string= token "") (push token tokens)))
        (if pos
            (setf start (1+ pos))
            (return))))
    (nreverse tokens)))

(defun %parse-decimal (string)
  "STRING as an integer (a leading sign is allowed), or NIL when it is not
   exactly one integer."
  (multiple-value-bind (value end)
      (parse-integer string :junk-allowed t)
    (and value (= end (length string)) value)))

(defun %parse-digits (string)
  "STRING as a non-negative integer, or NIL unless every character is a
   digit.  Unlike %PARSE-DECIMAL this rejects a signed token, so a timezone
   offset such as \"+0000\" is not mistaken for a day or a year."
  (and (plusp (length string))
       (every #'digit-char-p string)
       (parse-integer string)))

(defun %expand-two-digit-year (year)
  "RFC 6265 5.1.1: a two-digit year of 70..99 means 19xx, 0..69 means 20xx."
  (cond ((>= year 100) year)
        ((>= year 70) (+ 1900 year))
        (t (+ 2000 year))))

(defun parse-http-date (string)
  "Parse an HTTP-date into a universal time, or NIL when STRING is not a date
   this function recognises.

   All three formats RFC 7231 allows are accepted -- IMF-fixdate
   (\"Mon, 30 Jul 2153 21:39:38 GMT\", which is what lack/response:bake-cookie
   writes), RFC 850 (\"Monday, 30-Jul-53 21:39:38 GMT\") and asctime
   (\"Mon Jul 30 21:39:38 2153\") -- because an Expires attribute is written
   by whoever set the cookie, not only by lack.

   NIL means \"not understood\", never \"expired\": a cookie whose Expires
   cannot be read is treated as a session cookie, which is the conservative
   choice for a test harness."
  (when (stringp string)
    (let ((tokens (%split-on '(#\Space #\Tab #\, #\-) string))
          (day nil) (month nil) (year nil)
          (hour 0) (minute 0) (sec 0))
      ;; the leading weekday carries no information and gets in the way
      (when (and tokens
                 (find-if (lambda (prefix)
                            (and (>= (length (first tokens)) (length prefix))
                                 (string-equal prefix (first tokens)
                                               :end2 (length prefix))))
                          +http-weekday-prefixes+))
        (pop tokens))
      (dolist (token tokens)
        (let ((month-index (position token +http-month-names+ :test #'string-equal))
              (number (%parse-digits token)))
          (cond (month-index (setf month (1+ month-index)))
                ((find #\: token)
                 (let ((parts (mapcar #'%parse-digits (%split-on '(#\:) token))))
                   (when (and (= (length parts) 3) (every #'identity parts))
                     (setf hour (first parts) minute (second parts) sec (third parts)))))
                ;; The first plain number is the day and the second is the
                ;; year, whichever of the two comes first -- which holds for
                ;; all three formats.
                (number
                 (cond ((null day) (setf day number))
                       ((null year) (setf year number)))))))
      (when (and day month year)
        (ignore-errors
         (encode-universal-time sec minute hour day month
                                (%expand-two-digit-year year)
                                0))))))

(defun set-cookie-max-age (set-cookie)
  "The Max-Age of SET-COOKIE in seconds, or NIL when the attribute is absent
   or not an integer."
  (let ((raw (set-cookie-attribute set-cookie "max-age")))
    (and (stringp raw) (%parse-decimal (%trim raw)))))

(defun set-cookie-expires-at (set-cookie)
  "The universal time at which SET-COOKIE expires, or NIL when it is a
   session cookie -- no Max-Age and no readable Expires."
  (let ((max-age (set-cookie-max-age set-cookie)))
    (if max-age
        (+ (get-universal-time) max-age)
        (let ((raw (set-cookie-attribute set-cookie "expires")))
          (and (stringp raw) (parse-http-date raw))))))

(defun set-cookie-expired-p (set-cookie &optional (now (get-universal-time)))
  "True when SET-COOKIE tells the client to drop the cookie immediately: a
   Max-Age of zero or less, or an Expires that is not in the future.

   RFC 6265 gives Max-Age precedence over Expires, and so does this.  A
   session cookie -- neither attribute present -- is never expired."
  (let ((max-age (set-cookie-max-age set-cookie)))
    (if max-age
        (<= max-age 0)
        (let ((raw (set-cookie-attribute set-cookie "expires")))
          (let ((expires (and (stringp raw) (parse-http-date raw))))
            (and expires (<= expires now)))))))

(defun response-cookie-pair (response &optional name)
  "Return \"name=value\" for the Set-Cookie of RESPONSE -- ready to be handed
   back as a Cookie header on the next request -- or NIL.  NAME selects one
   cookie out of a response that sets several (see RESPONSE-SET-COOKIE)."
  (let ((parsed (response-set-cookie response name)))
    (when parsed
      (format nil "~A=~A" (set-cookie-name parsed) (set-cookie-value parsed)))))

(defun response-session-id (response &optional name)
  "Return the session id carried by the Set-Cookie of RESPONSE, or NIL.  NAME
   selects one cookie out of a response that sets several (see
   RESPONSE-SET-COOKIE)."
  (let ((parsed (response-set-cookie response name)))
    (when parsed
      (set-cookie-value parsed))))


;;; ------------------------------------------------------------------
;;; Carrying cookies across requests
;;; ------------------------------------------------------------------

(defparameter +default-session-cookie-name+ "lack.session"
  "The cookie name lack/middleware/session/state/cookie uses by default.")

(defstruct (session-client (:constructor %make-session-client))
  "A tiny browser: remembers the cookies a response sets and sends them back.

   It remembers *every* cookie of the response, not only the session cookie,
   because lack emits one Set-Cookie header per cookie: an application that
   sets a cookie of its own would otherwise shadow the session cookie and the
   client would send the wrong value back.

   It also drops a cookie the response declared dead -- Max-Age of zero or
   less, or Expires not in the future -- the way a browser drops it.  Keeping
   such a cookie would let a test observe a session id being reused after the
   server told the client to forget it, which no real browser would ever do.

   SESSION-COOKIE-NAME is the cookie LAST-SET-COOKIE reports on and the one
   session assertions are about; JAR holds all of them as an alist of
   (name . value) in the order they were first seen."
  app
  (session-cookie-name +default-session-cookie-name+ :type string)
  (jar nil :type list)
  (last-set-cookies nil :type list)
  (last-set-cookie nil)
  (last-response nil))

(defun make-session-client (app &key (session-cookie-name +default-session-cookie-name+))
  (%make-session-client :app app :session-cookie-name session-cookie-name))

(defun session-client-cookie (client)
  "The Cookie header value CLIENT sends on its next request: every cookie it
   still holds, as \"name=value\" pairs separated by \"; \", or NIL when it
   holds none."
  (let ((jar (session-client-jar client)))
    (when jar
      (format nil "~{~A~^; ~}"
              (loop for (name . value) in jar
                    collect (format nil "~A=~A" name value))))))

(defun %store-cookie (client parsed now)
  "Apply one parsed Set-Cookie to CLIENT's jar, NOW being the time the
   response was received."
  (let* ((name (set-cookie-name parsed))
         (entry (assoc name (session-client-jar client) :test #'string=)))
    (cond ((set-cookie-expired-p parsed now)
           (when entry
             (setf (session-client-jar client)
                   (remove entry (session-client-jar client)))))
          (entry
           (setf (cdr entry) (set-cookie-value parsed)))
          (t
           (setf (session-client-jar client)
                 (append (session-client-jar client)
                         (list (cons name (set-cookie-value parsed)))))))))

(defun client-request (client uri &key (method :get) content headers)
  "Send one request through CLIENT, carrying the cookies learned from the
   previous responses, and remember the cookies this response sets.

   Returns (values RESPONSE ENV).  After the call,
   SESSION-CLIENT-LAST-SET-COOKIES holds every cookie this response set, and
   SESSION-CLIENT-LAST-SET-COOKIE is NIL exactly when the response set no
   cookie named SESSION-CLIENT-SESSION-COOKIE-NAME."
  (multiple-value-bind (response env)
      (call-app (session-client-app client) uri
                :method method
                :content content
                :headers headers
                :cookie (session-client-cookie client))
    (let ((parsed (response-set-cookies response))
          (now (get-universal-time)))
      (setf (session-client-last-set-cookies client) parsed)
      (setf (session-client-last-set-cookie client)
            (find (session-client-session-cookie-name client) parsed
                  :key #'set-cookie-name :test #'string=))
      (dolist (cookie parsed)
        (%store-cookie client cookie now)))
    (setf (session-client-last-response client) response)
    (values response env)))


;;; ------------------------------------------------------------------
;;; Session store / cookie state
;;; ------------------------------------------------------------------
;;;
;;; clails' own store package DOES have a matching ASDF system
;;; (clails/middleware/session/store/memory), so it could be named in an
;;; :import-from clause -- but its constructor is spelled exactly like the
;;; helper below, so it is referenced fully-qualified here to keep the two
;;; apart.  The lack sub-packages are fully-qualified for the different reason
;;; explained at the top of this file.

(defun make-memory-session-store ()
  "A fresh clails in-memory session store, for *session-store*.

   This is the store the framework itself falls back to when *session-store*
   is NIL, so a test that hands this in is testing the default configuration."
  (clails/middleware/session/store/memory:make-memory-session-store))

(defun make-lack-memory-session-store ()
  "A fresh LACK in-memory session store, for *session-store*.

   A lack session store is a structure, not a clails <session-store>; older
   clails releases documented *session-store* as holding exactly this.  It is
   kept available so that the compatibility of those configurations can be
   asserted rather than assumed."
  (lack/middleware/session/store/memory:make-memory-store))

(defun make-test-cookie-state (&rest args)
  "A lack cookie state, for *session-state*.  ARGS are passed through to
   lack/middleware/session/state/cookie:make-cookie-state (:cookie-key :path
   :domain :expires :secure :httponly :samesite)."
  (apply #'lack/middleware/session/state/cookie:make-cookie-state args))

(defun fetch-stored-session (store session-id)
  "Read the session hash-table that STORE holds for SESSION-ID.

   Goes through lack's generic function, which is the one the session stage
   itself calls, so this works for both kinds of store a test may build: a
   lack store structure and a clails <session-store> derivation (the latter
   bridges lack's three operations to its own)."
  (lack/middleware/session/store:fetch-session store session-id))

(defun remove-stored-session (store session-id)
  "Drop SESSION-ID from STORE, through the same generic function the session
   stage uses.  Works for lack stores and clails stores alike."
  (lack/middleware/session/store:remove-session store session-id))


;;; ------------------------------------------------------------------
;;; Concurrency
;;; ------------------------------------------------------------------

(defun %destroy-stragglers (threads)
  "Destroy every thread in THREADS that is still running, and return how many
   there were.

   Called from RUN-CONCURRENTLY's cleanup.  A worker that outlived the
   timeout would otherwise go on pushing onto the error list and writing into
   the result vector while they are being read, and go on sending requests
   into whatever test rove runs next -- so the timeout would leak the hang
   instead of containing it."
  (let ((destroyed 0))
    (dolist (thread threads)
      (when (bt:thread-alive-p thread)
        (incf destroyed)
        (handler-case (bt:destroy-thread thread)
          (serious-condition () nil))))
    ;; Wait for the unwinding to finish, but never hang in the cleanup itself.
    (dolist (thread threads)
      (handler-case (bt:with-timeout (5) (bt:join-thread thread))
        (serious-condition () nil)))
    destroyed))

(defun run-concurrently (worker &key (threads 2) (iterations 1) timeout)
  "Call (WORKER thread-index iteration-index) ITERATIONS times in each of
   THREADS threads.  All threads wait on one semaphore until they are released
   together, so the calls really overlap.

   TIMEOUT, when given, is the number of seconds to wait for the threads to
   finish.  It is a safety net rather than an assertion: unsynchronised
   concurrent writes can in principle leave a container in a state where a
   worker never finishes, and a hung test is much harder to read than a
   reported timeout.  When it fires, the threads still running are destroyed
   before the results are read, so the safety net contains the hang rather
   than leaking it into the next test.

   Returns a plist:
     :values     list of THREADS lists, each holding that thread's results in
                 iteration order
     :errors     list of (thread-index . condition) for every call that
                 signalled an error -- errors are collected, never
                 resignalled, so that a test can assert on them
     :timed-out  NIL, or the condition that ended the wait
     :abandoned  how many threads were still running when the wait ended and
                 had to be destroyed (0 on a clean run)"
  (let* ((lock (bt:make-lock "clails-test-run-concurrently"))
         (gate (bt:make-semaphore :name "clails-test-run-concurrently-gate"))
         (errors '())
         (results (make-array threads :initial-element nil))
         (workers '())
         (abandoned 0)
         (timed-out nil))
    (unwind-protect
         (progn
           ;; The threads are created inside the UNWIND-PROTECT so that a
           ;; MAKE-THREAD failing part way through the loop cannot leave the
           ;; threads already created waiting on a gate that nobody opens.
           (setf workers
                 (loop for i below threads
                       collect (let ((index i))
                                 (bt:make-thread
                                  (lambda ()
                                    (bt:wait-on-semaphore gate)
                                    (let ((acc '()))
                                      (dotimes (j iterations)
                                        (handler-case
                                            (push (funcall worker index j) acc)
                                          (error (condition)
                                            (bt:with-lock-held (lock)
                                              (push (cons index condition) errors)))))
                                      ;; written under the lock: the reader
                                      ;; below takes the same lock
                                      (bt:with-lock-held (lock)
                                        (setf (aref results index) (nreverse acc)))))
                                  :name (format nil "clails-test-worker-~D" index)))))
           (bt:signal-semaphore gate :count threads)
           (flet ((join-all ()
                    (dolist (worker-thread workers)
                      (bt:join-thread worker-thread))))
             (if timeout
                 (handler-case (bt:with-timeout (timeout) (join-all))
                   (serious-condition (condition) (setf timed-out condition)))
                 (join-all))))
      ;; Release anyone still at the gate (MAKE-THREAD may have failed before
      ;; the semaphore was signalled) and make sure nothing outlives the call.
      (bt:signal-semaphore gate :count threads)
      (setf abandoned (%destroy-stragglers workers)))
    ;; Every writer is dead by now, and the lock makes the read safe against
    ;; a writer that was still finishing while the stragglers were destroyed.
    (bt:with-lock-held (lock)
      (list :values (coerce results 'list)
            :errors (reverse errors)
            :timed-out timed-out
            :abandoned abandoned))))

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
                                                   (session-cookie-name
                                                    +default-session-cookie-name+)
                                                   (timeout 60))
  "Replay THREADS x ITERATIONS concurrent requests that all carry ONE session
   cookie, and report what the session looks like afterwards.

   A session-enabled clails stack is built over INNER, one sequential request
   opens the session, and the resulting cookie is then sent by every
   concurrent request.  No database, no server, no network: the store is
   lack's in-memory store.

   The opening request goes through a SESSION-CLIENT, so an INNER that sets
   cookies of its own does not shadow the session cookie;
   SESSION-COOKIE-NAME says which cookie the session id is read from (pass it
   when STATE uses a non-default :cookie-key).

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
     :abandoned       how many worker threads had to be destroyed (0 on a
                      clean run)

   Note for M4: this helper only *measures*.  It deliberately makes no claim
   about the final counter value, because concurrent requests on one session
   currently lose updates."
  (let ((store (or store (make-memory-session-store))))
    (with-middleware-stack (app :inner inner :store store :state state)
      (let* ((client (make-session-client app :session-cookie-name session-cookie-name))
             (setup (client-request client (%resolve uri -1 -1)
                                    :method method
                                    :content (%resolve content -1 -1)))
             (set-cookie (session-client-last-set-cookie client))
             (session-id (and set-cookie (set-cookie-value set-cookie)))
             (cookie (session-client-cookie client))
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
              :timed-out (getf result :timed-out)
              :abandoned (getf result :abandoned))))))

(defun hash-table-sane-p (table &key (timeout 10))
  "Walk TABLE and check that it is still internally consistent: iteration
   works, every iterated key is still reachable with GETHASH, and the number
   of iterated entries matches HASH-TABLE-COUNT.

   Returns (values SANE-P REASON).  This is how a test detects that
   concurrent writes corrupted the session container rather than merely
   losing an update.

   The walk runs under TIMEOUT seconds (NIL disables it) because a table that
   concurrent writes left half-rehashed can make MAPHASH or GETHASH loop
   without ever signalling a condition.  A HANDLER-CASE alone cannot catch
   that -- it would hang the whole test run, which is exactly the failure
   RUN-CONCURRENTLY's timeout exists to keep out."
  (flet ((walk ()
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
                   (t (values t nil))))))
    (handler-case
        (if timeout
            (bt:with-timeout (timeout) (walk))
            (walk))
      (serious-condition (condition)
        (values nil (format nil "~A: ~A" (type-of condition) condition))))))
