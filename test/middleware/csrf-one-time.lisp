(in-package #:cl-user)
(defpackage #:clails-test/middleware/csrf-one-time
  (:use #:cl
        #:rove
        #:clails-test/support/http
        #:clails-test/support/csrf)
  (:import-from #:clails/middleware/csrf-middleware
                #:*csrf-session-key*
                #:*csrf-one-time-token*
                #:csrf-token))
(in-package #:clails-test/middleware/csrf-one-time)


;;; One-time tokens (*CSRF-ONE-TIME-TOKEN*).
;;;
;;; The switch itself belongs to the lower library; what this suite is for is
;;; fixing the behaviour that was measured before the setting was exposed, so
;;; that it cannot change underneath an application without a test saying so.
;;; Two of those measurements are the reason the setting could be offered at
;;; all, and they are the ones to read first:
;;;
;;;   * a FAILED verification does not consume the token, so a third party
;;;     cannot destroy a user's token by throwing invalid ones at it;
;;;   * two concurrent legitimate submissions resolve to exactly one winner --
;;;     not to two winners, not to two losers, and not to an error.


;;; ------------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------------

(defun make-form-page-app ()
  "An application that renders the token for a safe request and reports what
   the session still holds for an unsafe one.

   The second half is how \"the action body cannot read a one-time token any
   more\" is observed: by the time the action runs, the stage has consumed it."
  (lambda (env)
    (let ((session (getf env :lack.session)))
      (list 200
            (list :content-type "text/plain")
            (list (if (member (getf env :request-method) '(:post :put :patch :delete))
                      (or (gethash *csrf-session-key* session) "")
                      (csrf-token session)))))))

(defun concurrent-statuses (app cookie content &key (threads 2) (timeout 30))
  "Send THREADS requests through APP at the same time, all carrying COOKIE and
   CONTENT, and return (values STATUSES ERRORS).

   Built on CLAILS-TEST/SUPPORT/HTTP:RUN-CONCURRENTLY, which releases every
   thread from one semaphore so that the requests really do overlap."
  (let ((result (run-concurrently
                 (lambda (thread-index iteration-index)
                   (declare (ignore thread-index iteration-index))
                   (response-status (call-app app "/" :method :post
                                                      :content content
                                                      :cookie cookie)))
                 :threads threads
                 :iterations 1
                 :timeout timeout)))
    (values (apply #'append (getf result :values))
            (getf result :errors))))


;;; ------------------------------------------------------------------
;;; The default is reuse (M6-T6)
;;; ------------------------------------------------------------------

(deftest csrf-reusable-token-test
  (testing "the default setting is reuse, and nothing in it has changed"
    (ok (null *csrf-one-time-token*)))

  (testing "the same token can be sent again and again"
    (with-no-loggers
      (let ((store (make-memory-session-store)))
        (with-csrf-stack (app :inner (make-token-app) :store store :one-time nil)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (dotimes (i 3)
              (ok (eql (response-status
                        (client-request client "/" :method :post
                                                   :content (form-content token)))
                       200)
                  (format nil "submission ~D with the same token" (1+ i))))
            (ok (equal (gethash *csrf-session-key* (fetch-stored-session store session-id))
                       token)
                "and the token is still in the session afterwards")))))))


;;; ------------------------------------------------------------------
;;; One-time tokens: the basics (F1-3, F1-8)
;;; ------------------------------------------------------------------

(deftest csrf-one-time-basics-test
  (testing "the first submission is served and a replay is refused"
    (with-no-loggers
      (let ((store (make-memory-session-store)))
        ;; The application here reports what the session still holds rather
        ;; than asking for a token, because asking for one would mint a
        ;; replacement and hide the consumption.
        (with-csrf-stack (app :inner (make-form-page-app) :store store :one-time t)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (eql (response-status
                      (client-request client "/" :method :post :content (form-content token)))
                     200)
                "the first submission")
            (ok (null (gethash *csrf-session-key* (fetch-stored-session store session-id)))
                "the token was consumed")
            (ok (eql (response-status
                      (client-request client "/" :method :post :content (form-content token)))
                     403)
                "the replay")
            (ok (eql (response-status
                      (client-request client "/" :method :post :content (form-content token)))
                     403)
                "and it stays refused"))))))

  (testing "the 403 replacement and one-time work together"
    ;; F1-8: no token -> 403, the right token -> 200, a replay -> 403, with
    ;; the framework's own rejection rather than the library's 400.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (token (fetch-token client))
               (missing (client-request client "/" :method :post :content (form-content nil)))
               (accepted (client-request client "/" :method :post :content (form-content token)))
               (replay (client-request client "/" :method :post :content (form-content token))))
          (ok (eql (response-status missing) 403))
          (ok (eql (response-status accepted) 200))
          (ok (eql (response-status replay) 403))
          (ok (equal (response-header replay :content-type) "text/plain; charset=utf-8"))
          (ok (search "Reload" (response-body-string replay))
              "the body tells the refused user what to do -- which matters most here")))))

  (testing "the token is consumed in every state-changing method"
    (with-no-loggers
      (dolist (method '(:post :put :patch :delete))
        (with-csrf-stack (app :inner (make-token-app) :one-time t
                              :store (make-memory-session-store))
          (let* ((client (open-session app))
                 (token (fetch-token client)))
            (ok (eql (response-status
                      (client-request client "/" :method method :content (form-content token)))
                     200)
                (format nil "~A with the token" method))
            (ok (eql (response-status
                      (client-request client "/" :method method :content (form-content token)))
                     403)
                (format nil "~A replayed" method))))))))


;;; ------------------------------------------------------------------
;;; A failed verification must not consume the token (F1-4)
;;; ------------------------------------------------------------------
;;;
;;; This is the precondition for offering one-time tokens at all: if an
;;; invalid attempt consumed the token, anybody able to send a request could
;;; destroy a user's token -- a denial of service on the user, caused by the
;;; protection itself.

(deftest csrf-one-time-failed-attempt-test
  (testing "a wrong token does not consume the real one"
    (with-no-loggers
      (let ((store (make-memory-session-store)))
        (with-csrf-stack (app :inner (make-token-app) :store store :one-time t)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (eql (response-status
                      (client-request client "/" :method :post
                                                 :content (form-content "0123456789")))
                     403))
            (ok (equal (gethash *csrf-session-key* (fetch-stored-session store session-id))
                       token)
                "the session still holds the token")
            (ok (eql (response-status
                      (client-request client "/" :method :post :content (form-content token)))
                     200)
                "and the legitimate submission still works"))))))

  (testing "a missing token does not consume it either"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (token (fetch-token client)))
          (ok (eql (response-status
                    (client-request client "/" :method :post :content (form-content nil)))
                   403))
          (ok (eql (response-status
                    (client-request client "/" :method :post :content nil))
                   403))
          (ok (eql (response-status
                    (client-request client "/" :method :post :content (form-content token)))
                   200)
              "the legitimate submission still works")))))

  (testing "an unparseable body does not consume it either"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (token (fetch-token client)))
          (ok (eql (response-status
                    (client-request client "/" :method :post
                                               :content "{\"a\":"
                                               :headers (json-headers)))
                   403))
          (ok (eql (response-status
                    (client-request client "/" :method :post :content (form-content token)))
                   200))))))

  (testing "a third party cannot spend the token of another session"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((victim (open-session app))
               (victim-token (fetch-token victim))
               (attacker (open-session app)))
          (fetch-token attacker)
          ;; the attacker throws the victim's token at its own session, and at
          ;; no session at all
          (ok (eql (response-status
                    (client-request attacker "/" :method :post
                                                 :content (form-content victim-token)))
                   403))
          (ok (eql (response-status
                    (call-app app "/" :method :post :content (form-content victim-token)))
                   403))
          (ok (eql (response-status
                    (client-request victim "/" :method :post
                                               :content (form-content victim-token)))
                   200)
              "the victim's own submission still works"))))))


;;; ------------------------------------------------------------------
;;; Safe requests (F1-5, F1-2, F1-7)
;;; ------------------------------------------------------------------

(deftest csrf-one-time-safe-request-test
  (testing "a safe request neither replaces nor consumes the token"
    (with-no-loggers
      (let ((store (make-memory-session-store)))
        (with-csrf-stack (app :inner (make-token-app) :store store :one-time t)
          (let* ((client (open-session app))
                 (token (fetch-token client))
                 (session-id (set-cookie-value (session-client-last-set-cookie client))))
            (ok (equal (fetch-token client) token) "a second page load")
            (ok (equal (fetch-token client) token) "and a third")
            (ok (equal (gethash *csrf-session-key* (fetch-stored-session store session-id))
                       token))
            (ok (eql (response-status
                      (client-request client "/" :method :post :content (form-content token)))
                     200)
                "the token minted by the first page still works"))))))

  (testing "opening a second page does not invalidate the first page's form"
    ;; F1-2, which is what keeps one-time tokens usable at all: two tabs on
    ;; one session share one token rather than invalidating each other.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((first-tab (open-session app))
               (first-token (fetch-token first-tab))
               (second-token (fetch-token first-tab)))
          (ok (equal first-token second-token))
          (ok (eql (response-status
                    (client-request first-tab "/" :method :post
                                                  :content (form-content first-token)))
                   200)
              "the form opened first is still good")))))

  (testing "all the forms on one page carry the same token"
    ;; F1-7: the form helper may be called several times in one request, and
    ;; every call has to return the same value -- otherwise a page with two
    ;; forms would have two tokens of which only the last one is in the
    ;; session.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-token-tag-app :calls 3) :one-time t
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (tags (response-body-string (client-request client "/"))))
          (let ((lines (loop with start = 0
                             for position = (position #\Newline tags :start start)
                             collect (subseq tags start (or position (length tags)))
                             while position
                             do (setf start (1+ position)))))
            (ok (eql (length lines) 3))
            (ok (every (lambda (line) (equal line (first lines))) lines)
                "all three rendered tags are identical")
            (ok (search "type=\"hidden\"" (first lines)))
            (ok (search "_csrf_token" (first lines)))))))))


;;; ------------------------------------------------------------------
;;; What the action body can still read (F1-1)
;;; ------------------------------------------------------------------

(deftest csrf-one-time-action-body-test
  (testing "a successful one-time submission cannot read the token any more"
    ;; The consumption happens in the stage, before the action runs.  An
    ;; application that wants to show the user a new form has to re-render it,
    ;; which mints a new token -- so \"submit, then re-render\" is
    ;; self-healing.
    (with-no-loggers
      (with-csrf-stack (app :inner (make-form-page-app) :one-time t
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (token (fetch-token client))
               (submitted (client-request client "/" :method :post
                                                     :content (form-content token))))
          (ok (eql (response-status submitted) 200))
          (ok (equal (response-body-string submitted) "")
              "the action body found no token in the session")
          (let ((new-token (fetch-token client)))
            (ok (stringp new-token))
            (ok (not (equal new-token token))
                "re-rendering the form minted a new token")
            (ok (eql (response-status
                      (client-request client "/" :method :post
                                                 :content (form-content new-token)))
                     200)
                "and the new token works"))))))

  (testing "with reusable tokens the action body still reads the token"
    (with-no-loggers
      (with-csrf-stack (app :inner (make-form-page-app) :one-time nil
                            :store (make-memory-session-store))
        (let* ((client (open-session app))
               (token (fetch-token client))
               (submitted (client-request client "/" :method :post
                                                     :content (form-content token))))
          (ok (eql (response-status submitted) 200))
          (ok (equal (response-body-string submitted) token)))))))


;;; ------------------------------------------------------------------
;;; Concurrent submissions (F1-6)
;;; ------------------------------------------------------------------

(deftest csrf-one-time-concurrency-test
  (testing "two concurrent legitimate submissions resolve to exactly one winner"
    ;; Repeated, because the property being asserted is about a race: a single
    ;; round could pass by luck.  Each round opens a fresh session, mints a
    ;; token and submits it twice at once.
    (let ((rounds 12)
          (double-wins 0)
          (single-wins 0)
          (no-wins 0)
          (other '())
          (errors '()))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-token-app) :one-time t
                              :store (make-memory-session-store))
          (dotimes (round rounds)
            (declare (ignore round))
            (let* ((client (open-session app))
                   (token (fetch-token client))
                   (cookie (session-client-cookie client)))
              (multiple-value-bind (statuses round-errors)
                  (concurrent-statuses app cookie (form-content token))
                (setf errors (append errors round-errors))
                (let ((accepted (count 200 statuses))
                      (refused (count 403 statuses)))
                  (cond ((not (eql (+ accepted refused) 2))
                         (push statuses other))
                        ((eql accepted 2) (incf double-wins))
                        ((eql accepted 1) (incf single-wins))
                        (t (incf no-wins)))))))))
      (ok (eql single-wins rounds)
          (format nil "~D of ~D rounds had exactly one winner" single-wins rounds))
      (ok (eql double-wins 0) "no round accepted both submissions")
      (ok (eql no-wins 0) "no round refused both submissions")
      (ok (null other) (format nil "every response was a 200 or a 403: ~S" other))
      (ok (null errors) (format nil "no request signalled: ~S" errors))))

  (testing "with reusable tokens both concurrent submissions are served"
    ;; The contrast that shows the exclusion above really comes from the
    ;; one-time setting and not from the stack serialising requests.
    (let ((both-accepted 0)
          (rounds 5))
      (with-no-loggers
        (with-csrf-stack (app :inner (make-token-app) :one-time nil
                              :store (make-memory-session-store))
          (dotimes (round rounds)
            (declare (ignore round))
            (let* ((client (open-session app))
                   (token (fetch-token client))
                   (cookie (session-client-cookie client)))
              (multiple-value-bind (statuses errors)
                  (concurrent-statuses app cookie (form-content token))
                (ok (null errors))
                (when (eql (count 200 statuses) 2)
                  (incf both-accepted)))))))
      (ok (eql both-accepted rounds)))))
