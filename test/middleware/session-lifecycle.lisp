(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-lifecycle
  (:use #:cl
        #:rove
        #:clails-test/support/http)
  (:import-from #:clails/controller/base-controller
                #:<base-controller>
                #:session
                #:current-user)
  (:import-from #:clails/middleware/session/lifecycle
                #:+session-reserved-key-prefix+
                #:session-reserved-key-p
                #:session-table
                #:session-options
                #:session-id
                #:session-new-p
                #:session-value
                #:session-delete
                #:session-clear
                #:session-destroy
                #:session-regenerate-id
                #:session-destroyed-p
                #:session-id-changing-p
                #:clear-session-table))
(in-package #:clails-test/middleware/session-lifecycle)


;;; M5-T1 and M5-T2: the session lifecycle API.
;;;
;;;   M5-T1  read, write, delete one key, clear every key
;;;   M5-T2  destroy the session (logout), rename it (fixation defence)
;;;
;;; Two levels are exercised throughout, because the API has to work at both:
;;; against a synthetic request environment (and a controller holding one),
;;; which is where the semantics are pinned down exactly, and over a real HTTP
;;; round trip, which is where "the next request sees it" can be asserted at
;;; all.


;;; ------------------------------------------------------------------
;;; A request environment, and a controller holding one
;;; ------------------------------------------------------------------

(defun make-test-env (&key (id "0123456789abcdef0123456789abcdef01234567")
                           (new-session nil)
                           (contents '()))
  "A request environment shaped like the one the session stage builds."
  (let ((session (make-hash-table :test 'equal)))
    (loop for (key value) on contents by #'cddr
          do (setf (gethash key session) value))
    (list :path-info "/"
          :lack.session session
          :lack.session.options (list :id id
                                      :new-session new-session
                                      :change-id nil
                                      :expire nil))))

(defun make-test-controller (env)
  (let ((controller (make-instance '<base-controller>)))
    (setf (slot-value controller 'clails/controller/base-controller::env) env)
    controller))


;;; ------------------------------------------------------------------
;;; M5-T1: read, write, delete, clear
;;; ------------------------------------------------------------------

(deftest session-value-read-write-test
  (testing "a value can be written and read back"
    (let ((env (make-test-env)))
      (ok (null (session-value env "user")))
      (setf (session-value env "user") "alice")
      (ok (string= (session-value env "user") "alice"))
      (ok (string= (gethash "user" (session-table env)) "alice")
          "and it really is in the session table")))

  (testing "a missing key answers the default"
    (let ((env (make-test-env)))
      (ok (null (session-value env "absent")))
      (ok (eq (session-value env "absent" :fallback) :fallback))
      ;; a stored NIL is not the same as a missing key
      (setf (session-value env "explicit-nil") nil)
      (ok (null (session-value env "explicit-nil" :fallback))
          "a key holding NIL answers NIL, not the default")))

  (testing "the same API works through a controller"
    (let* ((env (make-test-env))
           (controller (make-test-controller env)))
      (setf (session-value controller "cart") '(1 2 3))
      (ok (equal (session-value controller "cart") '(1 2 3)))
      (ok (equal (gethash "cart" (session controller)) '(1 2 3))
          "the controller's session table is the one that was written")))

  (testing "reading a session the stage never built is an error"
    (let ((controller (make-test-controller '(:path-info "/"))))
      (ok (signals (session-value controller "user") 'error))
      (ok (signals (session-clear controller) 'error))
      (ok (signals (session-id controller) 'error)))))


(deftest session-delete-test
  (testing "one key can be deleted, the rest stays"
    (let ((env (make-test-env :contents '("a" 1 "b" 2))))
      (ok (session-delete env "a"))
      (ok (null (session-value env "a")))
      (ok (eql (session-value env "b") 2))
      (ok (= (hash-table-count (session-table env)) 1))))

  (testing "deleting a key that is not there is not an error"
    (let ((env (make-test-env)))
      (ok (null (session-delete env "absent")))))

  (testing "it works through a controller too"
    (let* ((env (make-test-env :contents '("a" 1)))
           (controller (make-test-controller env)))
      (ok (session-delete controller "a"))
      (ok (zerop (hash-table-count (session controller)))))))


(deftest session-clear-test
  (testing "every value goes, and the session stays usable"
    (let ((env (make-test-env :contents '("a" 1 "b" 2 "c" 3))))
      (let ((table (session-table env)))
        (session-clear env)
        (ok (zerop (hash-table-count table)))
        (ok (eq (session-table env) table)
            "the container is emptied, not replaced")
        (setf (session-value env "after") "ok")
        (ok (string= (session-value env "after") "ok")))))

  ;; The decision M5-T1 asks for, asserted both ways round.
  (testing "by default the framework's own keys go as well"
    (let* ((env (make-test-env))
           (controller (make-test-controller env)))
      (setf (current-user controller) '(:id 1))
      (setf (session-value env "cart") '(1))
      (ok (current-user controller) "the user is in the session to begin with")
      (session-clear env)
      (ok (null (current-user controller))
          "clearing the session logs the user out -- the safe reading of \"clear\"")
      (ok (null (session-value env "cart")))
      (ok (zerop (hash-table-count (session-table env))))))

  (testing ":keep-reserved keeps them"
    (let* ((env (make-test-env))
           (controller (make-test-controller env)))
      (setf (current-user controller) '(:id 1))
      (setf (session-value env "cart") '(1))
      (session-clear env :keep-reserved t)
      (ok (current-user controller)
          "the framework's bookkeeping survives when the caller asks for it")
      (ok (null (session-value env "cart"))
          "the application's keys go either way")
      (ok (= (hash-table-count (session-table env)) 1))))

  (testing "the reserved prefix is what tells the two apart"
    (ok (string= +session-reserved-key-prefix+ "clails."))
    (ok (session-reserved-key-p "clails.current-user"))
    (ok (session-reserved-key-p "clails.csrf-token"))
    (ok (not (session-reserved-key-p "current-user")))
    (ok (not (session-reserved-key-p "clail")))
    (ok (not (session-reserved-key-p "")))
    (ok (not (session-reserved-key-p :a-symbol))
        "a non-string key is never reserved"))

  (testing "the low-level helper behaves the same on a bare table"
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash "clails.current-user" table) 1)
      (setf (gethash "cart" table) 2)
      (clear-session-table table :keep-reserved t)
      (ok (= (hash-table-count table) 1))
      (clear-session-table table)
      (ok (zerop (hash-table-count table))))))


(deftest session-clear-roundtrip-test
  (testing "a cleared session is still cleared on the next request"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack
          (app :store store
               :inner (lambda (env)
                        ;; /write puts two values in, /clear empties the
                        ;; session, every request answers what it can see
                        (cond ((string= (getf env :path-info) "/write")
                               (setf (session-value env "a") 1)
                               (setf (session-value env "b") 2))
                              ((string= (getf env :path-info) "/clear")
                               (session-clear env)))
                        (list 200
                              (list :content-type "text/plain")
                              (list (format nil "~D" (hash-table-count (session-table env)))))))
        (let* ((client (make-session-client app))
               (session-id (progn (client-request client "/write")
                                  (set-cookie-value
                                   (session-client-last-set-cookie client)))))
          (ok (string= (response-body-string (client-request client "/")) "2")
              "the values are there on the request after the write")
          (ok (string= (response-body-string (client-request client "/clear")) "0"))
          (ok (string= (response-body-string (client-request client "/")) "0")
              "and the next request sees the cleared session, not the old values")
          (ok (zerop (hash-table-count (fetch-stored-session store session-id)))
              "the store holds the cleared session as well")
          ;; the session itself survived the clear: same id, same cookie
          (ok (string= (set-cookie-value
                        (or (session-client-last-set-cookie client)
                            (parse-set-cookie
                             (format nil "~A=~A" +default-session-cookie-name+ session-id))))
                       session-id)
              "clearing does not rename or end the session"))))))


;;; ------------------------------------------------------------------
;;; M5-T2: destroying a session
;;; ------------------------------------------------------------------

(deftest session-destroy-unit-test
  (testing "destroy empties the session and marks the request"
    (let ((env (make-test-env :contents '("a" 1))))
      (ok (not (session-destroyed-p env)))
      (session-destroy env)
      (ok (session-destroyed-p env))
      (ok (zerop (hash-table-count (session-table env)))
          "nothing can be read back out of a destroyed session")
      (ok (getf (session-options env) :expire))))

  (testing "destroy wins over a pending rename"
    (let ((env (make-test-env)))
      (ok (session-regenerate-id env))
      (session-destroy env)
      (ok (session-destroyed-p env))
      (ok (not (session-id-changing-p env))
          "a session that is ending has no use for a new name")
      (ok (null (session-regenerate-id env))
          "and asking for one afterwards does nothing"))))


(deftest session-destroy-roundtrip-test
  (testing "the response of a destroyed session expires the cookie"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack
          (app :store store
               :inner (lambda (env)
                        (when (string= (getf env :path-info) "/logout")
                          (session-destroy env))
                        (list 200 (list :content-type "text/plain") (list "ok"))))
        (let* ((client (make-session-client app))
               (session-id (progn (client-request client "/")
                                  (set-cookie-value
                                   (session-client-last-set-cookie client)))))
          (ok (= (length session-id) 40))
          (ok (fetch-stored-session store session-id) "the session is in the store")
          (let* ((response (client-request client "/logout"))
                 (cookie (response-set-cookie response +default-session-cookie-name+)))
            (ok cookie "destroying the session re-emits the cookie")
            (ok (string= (set-cookie-value cookie) session-id)
                "the expiring Set-Cookie still names the id being retired")
            (ok (set-cookie-expired-p cookie)
                "and it is expired: the client is told to delete it")
            (ok (set-cookie-attribute-present-p cookie "expires")
                "the deletion is expressed as a date in the past")
            (ok (null (session-client-jar client))
                "so the client drops the cookie, as a browser would")
            (ok (null (fetch-stored-session store session-id))
                "and the session is gone from the store"))
          ;; the next request is a brand new visit
          (let ((response (client-request client "/")))
            (ok (response-set-cookie response +default-session-cookie-name+))
            (ok (string/= (set-cookie-value
                           (session-client-last-set-cookie client))
                          session-id)
                "with an id that is not the retired one"))))))

  (testing "a destroyed session is empty for the rest of the request"
    (let ((seen :unset))
      (with-middleware-stack
          (app :inner (lambda (env)
                        (setf (session-value env "a") 1)
                        (session-destroy env)
                        (setf seen (hash-table-count (session-table env)))
                        (list 200 (list :content-type "text/plain") (list "ok"))))
        (call-app app "/")
        (ok (eql seen 0))))))


;;; ------------------------------------------------------------------
;;; M5-T2: renaming a session
;;; ------------------------------------------------------------------

(deftest session-regenerate-id-unit-test
  (testing "regenerate marks the request and keeps the contents"
    (let ((env (make-test-env :contents '("a" 1))))
      (ok (not (session-id-changing-p env)))
      (ok (session-regenerate-id env))
      (ok (session-id-changing-p env))
      (ok (eql (session-value env "a") 1)
          "nothing is thrown away -- that is the difference from destroy")))

  (testing "the old id is still what the request reports"
    (let ((env (make-test-env :id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
      (session-regenerate-id env)
      (ok (string= (session-id env) "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
          "the new id does not exist until the response is finalised"))))


(deftest session-regenerate-id-roundtrip-test
  (testing "the id changes, the contents survive, the old id is gone"
    (let ((store (make-memory-session-store)))
      (with-middleware-stack
          (app :store store
               :inner (lambda (env)
                        (setf (session-value env "visits")
                              (1+ (or (session-value env "visits") 0)))
                        (when (string= (getf env :path-info) "/login")
                          ;; what an action does right after authenticating
                          (setf (session-value env "user") "alice")
                          (session-regenerate-id env))
                        (list 200
                              (list :content-type "text/plain")
                              (list (format nil "~D" (session-value env "visits"))))))
        (let* ((client (make-session-client app))
               (first-id (progn (client-request client "/")
                                (set-cookie-value
                                 (session-client-last-set-cookie client)))))
          (ok (string= (response-body-string (client-request client "/")) "2"))
          (let* ((response (client-request client "/login"))
                 (cookie (response-set-cookie response +default-session-cookie-name+))
                 (second-id (set-cookie-value cookie)))
            (ok cookie "renaming the session re-emits the cookie")
            (ok (= (length second-id) 40))
            (ok (string/= second-id first-id)
                "the cookie now carries a different id")
            (ok (not (set-cookie-expired-p cookie))
                "and it is a live cookie, not a deletion")
            (ok (string= (response-body-string response) "3")
                "the contents were carried over, not reset")
            (ok (null (fetch-stored-session store first-id))
                "the old id can no longer be looked up in the store")
            (let ((stored (fetch-stored-session store second-id)))
              (ok (hash-table-p stored) "the session lives under the new id")
              (ok (string= (gethash "user" stored) "alice"))
              (ok (eql (gethash "visits" stored) 3))))
          ;; the client now sends the new id, and the session continues
          (ok (string= (response-body-string (client-request client "/")) "4"))))))

  (testing "a request that does not ask for a rename keeps its id"
    (with-middleware-stack (app :inner (make-session-counter-app))
      (let* ((client (make-session-client app))
             (first-id (progn (client-request client "/")
                              (set-cookie-value
                               (session-client-last-set-cookie client)))))
        (client-request client "/")
        (ok (null (session-client-last-set-cookie client))
            "no Set-Cookie at all on an ordinary continuing request")
        (ok (string= (cdr (first (session-client-jar client))) first-id)
            "so the client keeps sending the same id")))))
