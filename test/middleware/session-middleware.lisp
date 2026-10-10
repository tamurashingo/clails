(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-middleware
  (:use #:cl
        #:rove)
  (:import-from #:clails/middleware/session-middleware
                #:*enable-session-middleware*
                #:*session-store*
                #:*session-state*
                #:*lack-middleware-clails-session*))
(in-package #:clails-test/middleware/session-middleware)


(defun make-env (&key (path "/") (cookie nil))
  (let ((headers (make-hash-table :test 'equal)))
    (when cookie
      (setf (gethash "cookie" headers) cookie))
    (list :path-info path
          :request-method :get
          :headers headers)))

(defun extract-cookie-pair (response)
  (let* ((headers (second response))
         (set-cookie (getf headers :set-cookie)))
    (when set-cookie
      (subseq set-cookie 0 (position #\; set-cookie)))))


(deftest session-middleware-disabled-by-default-test
  (testing "*enable-session-middleware* defaults to NIL"
    (ok (null *enable-session-middleware*)))

  (testing "when disabled, the middleware is a pure passthrough (no env mutation, no Set-Cookie)"
    (let* ((*enable-session-middleware* nil)
           (seen-env nil)
           (inner (lambda (env)
                    (setf seen-env env)
                    (list 200 nil (list "hi"))))
           (wrapped (funcall *lack-middleware-clails-session* inner)))
      (let ((response (funcall wrapped (make-env))))
        (ok (equal response (list 200 nil (list "hi"))))
        (ok (null (getf seen-env :lack.session)))))))


(deftest session-middleware-enabled-test
  (testing "when enabled, env is populated with a :lack.session hash-table"
    (let* ((*enable-session-middleware* t)
           (*session-store* nil)
           (*session-state* nil)
           (captured-session nil)
           (inner (lambda (env)
                    (setf captured-session (getf env :lack.session))
                    (list 200 nil (list "hi"))))
           (wrapped (funcall *lack-middleware-clails-session* inner)))
      (funcall wrapped (make-env))
      (ok (hash-table-p captured-session))))

  (testing "session data set on one request is visible on a subsequent request carrying the same cookie"
    (let* ((*enable-session-middleware* t)
           (*session-store* nil)
           (*session-state* nil)
           (captured-session nil)
           (inner (lambda (env)
                    (setf captured-session (getf env :lack.session))
                    (setf (gethash "counter" captured-session)
                          (1+ (or (gethash "counter" captured-session) 0)))
                    (list 200 (list :content-type "text/plain") (list "hi"))))
           (wrapped (funcall *lack-middleware-clails-session* inner)))
      ;; 1st request: no cookie yet
      (let* ((response1 (funcall wrapped (make-env)))
             (cookie (extract-cookie-pair response1)))
        (ok (= (gethash "counter" captured-session) 1))
        (ok cookie)

        ;; 2nd request: send the cookie back, same session must be reused
        (let ((response2 (funcall wrapped (make-env :cookie cookie))))
          (declare (ignore response2))
          (ok (= (gethash "counter" captured-session) 2))))))

  (testing "two independent (cookie-less) requests do not share session state"
    (let* ((*enable-session-middleware* t)
           (*session-store* nil)
           (*session-state* nil)
           (counters nil)
           (inner (lambda (env)
                    (let ((session (getf env :lack.session)))
                      (setf (gethash "counter" session)
                            (1+ (or (gethash "counter" session) 0)))
                      (push (gethash "counter" session) counters))
                    (list 200 nil (list "hi"))))
           (wrapped (funcall *lack-middleware-clails-session* inner)))
      (funcall wrapped (make-env))
      (funcall wrapped (make-env))
      ;; Each request without a cookie gets its own fresh session (counter starts at 1 both times)
      (ok (equal (sort counters #'<) '(1 1))))))
