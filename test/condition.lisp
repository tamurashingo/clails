(in-package #:cl-user)
(defpackage #:clails-test/condition
  (:use #:cl
        #:rove)
  (:import-from #:clails/condition
                #:400/bad-request
                #:401/unauthorized
                #:403/forbidden
                #:404/not-found
                #:405/method-not-allowed
                #:500/internal-server-error
                #:code
                #:message
                #:template-name
                #:path))
(in-package #:clails-test/condition)


;;; The HTTP error conditions supply their status code, message and error
;;; template through :default-initargs.  A misspelled initarg name is accepted
;;; silently by DEFINE-CONDITION -- it just never reaches a slot -- so the
;;; mistake only surfaces much later, when whoever turns the condition into a
;;; response reads the slot and gets an UNBOUND-SLOT error instead of a
;;; status code.  :code on 403/forbidden and :message on 401/unauthorized
;;; were misspelled that way (":codee", ":messsage") until M2.
;;;
;;; Every slot of every condition class is therefore asserted below, so that
;;; the next typo fails here rather than in a running application.

(defparameter +expected-conditions+
  '((400/bad-request         400 "Bad Request"           "public/error.html")
    (401/unauthorized        401 "Unauthorized"          "public/error.html")
    (403/forbidden           403 "Forbidden"             "public/error.html")
    (404/not-found           404 "Not Found"             "public/404.html")
    (405/method-not-allowed  405 "Method Not Allowed"    "public/error.html")
    (500/internal-server-error 500 "Internal Server Error" "public/500.html"))
  "(condition-class status-code message template-name) for every HTTP error
   condition clails defines.")

(defun slot-or-unbound (reader condition)
  "Read CONDITION through READER, answering :UNBOUND when the slot behind it
   has no value.

   An unbound slot would otherwise abort the whole test instead of failing
   one assertion, and the point of these tests is to say exactly which slot
   of which condition class is missing."
  (handler-case (funcall reader condition)
    (unbound-slot () :unbound)
    (error () :unbound)))


(deftest http-error-default-initargs-test
  (testing "every condition class fills in status code, message and template"
    (dolist (expected +expected-conditions+)
      (destructuring-bind (class status message-text template) expected
        (let ((c (make-condition class :path "/somewhere")))
          (ok (eql (slot-or-unbound #'code c) status)
              (format nil "~A reports status code ~A" class status))
          (ok (equal (slot-or-unbound #'message c) message-text)
              (format nil "~A reports the message ~S" class message-text))
          (ok (equal (slot-or-unbound #'template-name c) template)
              (format nil "~A reports the template ~S" class template))
          (ok (equal (slot-or-unbound #'path c) "/somewhere")
              (format nil "~A carries the path it was signalled with" class))))))

  (testing "403/forbidden reports status code 403 (the initarg M2 fixed)"
    (let ((c (make-condition '403/forbidden :path "/admin")))
      (ok (eql (code c) 403))
      (ok (equal (message c) "Forbidden"))))

  (testing "401/unauthorized reports its message (the initarg M2 fixed)"
    (let ((c (make-condition '401/unauthorized :path "/admin")))
      (ok (equal (message c) "Unauthorized"))
      (ok (eql (code c) 401))))

  (testing "the report function, which reads all three slots, prints"
    ;; This is what used to fail: not the signalling, but the printing /
    ;; response building that reads the slots afterwards.
    (ok (equal (princ-to-string (make-condition '403/forbidden :path "/admin"))
               "403 Forbidden: /admin"))
    (ok (equal (princ-to-string (make-condition '401/unauthorized :path "/admin"))
               "401 Unauthorized: /admin")))

  (testing "an explicit initarg still wins over the default"
    (let ((c (make-condition '403/forbidden
                             :path "/admin"
                             :message "Forbidden: no token"
                             :template-name "public/403.html")))
      (ok (eql (code c) 403))
      (ok (equal (message c) "Forbidden: no token"))
      (ok (equal (template-name c) "public/403.html")))))


(deftest http-error-signalling-test
  (testing "the conditions can be signalled and caught by class"
    ;; 404/not-found is the one condition class the framework already relies
    ;; on: clails/middleware/clails-middleware signals it for an unknown
    ;; action and catches it to delegate to the next application.  This is
    ;; here so that a change to the condition definitions cannot quietly
    ;; break that path.
    (ok (eql 404
             (handler-case (error '404/not-found :path "/nope")
               (404/not-found (c) (code c)))))
    (ok (eql 403
             (handler-case (error '403/forbidden :path "/nope")
               (403/forbidden (c) (code c)))))
    (ok (equal "Unauthorized"
               (handler-case (error '401/unauthorized :path "/nope")
                 (401/unauthorized (c) (message c)))))))
