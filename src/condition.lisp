(in-package #:cl-user)
(defpackage #:clails/condition
  (:use #:cl)
  (:export #:400/bad-request
           #:401/unauthorized
           #:403/forbidden
           #:404/not-found
           #:405/method-not-allowed
           #:500/internal-server-error
           #:optimistic-lock-error
           #:code
           #:message
           #:template-name
           #:path))

(in-package #:clails/condition)

(define-condition http-error (simple-error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)
   (template-name :initarg :template-name :reader template-name)
   (path :initarg :path :reader path))
  (:report (lambda (c s)
             (format s "~A ~A: ~A" (code c) (message c) (path c)))))


(define-condition 400/bad-request (http-error)
  ()
  (:default-initargs
   :code 400
   :template-name "public/error.html"
   :message "Bad Request"))

(define-condition 401/unauthorized (http-error)
  ()
  (:default-initargs
   :code 401
   :template-name "public/error.html"
   :messsage "Unauthorized"))


(define-condition 403/forbidden (http-error)
  ()
  (:default-initargs
   :codee 403
   :template-name "public/error.html"
   :message "Forbidden"))

(define-condition 404/not-found (http-error)
  ()
  (:default-initargs
   :code 404
   :template-name "public/404.html"
   :message "Not Found"))

(define-condition 405/method-not-allowed (http-error)
  ()
  (:default-initargs
   :code 405
   :template-name "public/error.html"
   :message "Method Not Allowed"))



(define-condition 500/internal-server-error (http-error)
  ()
  (:default-initargs
   :code 500
   :template-name "public/500.html"
   :message "Internal Server Error"))


(define-condition optimistic-lock-error (simple-error)
  ())
