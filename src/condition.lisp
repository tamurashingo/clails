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
  ((code :initarg :code :reader code
         :documentation "HTTP status code")
   (message :initarg :message :reader message
            :documentation "Error message")
   (template-name :initarg :template-name :reader template-name
                  :documentation "Error template file path")
   (path :initarg :path :reader path
         :documentation "Request path that caused the error"))
  (:report (lambda (c s)
             (format s "~A ~A: ~A" (code c) (message c) (path c))))
  (:documentation "Base condition class for HTTP errors."))


(define-condition 400/bad-request (http-error)
  ()
  (:default-initargs
   :code 400
   :template-name "public/error.html"
   :message "Bad Request")
  (:documentation "HTTP 400 Bad Request error condition."))

(define-condition 401/unauthorized (http-error)
  ()
  (:default-initargs
   :code 401
   :template-name "public/error.html"
   :messsage "Unauthorized")
  (:documentation "HTTP 401 Unauthorized error condition."))


(define-condition 403/forbidden (http-error)
  ()
  (:default-initargs
   :codee 403
   :template-name "public/error.html"
   :message "Forbidden")
  (:documentation "HTTP 403 Forbidden error condition."))

(define-condition 404/not-found (http-error)
  ()
  (:default-initargs
   :code 404
   :template-name "public/404.html"
   :message "Not Found")
  (:documentation "HTTP 404 Not Found error condition."))

(define-condition 405/method-not-allowed (http-error)
  ()
  (:default-initargs
   :code 405
   :template-name "public/error.html"
   :message "Method Not Allowed")
  (:documentation "HTTP 405 Method Not Allowed error condition."))



(define-condition 500/internal-server-error (http-error)
  ()
  (:default-initargs
   :code 500
   :template-name "public/500.html"
   :message "Internal Server Error")
  (:documentation "HTTP 500 Internal Server Error condition."))


(define-condition optimistic-lock-error (simple-error)
  ()
  (:documentation "Error condition raised when an optimistic lock conflict occurs during database update."))
