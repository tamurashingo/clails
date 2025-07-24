(in-package #:cl-user)
(defpackage #:clails/condition
  (:use #:cl))

(in-package #:clails/condition)

(define-condition http-error (simple-error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)
   (path :initarg :path :reader path))
  (:report (lambda (c s)
             (format s "~A ~A: ~A" code message path))))


(define-condition 400/bad-request (http-error)
  ()
  (:default-initargs
   :code 400
   :message "Bad Request"))

(define-condition 401/unauthorized (http-error)
  ()
  (:default-initargs
   :code 401
   :messsage "Unauthorized"))


(define-condition 403/forbidden (http-error)
  ()
  (:default-initargs
   :codee 403
   :message "Forbidden"))

(define-condition 404/not-found (http-error)
  ()
  (:default-initargs
   :code 404
   :message "Not Found"))

(define-condition 405/method-not-allowed (http-error)
  ()
  (:default-initargs
   :code 405
   :message "Method Not Allowed"))



(define-condition 500/internal-server-error (http-error)
  ()
  (:default-initargs
   :code 500
   :message "Internal Server Error"))
