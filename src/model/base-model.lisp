(in-package #:cl-user)
(defpackage #:clails/model/base-model
  (:use #:cl)
  (:export #:<base-model>))
(in-package #:clails/model/base-model)

(defclass <base-model> ()
  ((data :initform (make-hash-table))
   (save-p :initform nil)))

(defmethod initialize-instance :after ((m <base-model>) &rest initargs)
  (declare (ignorable initargs))
  ;; make base field
  (setf (gethash :id (slot-value m 'data)) nil)
  (setf (gethash :created-at (slot-value m 'data)) nil)
  (setf (gethash :updated-at (slot-value m 'data)) nil))

(defmethod ref ((inst <base-model>) key)
  (multiple-value-bind (value present-p)
      (gethash key (slot-value inst 'data))
    (when (not present-p)
      (error "not found slot name: ~A" key))
    value))

(defun (setf ref) (new-value inst key)
  (multiple-value-bind (value present-p)
      (gethash key (slot-value inst 'data))
    (declare (ignorable value))
    (when (not present-p)
      (error "not found slot name: ~A" key)))
  (setf (gethash key (slot-value inst 'data))
        new-value))

(defun show-model (model)
  (maphash #'(lambda (key value)
               (format t "~A:~A~%" key value))
           (slot-value model 'data)))


;;
;; (defclass <todo> (<base-model>) ())
;; (defmethod initialize-instance :after ((m <todo>) &rest initargs)
;;   ;; make field
;;   (setf (gethash :title (slot-value m 'data)) nil)
;;   (setf (gethash :done (slot-value m 'data)) nil))
;; (defvar todo (make-instance '<todo>))
;;
;; (setf (ref todo :title) "refactor all products")
;; (show-model todo)
;;
;; ID:NIL
;; CREATED-AT:NIL
;; UPDATED-AT:NIL
;; TITLE:refactor all products
;; DONE:NIL
