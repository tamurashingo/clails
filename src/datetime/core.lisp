(in-package #:cl-user)
(defpackage #:clails/datetime/core
  (:use #:cl)
  (:import-from #:local-time
                #:timestamp
                #:+utc-zone+)
  (:export #:<datetime>
           #:timestamp
           #:datetime-p))

(in-package #:clails/datetime/core)

;;; <datetime> Class

(defclass <datetime> ()
  ((timestamp :initarg :timestamp
              :reader timestamp
              :type local-time:timestamp
              :documentation "Underlying local-time timestamp"))
  (:documentation "Immutable datetime class wrapping local-time:timestamp"))

(defun datetime-p (object)
  "Check if object is a <datetime> instance.
   
   @param object [t] Object to check
   @return [boolean] T if object is <datetime>, NIL otherwise
   "
  (typep object '<datetime>))

;;; Print Object

(defmethod print-object ((obj <datetime>) stream)
  "Print <datetime> object in ISO 8601 format"
  (print-unreadable-object (obj stream :type t)
    (format stream "~A"
            (local-time:format-timestring nil (timestamp obj)
                                          :format local-time:+iso-8601-format+))))
