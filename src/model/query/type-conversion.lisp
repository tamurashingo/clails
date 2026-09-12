(in-package #:cl-user)
(defpackage #:clails/model/query/type-conversion
  (:use #:cl)
  (:import-from #:clails/environment
                #:*database-type*)
  (:export #:to-string-impl
           #:to-text-impl
           #:to-integer-impl
           #:to-float-impl
           #:to-decimal-impl
           #:to-datetime-impl
           #:to-date-impl
           #:to-time-impl
           #:to-boolean-impl
           #:to-string
           #:to-text
           #:to-integer
           #:to-float
           #:to-decimal
           #:to-datetime
           #:to-date
           #:to-time
           #:to-boolean))
(in-package #:clails/model/query/type-conversion)

;;;; ----------------------------------------
;;;; type conversion functions
;;;;
;;;; Database-specific conversion is implemented via the *-impl generic
;;;; functions below. Each database backend (see src/model/impl/*.lisp)
;;;; provides methods specialized on its <database-type-*> class.

(defgeneric to-string-impl (database value)
  (:documentation "Convert value to string type for the specified database."))

(defgeneric to-text-impl (database value)
  (:documentation "Convert value to text type for the specified database."))

(defgeneric to-integer-impl (database value)
  (:documentation "Convert value to integer type for the specified database."))

(defgeneric to-float-impl (database value)
  (:documentation "Convert value to floating-point type for the specified database."))

(defgeneric to-decimal-impl (database value)
  (:documentation "Convert value to decimal type for the specified database."))

(defgeneric to-datetime-impl (database value)
  (:documentation "Convert value to datetime type for the specified database."))

(defgeneric to-date-impl (database value)
  (:documentation "Convert value to date type for the specified database."))

(defgeneric to-time-impl (database value)
  (:documentation "Convert value to time type for the specified database."))

(defgeneric to-boolean-impl (database value)
  (:documentation "Convert value to boolean type for the specified database."))


(defun to-string (v &optional (database *database-type*))
  "Convert value to string type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-string-impl database v))

(defun to-text (v &optional (database *database-type*))
  "Convert value to text type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-text-impl database v))

(defun to-integer (v &optional (database *database-type*))
  "Convert value to integer type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-integer-impl database v))

(defun to-float (v &optional (database *database-type*))
  "Convert value to floating-point type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-float-impl database v))

(defun to-decimal (v &optional (database *database-type*))
  "Convert value to decimal type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-decimal-impl database v))

(defun to-datetime (v &optional (database *database-type*))
  "Convert value to datetime type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-datetime-impl database v))

(defun to-date (v &optional (database *database-type*))
  "Convert value to date type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-date-impl database v))

(defun to-time (v &optional (database *database-type*))
  "Convert value to time type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-time-impl database v))

(defun to-boolean (v &optional (database *database-type*))
  "Convert value to boolean type for the specified database.

   @param v [t] Value to convert
   @param database [t] Database type instance (optional, defaults to *database-type*)
   @return [t] Converted value
   "
  (to-boolean-impl database v))
