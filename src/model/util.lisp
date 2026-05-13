(in-package #:cl-user)
(defpackage #:clails/model/util
  (:use #:cl)
  (:export #:get-cl-db-fn-by-type))
(in-package #:clails/model/util)


(defun get-cl-db-fn-by-type (type-convert-functions type)
  "Get cl-db-fn function for the specified type from type conversion functions.
   
   @param type-convert-functions [list] Type conversion functions alist
   @param type [keyword] Type keyword (e.g., :string, :integer, :boolean)
   @return [function] Conversion function, or identity if not found
   "
  (let ((fn (loop for (db-type . props) in type-convert-functions
                  when (eq (getf props :type) type)
                  return (getf props :cl-db-fn))))
    (or fn #'identity)))
