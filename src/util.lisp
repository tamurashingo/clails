(in-package #:cl-user)
(defpackage #:clails/util
  (:use #:cl))
(in-package #:clails/util)

(defun kebab->snake (s)
  "convert KEBAB-CASE to SNAKE_CASE"
  (assert (or (and (symbolp s)
                   (not (keywordp s)))
              (stringp s)))

  (let ((str (if (symbolp s)
                 (string s)
                 s)))
    (ppcre:regex-replace-all "-" str "_")))


;;
;; check args are not null
;;
;; sample
;; (defun foo (name value)
;;   (mandatory-check name value)
;;   (format t "~A is ~A" name value))
;;
(defmacro mandatory-check (&rest args)
  `(funcall #'(lambda ()
                ,@(loop for a in args
                        collect `(assert (not (null ,a))
                                         (,a)
                                         "mandatory-check error param: ~A" ',a)))))

