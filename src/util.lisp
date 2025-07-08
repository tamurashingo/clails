(in-package #:cl-user)
(defpackage #:clails/util
  (:use #:cl)
  (:export #:kebab->snake
           #:mandatory-check
           #:env-or-default))
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

(defun snake->kebab (s)
  "convert SNAKE_CASE to KEBAB-CASE"
  (assert (or (and (symbolp s)
                   (not (keywordp s)))
              (stringp s)))

  (let ((str (if (symbolp s)
                 (string s)
                 s)))
    (ppcre:regex-replace-all "_" str "-")))



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

(defun env-or-default (env-name default-value)
  (or (uiop:getenv env-name)
      default-value))



(defun plist-values (plist)
  "#### Syntax:

**plist-values** plist => result

#### Arguments and values:

*plist* -> property list \
*result* -> list

#### Description:

Returns only the value of plist.


#### Example:

(plist-avlues '(:a 1 :b 2: :c :d)) => (1 2 :d)
"
  (loop for (key . rest) on plist by #'cddr
        collect (car rest)))
