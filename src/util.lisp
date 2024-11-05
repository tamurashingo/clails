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

