(in-package #:cl-user)
(defpackage #:clails/util
  (:use #:cl)
  (:export #:kebab->snake
           #:mandatory-check
           #:env-or-default
           #:plist-exists
           #:function-from-string
           #:now))
(in-package #:clails/util)

(defun kebab->snake (s)
  "convert KEBAB-CASE to SNAKE_CASE"
  (assert (or (symbolp s)
              (stringp s)))

  (let ((str (if (symbolp s)
                 (string s)
                 s)))
    (ppcre:regex-replace-all "-" str "_")))

(defun snake->kebab (s)
  "convert SNAKE_CASE to KEBAB-CASE"
  (assert (or (symbolp s)
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

(defun env (env-name)
  (uiop:getenv env-name))




(defun plist-exists (plist key)
  "#### Syntax:

**plist-exists** plist key => result

#### Arguments and values:

*plist* -> property list \
*key* -> property indicator\
*result* -> boolean

#### Description:

Return t if key exists in plist. otherwise, returns nil.


#### Example:

(plist-exists '(:a 1 :b 2 :c :d :e nil) :a) => t \
(plist-exists '(:a 1 :b 2 :c :d :e nil) :e) => t


(plist-exists '(:a 1 :b 2 :c :d :e nil) :f) => nil

"
  (loop for (k . rest) on plist by #'cddr
        when (eq k key)
          return t))


(defun function-from-string (str)
  (let* ((parts (ppcre:split #\: str))
         (package-name (if (= (length parts) 2) (first parts) nil))
         (function-name (if (= (length parts) 2) (second parts) (first parts)))
         (package (if package-name (find-package (string-upcase package-name)) *package*)))
    (when package
      (let ((symbol (find-symbol (string-upcase function-name) package)))
        (when (and symbol (fboundp symbol))
          (symbol-function symbol))))))


(defun now ()
  (get-universal-time))


(defun symbol-from-string (str)
  "finds or interns a symbol from a string.
- \"package:symbol\" or \"package::symbol\": interns in the specified package.
- \"symbol\": interns in the current package (*package*).
returns the symbol. signals an error if a specified package is not found."
  (let ((parts (cl-ppcre:split "::?" str)))
    (let* ((package-name (if (= (length parts) 2) (first parts) nil))
           (symbol-name (if package-name (second parts) (first parts)))
           (package (if package-name
                        (find-package (string-upcase package-name))
                        *package*)))
      (if package
          (intern (string-upcase symbol-name) package)
          (error "package ~S not found." package-name)))))
