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
  "Convert KEBAB-CASE to SNAKE_CASE.
   
   Replaces hyphens with underscores.
   
   @param s [symbol] Symbol to convert
   @param s [string] String to convert
   @return [string] Converted string in SNAKE_CASE
   "
  (assert (or (symbolp s)
              (stringp s)))

  (let ((str (if (symbolp s)
                 (string s)
                 s)))
    (ppcre:regex-replace-all "-" str "_")))

(defun snake->kebab (s)
  "Convert SNAKE_CASE to KEBAB-CASE.
   
   Replaces underscores with hyphens.
   
   @param s [symbol] Symbol to convert
   @param s [string] String to convert
   @return [string] Converted string in KEBAB-CASE
   "
  (assert (or (symbolp s)
              (stringp s)))

  (let ((str (if (symbolp s)
                 (string s)
                 s)))
    (ppcre:regex-replace-all "_" str "-")))


(defmacro mandatory-check (&rest args)
  "Verify that all specified arguments are not null.
   
   Asserts that each argument is non-null. If any argument is null,
   signals an error with the argument name.
   
   @param args [symbol] Variable names to check
   @condition error Raised when any argument is null
   "
  `(funcall #'(lambda ()
                ,@(loop for a in args
                        collect `(assert (not (null ,a))
                                         (,a)
                                         "mandatory-check error param: ~A" ',a)))))

(defun env-or-default (env-name default-value)
  "Get environment variable value or return default if not set.
   
   @param env-name [string] Environment variable name
   @param default-value [t] Default value to return if variable is not set
   @return [string] Environment variable value
   @return [t] Default value if environment variable is not set
   "
  (or (uiop:getenv env-name)
      default-value))

(defun env (env-name)
  "Get environment variable value.
   
   @param env-name [string] Environment variable name
   @return [string] Environment variable value, or NIL if not set
   "
  (uiop:getenv env-name))




(defun plist-exists (plist key)
  "Check if a key exists in a property list.
   
   Returns T if the key exists in the plist, regardless of its value.
   Returns NIL if the key is not found.
   
   @param plist [plist] Property list to search
   @param key [symbol] Property indicator to find
   @return [boolean] T if key exists, NIL otherwise
   
   Examples:
   (plist-exists '(:a 1 :b 2 :c :d :e nil) :a) => T
   (plist-exists '(:a 1 :b 2 :c :d :e nil) :e) => T
   (plist-exists '(:a 1 :b 2 :c :d :e nil) :f) => NIL
   "
  (loop for (k . rest) on plist by #'cddr
        when (eq k key)
          return t))


(defun function-from-string (str)
  "Convert a string to a function object.
   
   Parses a string in the format \"package:function-name\" or \"function-name\"
   and returns the corresponding function object.
   
   @param str [string] Function specification string
   @return [function] Function object if found and bound
   @return [nil] NIL if package not found or symbol not bound to a function
   "
  (let* ((parts (ppcre:split #\: str))
         (package-name (if (= (length parts) 2) (first parts) nil))
         (function-name (if (= (length parts) 2) (second parts) (first parts)))
         (package (if package-name (find-package (string-upcase package-name)) *package*)))
    (when package
      (let ((symbol (find-symbol (string-upcase function-name) package)))
        (when (and symbol (fboundp symbol))
          (symbol-function symbol))))))


(defun now ()
  "Get the current universal time.
   
   @return [integer] Current universal time (seconds since 1900-01-01 00:00:00 UTC)
   "
  (get-universal-time))


(defun symbol-from-string (str)
  "Find or intern a symbol from a string.
   
   Parses strings in the following formats:
   - \"package:symbol\" or \"package::symbol\": interns in the specified package
   - \"symbol\": interns in the current package (*package*)
   
   @param str [string] Symbol specification string
   @return [symbol] The interned symbol
   @condition error Signaled if a specified package is not found
   "
  (let ((parts (cl-ppcre:split "::?" str)))
    (let* ((package-name (if (= (length parts) 2) (first parts) nil))
           (symbol-name (if package-name (second parts) (first parts)))
           (package (if package-name
                        (find-package (string-upcase package-name))
                        *package*)))
      (if package
          (intern (string-upcase symbol-name) package)
          (error "package ~S not found." package-name)))))
