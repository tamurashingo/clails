(in-package #:cl-user)

(defpackage #:clails/test/suite
  (:use #:cl)
  (:import-from #:clails/test/registry
                #:register-test-with-tags
                #:register-test-with-package)
  (:export #:deftest-suite))

(in-package #:clails/test/suite)

(defmacro deftest-suite (tags test-name &body body)
  "Define a test with tags.
   
   @param tags [keyword or list] Tags to assign to the test. Single keyword or list of keywords
   @param test-name [symbol] Test name
   @param body [form*] Test body
   "
  (let ((tag-list (if (listp tags) tags (list tags))))
    `(progn
       ;; Register tag information
       (register-test-with-tags ',test-name ',tag-list)
       ;; Register package information
       (register-test-with-package ',test-name ,(package-name *package*))
       ;; Define normal deftest
       (rove:deftest ,test-name
         ,@body))))
