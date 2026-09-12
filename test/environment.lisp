(in-package #:cl-user)
(defpackage #:clails-test/environment
  (:use #:cl
        #:rove)
  (:import-from #:clails/environment
                #:clails-framework-version
                #:warn-if-framework-version-mismatch))
(in-package #:clails-test/environment)

;;; Test: clails-framework-version

(deftest clails-framework-version-test
  (testing "clails-framework-version returns the version recorded in clails.asd"
    (ok (string= (clails-framework-version)
                 (asdf:component-version (asdf:find-system :clails)))
        "should match asdf:component-version for the :clails system")))

;;; Test: warn-if-framework-version-mismatch

(deftest warn-if-framework-version-mismatch-matching-test
  (testing "prints nothing and returns NIL when versions match"
    (let* ((current (clails-framework-version))
           (output (with-output-to-string (*error-output*)
                     (ok (null (warn-if-framework-version-mismatch current))
                         "should return NIL when versions match"))))
      (ok (string= output "")
          "should not print a warning when versions match"))))

(deftest warn-if-framework-version-mismatch-differing-test
  (testing "prints a warning and returns T when versions differ"
    (let* ((current (clails-framework-version))
           (bogus-version (format nil "~A-does-not-exist" current))
           (output (with-output-to-string (*error-output*)
                     (ok (eq t (warn-if-framework-version-mismatch bogus-version))
                         "should return T when versions differ"))))
      (ok (search "WARNING" output)
          "should print a WARNING to *error-output*")
      (ok (search bogus-version output)
          "warning should mention the generated (old) version")
      (ok (search current output)
          "warning should mention the currently running version"))))
