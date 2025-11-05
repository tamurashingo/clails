(in-package #:cl-user)

(defpackage #:clails/test/registry
  (:use #:cl)
  (:export #:*test-suite-mapping*
           #:*test-package-mapping*
           #:*active-tags*
           #:*excluded-tags*
           #:*active-packages*
           #:register-test-with-tags
           #:register-test-with-package
           #:get-tests-by-tag
           #:get-tests-by-package
           #:get-all-tags
           #:get-all-packages
           #:clear-registry))

(in-package #:clails/test/registry)

(defvar *test-suite-mapping* (make-hash-table)
  "Mapping from tag to list of test names.
   key: keyword (tag)
   value: list of test names having this tag")

(defvar *test-package-mapping* (make-hash-table :test 'equal)
  "Mapping from package name to list of test names.
   key: string (package name)
   value: list of test names in this package")

(defvar *active-tags* nil
  "List of tags to include in test execution. nil means all tests.")

(defvar *excluded-tags* nil
  "List of tags to exclude from test execution.")

(defvar *active-packages* nil
  "List of packages to include in test execution (exact match). nil means all packages.")

(defun register-test-with-tags (test-name tags)
  "Register a test with its tags.
   
   @param test-name [symbol] Test name
   @param tags [list] List of tags (keywords)
   "
  (dolist (tag tags)
    (pushnew test-name (gethash tag *test-suite-mapping*))))

(defun register-test-with-package (test-name package-name)
  "Register a test with its package.
   
   @param test-name [symbol] Test name
   @param package-name [string] Package name
   "
  (pushnew test-name (gethash package-name *test-package-mapping*)))

(defun get-tests-by-tag (tag)
  "Get all tests with a specific tag.
   
   @param tag [keyword] Tag to search for
   @return [list] List of test names
   "
  (gethash tag *test-suite-mapping*))

(defun get-tests-by-package (package-name)
  "Get all tests in a specific package.
   
   @param package-name [string] Package name
   @return [list] List of test names
   "
  (gethash package-name *test-package-mapping*))

(defun get-all-tags ()
  "Get all registered tags.
   
   @return [list] List of all tags
   "
  (loop for tag being the hash-keys of *test-suite-mapping*
        collect tag))

(defun get-all-packages ()
  "Get all registered packages.
   
   @return [list] List of all package names
   "
  (loop for pkg being the hash-keys of *test-package-mapping*
        collect pkg))

(defun clear-registry ()
  "Clear all registry information."
  (clrhash *test-suite-mapping*)
  (clrhash *test-package-mapping*)
  (setf *active-tags* nil)
  (setf *excluded-tags* nil)
  (setf *active-packages* nil))
