(in-package #:cl-user)

(defpackage #:clails/test/runner
  (:use #:cl)
  (:import-from #:clails/test/registry
                #:*active-tags*
                #:*excluded-tags*
                #:*active-packages*
                #:get-tests-by-tag
                #:get-tests-by-package
                #:get-all-tags
                #:get-all-packages)
  (:import-from #:rove
                #:run-tests)
  (:export #:run-suite-tests
           #:run-suite-tests-by-tags
           #:run-suite-tests-by-packages
           #:list-all-tags
           #:list-test-packages
           #:list-tests-by-tag
           #:list-tests-by-package))

(in-package #:clails/test/runner)

(defun filter-tests-by-tags (all-tests)
  "Filter tests by active and excluded tags.
   
   @param all-tests [list] List of all test names
   @return [list] Filtered list of test names
   "
  (let ((filtered-tests all-tests))
    ;; Apply active tags filter
    (when *active-tags*
      (setf filtered-tests
            (remove-duplicates
             (loop for tag in *active-tags*
                   append (get-tests-by-tag tag)))))
    
    ;; Apply excluded tags filter
    (when *excluded-tags*
      (let ((excluded-tests (remove-duplicates
                             (loop for tag in *excluded-tags*
                                   append (get-tests-by-tag tag)))))
        (setf filtered-tests
              (set-difference filtered-tests excluded-tests))))
    
    filtered-tests))

(defun filter-tests-by-packages (all-tests)
  "Filter tests by active packages.
   
   @param all-tests [list] List of all test names
   @return [list] Filtered list of test names
   "
  (if *active-packages*
      (remove-duplicates
       (loop for pkg in *active-packages*
             append (get-tests-by-package pkg)))
      all-tests))

(defun get-filtered-tests ()
  "Get tests filtered by both tags and packages.
   
   @return [list] Filtered list of test names
   "
  (let* ((all-tests (remove-duplicates
                     (loop for pkg in (get-all-packages)
                           append (get-tests-by-package pkg))))
         (tests-by-tags (filter-tests-by-tags all-tests))
         (tests-by-packages (filter-tests-by-packages all-tests)))
    ;; Determine which filtered list to use
    (cond
      ;; Both tags and packages specified
      ((and (or *active-tags* *excluded-tags*) *active-packages*)
       (intersection tests-by-tags tests-by-packages))
      ;; Only tags specified (including excluded tags)
      ((or *active-tags* *excluded-tags*)
       tests-by-tags)
      ;; Only packages specified
      (*active-packages*
       tests-by-packages)
      ;; Nothing specified, return all
      (t
       all-tests))))

(defun run-suite-tests (&key tags excluded-tags packages (style :spec))
  "Run tests with optional filtering.
   
   @param tags [list] Tags to include
   @param excluded-tags [list] Tags to exclude
   @param packages [list] Packages to include (exact match)
   @param style [keyword] Reporter style (default: :spec)
   @return [boolean] True if all tests passed
   "
  (let ((*active-tags* tags)
        (*excluded-tags* excluded-tags)
        (*active-packages* packages)
        (failed-package-tests nil))
    (if (or tags excluded-tags packages)
        (let ((filtered-tests (get-filtered-tests)))
          (if filtered-tests
              (progn
                (format t "~&Running ~D test(s)...~%" (length filtered-tests))
                ;; Group tests by package and run each group
                (if packages
                    ;; When packages are specified, run tests package by package
                    (let ((all-passed t))
                      (dolist (pkg packages)
                        (let ((pkg-tests (intersection filtered-tests 
                                                       (get-tests-by-package pkg))))
                          (when pkg-tests
                            (format t "~%;; testing '~A'~%" pkg)
                            ;; Record failed test names
                            (let ((result (rove:run-tests pkg-tests :style style)))
                              (unless result
                                (setf all-passed nil)
                                ;; Simply record that this package had failures
                                (push (cons pkg pkg-tests) failed-package-tests))))))
                      ;; Print summary of failed tests
                      (when failed-package-tests
                        (format t "~%~%=== Failed Tests Summary ===~%")
                        (dolist (entry (reverse failed-package-tests))
                          (let ((pkg (car entry)))
                            (format t "~%Package: ~A~%" pkg))))
                      all-passed)
                    ;; When filtering by tags only, run all at once
                    (let ((all-passed t))
                      ;; Group filtered tests by package
                      (let ((pkg-test-map (make-hash-table :test 'equal)))
                        ;; Build package to tests mapping
                        (dolist (test filtered-tests)
                          (dolist (pkg (get-all-packages))
                            (when (member test (get-tests-by-package pkg))
                              (push test (gethash pkg pkg-test-map)))))
                        ;; Run tests package by package
                        (maphash (lambda (pkg tests)
                                   (when tests
                                     (format t "~%;; testing '~A'~%" pkg)
                                     (let ((result (rove:run-tests (nreverse tests) :style style)))
                                       (unless result
                                         (setf all-passed nil)
                                         (push (cons pkg tests) failed-package-tests)))))
                                 pkg-test-map))
                      ;; Print summary of failed tests
                      (when failed-package-tests
                        (format t "~%~%=== Failed Tests Summary ===~%")
                        (dolist (entry (reverse failed-package-tests))
                          (let ((pkg (car entry)))
                            (format t "~%Package: ~A~%" pkg))))
                      all-passed)))
              (progn
                (format t "~&No tests matched the filter criteria.~%")
                nil)))
        ;; Run all tests, grouped by package
        (progn
          (format t "~&Running all tests...~%")
          (let* ((all-packages (get-all-packages))
                 (all-passed t))
            (if all-packages
                (progn
                  (dolist (pkg all-packages)
                    (let ((pkg-tests (get-tests-by-package pkg)))
                      (when pkg-tests
                        (format t "~%;; testing '~A'~%" pkg)
                        ;; Record failed test names
                        (let ((result (rove:run-tests pkg-tests :style style)))
                          (unless result
                            (setf all-passed nil)
                            ;; Simply record that this package had failures
                            (push (cons pkg pkg-tests) failed-package-tests))))))
                  ;; Print summary of failed tests
                  (when failed-package-tests
                    (format t "~%~%=== Failed Tests Summary ===~%")
                    (dolist (entry (reverse failed-package-tests))
                      (let ((pkg (car entry)))
                        (format t "~%Package: ~A~%" pkg))))
                  all-passed)
                (progn
                  (format t "~&No tests found.~%")
                  nil)))))))

(defun run-suite-tests-by-tags (&rest tags)
  "Run tests that have any of the specified tags.
   
   @param tags [keyword*] Tags to filter by
   @return [t] Test results
   "
  (run-suite-tests :tags tags))

(defun run-suite-tests-by-packages (&rest packages)
  "Run tests in the specified packages.
   
   @param packages [string*] Package names to filter by
   @return [t] Test results
   "
  (run-suite-tests :packages packages))

(defun list-all-tags ()
  "List all available tags.
   
   @return [list] List of all tags
   "
  (let ((tags (get-all-tags)))
    (format t "~&Available tags (~D):~%" (length tags))
    (dolist (tag (sort (copy-list tags) #'string< :key #'symbol-name))
      (format t "  ~A (~D test(s))~%"
              tag
              (length (get-tests-by-tag tag))))
    tags))

(defun list-test-packages ()
  "List all available packages with tests.
   
   @return [list] List of all package names
   "
  (let ((packages (get-all-packages)))
    (format t "~&Available packages (~D):~%" (length packages))
    (dolist (pkg (sort (copy-list packages) #'string<))
      (format t "  ~A (~D test(s))~%"
              pkg
              (length (get-tests-by-package pkg))))
    packages))

(defun list-tests-by-tag (tag)
  "List all tests with a specific tag.
   
   @param tag [keyword] Tag to search for
   @return [list] List of test names
   "
  (let ((tests (get-tests-by-tag tag)))
    (if tests
        (progn
          (format t "~&Tests with tag ~A (~D):~%" tag (length tests))
          (dolist (test tests)
            (format t "  - ~A~%" test)))
        (format t "~&No tests found with tag ~A~%" tag))
    tests))

(defun list-tests-by-package (package-name)
  "List all tests in a specific package.
   
   @param package-name [string] Package name
   @return [list] List of test names
   "
  (let ((tests (get-tests-by-package package-name)))
    (if tests
        (progn
          (format t "~&Tests in package ~A (~D):~%" package-name (length tests))
          (dolist (test tests)
            (format t "  - ~A~%" test)))
        (format t "~&No tests found in package ~A~%" package-name))
    tests))
