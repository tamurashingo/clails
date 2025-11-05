(in-package #:cl-user)

(defpackage #:clails/test
  (:use #:cl)
  (:import-from #:clails/test/registry
                #:*test-suite-mapping*
                #:*test-package-mapping*
                #:*active-tags*
                #:*excluded-tags*
                #:*active-packages*
                #:get-tests-by-tag
                #:get-tests-by-package
                #:get-all-tags
                #:get-all-packages
                #:clear-registry)
  (:import-from #:clails/test/suite
                #:deftest-suite)
  (:import-from #:clails/test/runner
                #:run-suite-tests
                #:run-suite-tests-by-tags
                #:run-suite-tests-by-packages
                #:list-all-tags
                #:list-test-packages
                #:list-tests-by-tag
                #:list-tests-by-package)
  (:import-from #:clails/test/loader
                #:*test-modules-loaded*
                #:ensure-test-modules-loaded)
  (:export ;; Registry
           #:*test-suite-mapping*
           #:*test-package-mapping*
           #:*active-tags*
           #:*excluded-tags*
           #:*active-packages*
           #:get-tests-by-tag
           #:get-tests-by-package
           #:get-all-tags
           #:get-all-packages
           #:clear-registry
           ;; Suite
           #:deftest-suite
           ;; Runner
           #:run-suite-tests
           #:run-suite-tests-by-tags
           #:run-suite-tests-by-packages
           #:list-all-tags
           #:list-test-packages
           #:list-tests-by-tag
           #:list-tests-by-package
           ;; Loader
           #:*test-modules-loaded*
           #:ensure-test-modules-loaded))

(in-package #:clails/test)
