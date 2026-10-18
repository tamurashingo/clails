(in-package #:cl-user)
(defpackage #:clails-test/project/generate
  (:use #:cl
        #:rove)
  (:import-from #:clails/project/generate
                #:gen/model
                #:check-unregistered-models)
  (:import-from #:clails-test/support/temp-project
                #:make-test-project-root
                #:delete-test-project-root))
(in-package #:clails-test/project/generate)

(defvar *test-project-root* nil
  "This suite's own directory under the system temporary directory, used as
   CLAILS/ENVIRONMENT:*PROJECT-DIR*.  Scoped to the suite so that TEARDOWN
   deletes only what the suite created.")

(defparameter *test-project-name* "testproj")

(setup
  (setf *test-project-root* (make-test-project-root))
  (setf clails/environment:*project-dir* *test-project-root*)
  (setf clails/environment:*project-name* *test-project-name*)
  (ensure-directories-exist (merge-pathnames "app/models/" *test-project-root*))
  (ensure-directories-exist (merge-pathnames "test/models/" *test-project-root*))
  (with-open-file (out (merge-pathnames "app/application-loader.lisp" *test-project-root*)
                       :direction :output :if-exists :supersede)
    (format out "; -*- mode: lisp -*-~%(in-package #:cl-user)~%(defpackage #:~A/application-loader~%  (:use #:cl)~%  (:import-from #:~A/models/package))~%(in-package #:~A/application-loader)~%"
            *test-project-name* *test-project-name* *test-project-name*))
  (with-open-file (out (merge-pathnames "app/models/package.lisp" *test-project-root*)
                       :direction :output :if-exists :supersede)
    (format out "; -*- mode: lisp -*-~%(in-package #:cl-user)~%(defpackage #:~A/models/package~%  (:use #:cl))~%(in-package #:~A/models/package)~%"
            *test-project-name* *test-project-name*))
  (with-open-file (out (merge-pathnames "test/test-loader.lisp" *test-project-root*)
                       :direction :output :if-exists :supersede)
    (format out "; -*- mode: lisp -*-~%(in-package #:cl-user)~%(defpackage #:~A-test/test-loader~%  (:use #:cl))~%(in-package #:~A-test/test-loader)~%"
            *test-project-name* *test-project-name*)))

(teardown
  (delete-test-project-root *test-project-root*))

(defun read-project-file (relative-path)
  (uiop:read-file-string (merge-pathnames relative-path *test-project-root*)))


;;; Test: gen/model registers into app/models/package.lisp, not application-loader.lisp

(deftest gen-model-registers-in-models-package-test
  (testing "gen/model adds the new model to app/models/package.lisp and leaves application-loader.lisp alone"
    (let ((loader-before (read-project-file "app/application-loader.lisp")))
      (gen/model "widget")
      (let ((models-package (read-project-file "app/models/package.lisp"))
            (loader-after (read-project-file "app/application-loader.lisp")))
        (ok (search "testproj/models/widget" models-package)
            "app/models/package.lisp should import the new model's package")
        (ok (search "#:testproj/models/widget" models-package)
            "the import should be printed with the #: uninterned-symbol reader syntax")
        (ok (string= loader-before loader-after)
            "app/application-loader.lisp should not be modified by gen/model")))))


;;; Test: check-unregistered-models

(deftest check-unregistered-models-warns-for-unloaded-file-test
  (testing "check-unregistered-models warns about a model file whose package was never loaded"
    (with-open-file (out (merge-pathnames "app/models/foo.lisp" *test-project-root*)
                         :direction :output :if-exists :supersede)
      (format out "; a hand-written model file, never registered anywhere~%"))
    (let ((output (with-output-to-string (*standard-output*)
                    (check-unregistered-models))))
      (ok (search "foo" output)
          "warning output should mention the unregistered model file"))))

(deftest check-unregistered-models-silent-for-loaded-file-test
  (testing "check-unregistered-models stays silent about a model file whose package is already loaded"
    (with-open-file (out (merge-pathnames "app/models/bar.lisp" *test-project-root*)
                         :direction :output :if-exists :supersede)
      (format out "; simulate a model that was already loaded via application-loader.lisp~%"))
    (unless (find-package "TESTPROJ/MODELS/BAR")
      (make-package "TESTPROJ/MODELS/BAR" :use '(#:cl)))
    (let ((output (with-output-to-string (*standard-output*)
                    (check-unregistered-models))))
      (ok (not (search "bar" output))
          "no warning expected once the model's package is loaded"))))
