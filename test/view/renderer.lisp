(in-package #:cl-user)
(defpackage #:clails-test/view/renderer
  (:use #:cl
        #:rove
        #:clails/view/renderer))

(in-package #:clails-test/view/renderer)

(deftest test-render-with-view-helper
  (testing "Render template using view helper function"
    ;; Create temporary template file using uiop
    (uiop:with-temporary-file (:stream out 
                               :suffix ".html"
                               :direction :output
                               :keep t
                               :pathname temp-file
                               :external-format :utf-8)
      ;; Write template
      (write-string "Hello <%= (clails/view/view-helper:view :name) %>!" out)
      (finish-output out)
      (close out)
      
      ;; Render template
      (unwind-protect
           (let ((result (render temp-file '(:name "World"))))
             (ok (string= "Hello World!" result)))
        
        ;; Cleanup
        (when (probe-file temp-file)
          (delete-file temp-file))))))
