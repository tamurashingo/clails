(in-package #:cl-user)
(defpackage #:clails-test/view/compiler
  (:use #:cl
        #:rove
        #:clails/view/compiler)
  (:import-from #:clails/view/view-helper
                #:*view-context*))

(in-package #:clails-test/view/compiler)

(deftest test-compile-static-text
  (testing "Compile and execute static text template"
    (let* ((compiled-fn (compile-template "Hello World"))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Hello World" result)))))

(deftest test-compile-expression
  (testing "Compile and execute expression template using *view-context*"
    (let* ((compiled-fn (compile-template "Hello <%= (getf clails/view/view-helper:*view-context* :name) %>!"))
           (*view-context* '(:name "Lisp"))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Hello Lisp!" result)))))

(deftest test-compile-scriptlet
  (testing "Compile and execute scriptlet template - scriptlets don't output"
    (let* ((compiled-fn (compile-template "Before<% (princ \"INSIDE\" stream) %>After"))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "BeforeINSIDEAfter" result)))))

(deftest test-compile-loop
  (testing "Compile and execute loop template"
    (let* ((template "<cl:loop for=\"item\" in=\"(getf clails/view/view-helper:*view-context* :items)\"><%= item %> </cl:loop>")
           (compiled-fn (compile-template template))
           (*view-context* '(:items (1 2 3)))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "1 2 3 " result)))))

(deftest test-compile-if-without-else
  (testing "Compile and execute if without else (true case)"
    (let* ((template "<cl:if test=\"(getf clails/view/view-helper:*view-context* :show)\">Visible</cl:if>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show t))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Visible" result))))
  
  (testing "Compile and execute if without else (false case)"
    (let* ((template "<cl:if test=\"(getf clails/view/view-helper:*view-context* :show)\">Visible</cl:if>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show nil))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "" result)))))

(deftest test-compile-if-with-else
  (testing "Compile and execute if with else (true case)"
    (let* ((template "<cl:if test=\"(getf clails/view/view-helper:*view-context* :show)\">Yes<cl:else>No</cl:else></cl:if>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show t))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Yes" result))))
  
  (testing "Compile and execute if with else (false case)"
    (let* ((template "<cl:if test=\"(getf clails/view/view-helper:*view-context* :show)\">Yes<cl:else>No</cl:else></cl:if>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show nil))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "No" result)))))

(deftest test-compile-when
  (testing "Compile and execute when (true case)"
    (let* ((template "<cl:when test=\"(getf clails/view/view-helper:*view-context* :show)\">Content</cl:when>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show t))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Content" result))))
  
  (testing "Compile and execute when (false case)"
    (let* ((template "<cl:when test=\"(getf clails/view/view-helper:*view-context* :show)\">Content</cl:when>")
           (compiled-fn (compile-template template))
           (*view-context* '(:show nil))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "" result)))))

(deftest test-compile-unless
  (testing "Compile and execute unless (false case)"
    (let* ((template "<cl:unless test=\"(getf clails/view/view-helper:*view-context* :hide)\">Content</cl:unless>")
           (compiled-fn (compile-template template))
           (*view-context* '(:hide nil))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "Content" result))))
  
  (testing "Compile and execute unless (true case)"
    (let* ((template "<cl:unless test=\"(getf clails/view/view-helper:*view-context* :hide)\">Content</cl:unless>")
           (compiled-fn (compile-template template))
           (*view-context* '(:hide t))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (string= "" result)))))

(deftest test-compile-complex-template
  (testing "Compile and execute complex template"
    (let* ((template "
<html>
<body>
  <h1><%= (getf clails/view/view-helper:*view-context* :title) %></h1>
  <ul>
    <cl:loop for=\"item\" in=\"(getf clails/view/view-helper:*view-context* :items)\">
      <li>
        <cl:if test=\"(getf item :done)\">
          <s><%= (getf item :text) %></s>
        <cl:else>
          <%= (getf item :text) %>
        </cl:else>
        </cl:if>
      </li>
    </cl:loop>
  </ul>
</body>
</html>
")
           (compiled-fn (compile-template template))
           (*view-context* '(:title "Todo List"
                            :items ((:text "Buy milk" :done nil)
                                   (:text "Clean room" :done t))))
           (result (with-output-to-string (s)
                     (funcall compiled-fn s))))
      (ok (search "Todo List" result))
      (ok (search "Buy milk" result))
      (ok (search "<s>Clean room</s>" result)))))
