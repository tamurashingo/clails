(in-package #:cl-user)
(defpackage #:clails-test/view/parser
  (:use #:cl
        #:rove
        #:clails/view/parser))

(in-package #:clails-test/view/parser)

(deftest test-parse-static-text
  (testing "Parse simple static text"
    (let ((result (parse-template "Hello World")))
      (ok (= 1 (length result)))
      (ok (eq :static-text (getf (first result) :type)))
      (ok (string= "Hello World" (getf (first result) :content))))))

(deftest test-parse-expression
  (testing "Parse <%= ... %> expression"
    (let ((result (parse-template "Hello <%= name %> World")))
      (ok (= 3 (length result)))
      (ok (eq :static-text (getf (first result) :type)))
      (ok (string= "Hello " (getf (first result) :content)))
      (ok (eq :dynamic-expr (getf (second result) :type)))
      (ok (string= "name" (getf (second result) :content)))
      (ok (eq :static-text (getf (third result) :type)))
      (ok (string= " World" (getf (third result) :content))))))

(deftest test-parse-scriptlet
  (testing "Parse <% ... %> scriptlet"
    (let ((result (parse-template "<% (let ((x 10))) %><%= x %>")))
      (ok (= 2 (length result)))
      (ok (eq :scriptlet (getf (first result) :type)))
      (ok (eq :dynamic-expr (getf (second result) :type))))))

(deftest test-parse-cl-loop
  (testing "Parse <cl:loop> tag"
    (let ((result (parse-template "<cl:loop for=\"item\" in=\"items\"><%= item %></cl:loop>")))
      (ok (= 1 (length result)))
      (ok (eq :cl-loop (getf (first result) :type)))
      (let ((attrs (getf (first result) :attributes)))
        (ok (string= "item" (getf attrs :for)))
        (ok (string= "items" (getf attrs :in))))
      (let ((children (getf (first result) :children)))
        (ok (= 1 (length children)))
        (ok (eq :dynamic-expr (getf (first children) :type)))))))

(deftest test-parse-cl-if
  (testing "Parse <cl:if> tag without else"
    (let ((result (parse-template "<cl:if test=\"(> x 10)\">Big</cl:if>")))
      (ok (= 1 (length result)))
      (ok (eq :cl-if (getf (first result) :type)))
      (let ((attrs (getf (first result) :attributes)))
        (ok (string= "(> x 10)" (getf attrs :test))))
      (let ((children (getf (first result) :children)))
        (ok (= 1 (length children)))
        (ok (eq :then-clause (getf (first children) :type))))))
  
  (testing "Parse <cl:if> tag with else"
    (let ((result (parse-template "<cl:if test=\"(> x 10)\">Big<cl:else>Small</cl:else></cl:if>")))
      (ok (= 1 (length result)))
      (ok (eq :cl-if (getf (first result) :type)))
      (let ((children (getf (first result) :children)))
        (ok (= 2 (length children)))
        (ok (eq :then-clause (getf (first children) :type)))
        (ok (eq :else-clause (getf (second children) :type)))))))

(deftest test-parse-cl-when
  (testing "Parse <cl:when> tag"
    (let ((result (parse-template "<cl:when test=\"condition\">Content</cl:when>")))
      (ok (= 1 (length result)))
      (ok (eq :cl-when (getf (first result) :type)))
      (let ((attrs (getf (first result) :attributes)))
        (ok (string= "condition" (getf attrs :test)))))))

(deftest test-parse-cl-unless
  (testing "Parse <cl:unless> tag"
    (let ((result (parse-template "<cl:unless test=\"condition\">Content</cl:unless>")))
      (ok (= 1 (length result)))
      (ok (eq :cl-unless (getf (first result) :type)))
      (let ((attrs (getf (first result) :attributes)))
        (ok (string= "condition" (getf attrs :test)))))))
