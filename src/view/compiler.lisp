(in-package #:cl-user)
(defpackage #:clails/view/compiler
  (:use #:cl)
  (:import-from #:clails/view/parser
                #:parse-template)
  (:export #:compile-template))

(in-package #:clails/view/compiler)

(defun compile-template (template-string &key
                                         (start-expr "<%=")
                                         (start-script "<%")
                                         (tag-end "%>"))
  "Compile template string into a lambda function.
   The lambda takes a plist of data and an optional stream parameter.
   Returns HTML string."
  (let ((nodes (parse-template template-string
                               :start-expr start-expr
                               :start-script start-script
                               :tag-end tag-end)))
    (compile nil
             `(lambda (&optional (stream *standard-output*))
                ,@(compile-nodes nodes)
                nil))))

(defun compile-nodes (nodes)
  "Compile a list of nodes into Lisp code"
  (mapcar #'compile-node nodes))

(defun compile-node (node)
  "Compile a single node into Lisp code"
  (let ((type (getf node :type)))
    (case type
      (:static-text
       (compile-static-text node))
      
      (:dynamic-expr
       (compile-dynamic-expr node))
      
      (:scriptlet
       (compile-scriptlet node))
      
      (:cl-loop
       (compile-cl-loop node))
      
      (:cl-if
       (compile-cl-if node))
      
      (:cl-cond
       (compile-cl-cond node))
      
      (:cl-when
       (compile-cl-when node))
      
      (:cl-unless
       (compile-cl-unless node))
      
      (t
       (error "Unknown node type: ~A" type)))))

(defun compile-static-text (node)
  "Compile static text node"
  (let ((content (getf node :content)))
    `(write-string ,content stream)))

(defun compile-dynamic-expr (node)
  "Compile dynamic expression node <%= ... %>"
  (let ((content (getf node :content)))
    ;; Read the expression in a neutral context
    `(princ ,(read-from-string content) stream)))

(defun compile-scriptlet (node)
  "Compile scriptlet node <% ... %>"
  (let ((content (getf node :content)))
    (read-from-string content)))

(defun compile-cl-loop (node)
  "Compile <cl:loop> tag"
  (let* ((attributes (getf node :attributes))
         (var (getf attributes :for))
         (list-expr (getf attributes :in))
         (children (getf node :children)))
    (unless (and var list-expr)
      (error "cl:loop requires both 'for' and 'in' attributes"))
    `(loop for ,(read-from-string var) in ,(read-from-string list-expr)
           do (progn
                ,@(compile-nodes children)))))

(defun compile-cl-if (node)
  "Compile <cl:if> tag with optional <cl:else>"
  (let* ((attributes (getf node :attributes))
         (test-expr (getf attributes :test))
         (children (getf node :children)))
    (unless test-expr
      (error "cl:if requires 'test' attribute"))
    
    ;; Find then and else clauses
    (let ((then-clause (find-if #'(lambda (child)
                                    (eq (getf child :type) :then-clause))
                                children))
          (else-clause (find-if #'(lambda (child)
                                    (eq (getf child :type) :else-clause))
                                children)))
      (if else-clause
          `(if ,(read-from-string test-expr)
               (progn
                 ,@(compile-nodes (getf then-clause :children)))
               (progn
                 ,@(compile-nodes (getf else-clause :children))))
          `(when ,(read-from-string test-expr)
             ,@(compile-nodes (getf then-clause :children)))))))

(defun compile-cl-cond (node)
  "Compile <cl:cond> tag with <cl:when> and <cl:otherwise> clauses"
  (let ((children (getf node :children)))
    `(cond
       ,@(mapcar #'compile-cond-clause children))))

(defun compile-cond-clause (clause)
  "Compile a single clause of cl:cond"
  (let ((type (getf clause :type)))
    (case type
      (:when-clause
       (let* ((attributes (getf clause :attributes))
              (test-expr (getf attributes :test))
              (children (getf clause :children)))
         (unless test-expr
           (error "cl:when requires 'test' attribute"))
         `(,(read-from-string test-expr)
           ,@(compile-nodes children))))
      
      (:otherwise-clause
       (let ((children (getf clause :children)))
         `(t
           ,@(compile-nodes children))))
      
      (t
       (error "Unknown cond clause type: ~A" type)))))

(defun compile-cl-when (node)
  "Compile <cl:when> tag (standalone, not in cond)"
  (let* ((attributes (getf node :attributes))
         (test-expr (getf attributes :test))
         (children (getf node :children)))
    (unless test-expr
      (error "cl:when requires 'test' attribute"))
    `(when ,(read-from-string test-expr)
       ,@(compile-nodes children))))

(defun compile-cl-unless (node)
  "Compile <cl:unless> tag"
  (let* ((attributes (getf node :attributes))
         (test-expr (getf attributes :test))
         (children (getf node :children)))
    (unless test-expr
      (error "cl:unless requires 'test' attribute"))
    `(unless ,(read-from-string test-expr)
       ,@(compile-nodes children))))
