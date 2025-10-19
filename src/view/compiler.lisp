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
                                         (tag-end "%>")
                                         (package *package*))
  "Compile template string into a lambda function.
   
   The compiled function takes a stream parameter and writes the rendered
   HTML to that stream.
   
   @param template-string [string] Template content to compile
   @param start-expr [string] Opening delimiter for expressions (default: \"<%=\")
   @param start-script [string] Opening delimiter for scriptlets (default: \"<%\")
   @param tag-end [string] Closing delimiter (default: \"%>\")
   @param package [package] Package for symbol resolution (default: *package*)
   @return [function] Compiled template function
   "
  (let ((nodes (parse-template template-string
                               :start-expr start-expr
                               :start-script start-script
                               :tag-end tag-end)))
    (compile nil
             `(lambda (&optional (stream *standard-output*))
                ,@(compile-nodes nodes package)
                nil))))

(defun compile-nodes (nodes package)
  "Compile a list of nodes into Lisp code.
   
   @param nodes [list] List of parsed template nodes
   @param package [package] Package for symbol resolution
   @return [list] List of compiled forms
   "
  (mapcar (lambda (node) (compile-node node package)) nodes))

(defun compile-node (node package)
  "Compile a single node into Lisp code.
   
   @param node [plist] Parsed template node
   @param package [package] Package for symbol resolution
   @return [form] Compiled Lisp form
   @condition error Signaled for unknown node types
   "
  (let ((type (getf node :type)))
    (case type
      (:static-text
       (compile-static-text node))
      
      (:dynamic-expr
       (compile-dynamic-expr node package))
      
      (:scriptlet
       (compile-scriptlet node package))
      
      (:cl-loop
       (compile-cl-loop node package))
      
      (:cl-if
       (compile-cl-if node package))
      
      (:cl-cond
       (compile-cl-cond node package))
      
      (:cl-when
       (compile-cl-when node package))
      
      (:cl-unless
       (compile-cl-unless node package))
      
      (t
       (error "Unknown node type: ~A" type)))))

(defun compile-static-text (node)
  "Compile static text node.
   
   @param node [plist] Node with :content key
   @return [form] Form to write static text to stream
   "
  (let ((content (getf node :content)))
    `(write-string ,content stream)))

(defun compile-dynamic-expr (node package)
  "Compile dynamic expression node <%= ... %>.
   
   @param node [plist] Node with :content key containing expression
   @param package [package] Package for reading the expression
   @return [form] Form to evaluate expression and write to stream
   "
  (let ((content (getf node :content)))
    ;; Read the expression in the specified package context
    `(princ ,(let ((*package* package))
               (read-from-string content))
            stream)))

(defun compile-scriptlet (node package)
  "Compile scriptlet node <% ... %>.
   
   Scriptlets are evaluated but their result is not written to the output.
   
   @param node [plist] Node with :content key containing code
   @param package [package] Package for reading the code
   @return [form] Form to evaluate the scriptlet
   "
  (let ((content (getf node :content)))
    (let ((*package* package))
      (read-from-string content))))

(defun compile-cl-loop (node package)
  "Compile <cl:loop> tag into loop form.
   
   Transforms <cl:loop for=\"var\" in=\"list-expr\"> into a loop form that
   iterates over the list and renders the child nodes for each element.
   
   @param node [plist] Node with :attributes and :children
   @param package [package] Package for reading the expressions
   @return [form] Loop form with compiled child nodes
   @condition error Signaled when 'for' or 'in' attributes are missing
   "
  (let* ((attributes (getf node :attributes))
         (var (getf attributes :for))
         (list-expr (getf attributes :in))
         (children (getf node :children)))
    (unless (and var list-expr)
      (error "cl:loop requires both 'for' and 'in' attributes"))
    `(loop for ,(let ((*package* package))
                  (read-from-string var))
           in ,(let ((*package* package))
                 (read-from-string list-expr))
           do (progn
                ,@(compile-nodes children package)))))

(defun compile-cl-if (node package)
  "Compile <cl:if> tag with optional <cl:else>.
   
   Transforms <cl:if test=\"expr\"> with <cl:then> and optional <cl:else>
   into an if/when form that conditionally renders child nodes.
   
   @param node [plist] Node with :attributes and :children
   @param package [package] Package for reading the test expression
   @return [form] If or when form with compiled child nodes
   @condition error Signaled when 'test' attribute is missing
   "
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
          `(if ,(let ((*package* package))
                  (read-from-string test-expr))
               (progn
                 ,@(compile-nodes (getf then-clause :children) package))
               (progn
                 ,@(compile-nodes (getf else-clause :children) package)))
          `(when ,(let ((*package* package))
                    (read-from-string test-expr))
             ,@(compile-nodes (getf then-clause :children) package))))))

(defun compile-cl-cond (node package)
  "Compile <cl:cond> tag with <cl:when> and <cl:otherwise> clauses.
   
   Transforms <cl:cond> with nested <cl:when test=\"expr\"> and optional
   <cl:otherwise> into a cond form.
   
   @param node [plist] Node with :children containing when and otherwise clauses
   @param package [package] Package for reading expressions
   @return [form] Cond form with compiled clauses
   "
  (let ((children (getf node :children)))
    `(cond
       ,@(mapcar (lambda (clause) (compile-cond-clause clause package)) children))))

(defun compile-cond-clause (clause package)
  "Compile a single clause of cl:cond.
   
   Compiles <cl:when> or <cl:otherwise> clause into a cond clause.
   
   @param clause [plist] Clause node (:when-clause or :otherwise-clause)
   @param package [package] Package for reading expressions
   @return [list] Cond clause (test-form . body-forms)
   @condition error Signaled for unknown clause types or missing 'test' attribute
   "
  (let ((type (getf clause :type)))
    (case type
      (:when-clause
       (let* ((attributes (getf clause :attributes))
              (test-expr (getf attributes :test))
              (children (getf clause :children)))
         (unless test-expr
           (error "cl:when requires 'test' attribute"))
         `(,(let ((*package* package))
              (read-from-string test-expr))
           ,@(compile-nodes children package))))
      
      (:otherwise-clause
       (let ((children (getf clause :children)))
         `(t
           ,@(compile-nodes children package))))
      
      (t
       (error "Unknown cond clause type: ~A" type)))))

(defun compile-cl-when (node package)
  "Compile <cl:when> tag (standalone, not in cond).
   
   Transforms <cl:when test=\"expr\"> into a when form that conditionally
   renders child nodes.
   
   @param node [plist] Node with :attributes and :children
   @param package [package] Package for reading the test expression
   @return [form] When form with compiled child nodes
   @condition error Signaled when 'test' attribute is missing
   "
  (let* ((attributes (getf node :attributes))
         (test-expr (getf attributes :test))
         (children (getf node :children)))
    (unless test-expr
      (error "cl:when requires 'test' attribute"))
    `(when ,(let ((*package* package))
              (read-from-string test-expr))
       ,@(compile-nodes children package))))

(defun compile-cl-unless (node package)
  "Compile <cl:unless> tag.
   
   Transforms <cl:unless test=\"expr\"> into an unless form that conditionally
   renders child nodes when the test is false.
   
   @param node [plist] Node with :attributes and :children
   @param package [package] Package for reading the test expression
   @return [form] Unless form with compiled child nodes
   @condition error Signaled when 'test' attribute is missing
   "
  (let* ((attributes (getf node :attributes))
         (test-expr (getf attributes :test))
         (children (getf node :children)))
    (unless test-expr
      (error "cl:unless requires 'test' attribute"))
    `(unless ,(let ((*package* package))
                (read-from-string test-expr))
       ,@(compile-nodes children package))))
