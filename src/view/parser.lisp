(in-package #:cl-user)
(defpackage #:clails/view/parser
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:export #:parse-template))

(in-package #:clails/view/parser)

;;; Node is represented as a plist with keys:
;;; :type - :static-text, :dynamic-expr, :scriptlet, :cl-xxx
;;; :content - string or lisp code string
;;; :attributes - plist for tag attributes
;;; :children - list of child nodes (for :cl-tag)

(defun parse-template (template-string &key
                                       (start-expr "<%=")
                                       (start-script "<%")
                                       (tag-end "%>"))
  "Parse template string and return list of nodes.
   Supports:
   - <%= ... %> for expressions (evaluated and output)
   - <% ... %> for scriptlets (evaluated but not output)
   - <cl:xxx> tags for control structures"
  (let ((pos 0)
        (len (length template-string))
        (nodes '()))
    (loop while (< pos len)
          do (multiple-value-bind (node new-pos)
                 (parse-next template-string pos len start-expr start-script tag-end)
               (when node
                 (push node nodes))
               (setf pos new-pos)))
    (nreverse nodes)))

(defun parse-next (template pos len start-expr start-script tag-end)
  "Parse next node from template starting at pos.
   Returns (values node new-position)"
  (let ((cl-tag-pos (search "<cl:" template :start2 pos))
        (expr-pos (search start-expr template :start2 pos))
        (script-pos (search start-script template :start2 pos)))
    
    ;; Find the earliest delimiter
    (let ((next-pos (min-position cl-tag-pos expr-pos script-pos)))
      (cond
        ;; No more delimiters - return rest as static text
        ((null next-pos)
         (if (< pos len)
             (values (list :type :static-text
                          :content (subseq template pos))
                     len)
             (values nil len)))
        
        ;; Static text before next delimiter
        ((> next-pos pos)
         (values (list :type :static-text
                      :content (subseq template pos next-pos))
                 next-pos))
        
        ;; cl:tag
        ((eql next-pos cl-tag-pos)
         (parse-cl-tag template next-pos len start-expr start-script tag-end))
        
        ;; Expression <%= ... %>
        ((eql next-pos expr-pos)
         (parse-expression template next-pos len start-expr tag-end))
        
        ;; Scriptlet <% ... %>
        ((eql next-pos script-pos)
         (parse-scriptlet template next-pos len start-script tag-end))))))

(defun min-position (&rest positions)
  "Return the minimum non-nil position"
  (let ((valid-positions (remove nil positions)))
    (when valid-positions
      (apply #'min valid-positions))))

(defun parse-expression (template pos len start-expr tag-end)
  "Parse <%= ... %> expression"
  (let* ((start (+ pos (length start-expr)))
         (end-pos (search tag-end template :start2 start)))
    (if end-pos
        (values (list :type :dynamic-expr
                     :content (string-trim '(#\Space #\Tab #\Newline)
                                          (subseq template start end-pos)))
                (+ end-pos (length tag-end)))
        (error "Unclosed expression tag at position ~A" pos))))

(defun parse-scriptlet (template pos len start-script tag-end)
  "Parse <% ... %> scriptlet"
  (let* ((start (+ pos (length start-script)))
         (end-pos (search tag-end template :start2 start)))
    (if end-pos
        (values (list :type :scriptlet
                     :content (string-trim '(#\Space #\Tab #\Newline)
                                          (subseq template start end-pos)))
                (+ end-pos (length tag-end)))
        (error "Unclosed scriptlet tag at position ~A" pos))))

(defun find-closing-angle-bracket (template start)
  "Find the closing > of a tag, skipping over quoted strings"
  (let ((pos start)
        (len (length template))
        (in-quote nil))
    (loop while (< pos len)
          do (let ((c (aref template pos)))
               (cond
                 ((and (char= c #\") (not in-quote))
                  (setf in-quote t))
                 ((and (char= c #\") in-quote)
                  (setf in-quote nil))
                 ((and (char= c #\>) (not in-quote))
                  (return-from find-closing-angle-bracket pos)))
               (incf pos)))
    nil))

(defun parse-cl-tag (template pos len start-expr start-script tag-end)
  "Parse <cl:xxx> structured tags"
  (let* ((tag-start pos)
         (tag-name-end (or (position #\Space template :start pos)
                          (position #\> template :start pos)))
         (tag-name (subseq template (+ pos 4) tag-name-end)) ; skip "<cl:"
         ;; Find closing > considering quoted strings
         (close-tag-end (find-closing-angle-bracket template tag-name-end)))
    
    (unless close-tag-end
      (error "Malformed cl tag at position ~A" pos))
    
    ;; Parse attributes
    (let* ((attr-string (subseq template tag-name-end close-tag-end))
           (attributes (parse-attributes attr-string))
           (body-start (1+ close-tag-end))
           (closing-tag (format nil "</cl:~A>" tag-name))
           (closing-pos (search closing-tag template :start2 body-start)))
      
      (unless closing-pos
        (error "No closing tag found for <cl:~A>" tag-name))
      
      ;; Parse children recursively
      (let* ((body (subseq template body-start closing-pos))
             (children (parse-tag-body tag-name body start-expr start-script tag-end)))
        (values (list :type (intern (string-upcase (format nil "cl-~A" tag-name))
                                   :keyword)
                     :attributes attributes
                     :children children)
                (+ closing-pos (length closing-tag)))))))

(defun parse-tag-body (tag-name body start-expr start-script tag-end)
  "Parse the body of a cl: tag, handling special cases like cl:if/cl:else"
  (cond
    ;; For if tags, need to handle <cl:else>
    ((string= tag-name "if")
     (parse-if-body body start-expr start-script tag-end))
    
    ;; For cond tags, need to handle <cl:when> and <cl:otherwise>
    ((string= tag-name "cond")
     (parse-cond-body body start-expr start-script tag-end))
    
    ;; For other tags, just parse normally
    (t
     (parse-template body
                    :start-expr start-expr
                    :start-script start-script
                    :tag-end tag-end))))

(defun parse-if-body (body start-expr start-script tag-end)
  "Parse if tag body, splitting on <cl:else>"
  (let ((else-pos (search "<cl:else>" body)))
    (if else-pos
        (let* ((then-part (subseq body 0 else-pos))
               (else-start (+ else-pos (length "<cl:else>")))
               (else-end-pos (search "</cl:else>" body :start2 else-start)))
          (unless else-end-pos
            (error "No closing tag for <cl:else>"))
          (let ((else-part (subseq body else-start else-end-pos)))
            (list
             (list :type :then-clause
                  :children (parse-template then-part
                                           :start-expr start-expr
                                           :start-script start-script
                                           :tag-end tag-end))
             (list :type :else-clause
                  :children (parse-template else-part
                                           :start-expr start-expr
                                           :start-script start-script
                                           :tag-end tag-end)))))
        ;; No else clause
        (list (list :type :then-clause
                   :children (parse-template body
                                            :start-expr start-expr
                                            :start-script start-script
                                            :tag-end tag-end))))))

(defun parse-cond-body (body start-expr start-script tag-end)
  "Parse cond tag body, extracting <cl:when> and <cl:otherwise> clauses"
  (let ((clauses '())
        (pos 0)
        (len (length body)))
    (loop while (< pos len)
          do (let ((when-pos (search "<cl:when" body :start2 pos))
                   (otherwise-pos (search "<cl:otherwise>" body :start2 pos)))
               (cond
                 ;; Found a when clause
                 (when-pos
                  (multiple-value-bind (clause new-pos)
                      (parse-when-clause body when-pos start-expr start-script tag-end)
                    (push clause clauses)
                    (setf pos new-pos)))
                 
                 ;; Found otherwise clause
                 (otherwise-pos
                  (multiple-value-bind (clause new-pos)
                      (parse-otherwise-clause body otherwise-pos start-expr start-script tag-end)
                    (push clause clauses)
                    (setf pos new-pos)))
                 
                 ;; No more clauses, skip whitespace
                 (t
                  ;; Skip any remaining whitespace
                  (let ((next-non-space (position-if-not
                                        (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                                        body
                                        :start pos)))
                    (if next-non-space
                        (setf pos next-non-space)
                        (setf pos len)))))))
    (nreverse clauses)))

(defun parse-when-clause (body pos start-expr start-script tag-end)
  "Parse a single <cl:when> clause"
  (let* ((tag-end-pos (position #\> body :start pos))
         (attr-string (subseq body (+ pos (length "<cl:when")) tag-end-pos))
         (attributes (parse-attributes attr-string))
         (content-start (1+ tag-end-pos))
         (closing-pos (search "</cl:when>" body :start2 content-start)))
    (unless closing-pos
      (error "No closing tag for <cl:when>"))
    (let ((content (subseq body content-start closing-pos)))
      (values (list :type :when-clause
                   :attributes attributes
                   :children (parse-template content
                                            :start-expr start-expr
                                            :start-script start-script
                                            :tag-end tag-end))
              (+ closing-pos (length "</cl:when>"))))))

(defun parse-otherwise-clause (body pos start-expr start-script tag-end)
  "Parse <cl:otherwise> clause"
  (let* ((content-start (+ pos (length "<cl:otherwise>")))
         (closing-pos (search "</cl:otherwise>" body :start2 content-start)))
    (unless closing-pos
      (error "No closing tag for <cl:otherwise>"))
    (let ((content (subseq body content-start closing-pos)))
      (values (list :type :otherwise-clause
                   :children (parse-template content
                                            :start-expr start-expr
                                            :start-script start-script
                                            :tag-end tag-end))
              (+ closing-pos (length "</cl:otherwise>"))))))

(defun parse-attributes (attr-string)
  "Parse tag attributes into a plist.
   Example: ' test=\"(view :x)\" ' -> (:test \"(view :x)\")"
  (let ((attributes '())
        (pos 0)
        (len (length attr-string)))
    (loop while (< pos len)
          do (let ((eq-pos (position #\= attr-string :start pos)))
               (if eq-pos
                   ;; Found an attribute
                   (let* ((name-start (position-if-not (lambda (c)
                                                          (member c '(#\Space #\Tab #\Newline)))
                                                      attr-string
                                                      :start pos))
                          (name-end (position-if (lambda (c)
                                                   (member c '(#\Space #\Tab #\Newline #\=)))
                                                attr-string
                                                :start name-start))
                          (attr-name (when name-start
                                      (subseq attr-string name-start name-end)))
                          (quote-start (position #\" attr-string :start eq-pos))
                          (quote-end (when quote-start
                                      (position #\" attr-string :start (1+ quote-start)))))
                     (if (and attr-name quote-start quote-end)
                         (progn
                           (push (intern (string-upcase attr-name) :keyword) attributes)
                           (push (string-trim '(#\Space #\Tab #\Newline)
                                             (subseq attr-string (1+ quote-start) quote-end))
                                 attributes)
                           (setf pos (1+ quote-end)))
                         (setf pos len)))
                   ;; No more attributes
                   (setf pos len))))
    (nreverse attributes)))
