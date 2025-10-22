(in-package #:cl-user)
(defpackage #:clails/view/renderer
  (:use #:cl)
  (:import-from #:clails/view/parser
                #:parse-template)
  (:import-from #:clails/view/compiler
                #:compile-template)
  (:import-from #:clails/view/cache
                #:get-cached-template
                #:cache-template)
  (:import-from #:clails/view/view-helper
                #:*view-context*)
  (:export #:render))

(in-package #:clails/view/renderer)

(defun render (template-path data &key
                                   (start-expr "<%=")
                                   (start-script "<%")
                                   (tag-end "%>")
                                   (package *package*))
  "Render template with given data.
   
   Parameters:
   - template-path: Path to template file
   - data: plist of data to pass to template
   - start-expr: Opening delimiter for expressions (default: \"<%=\")
   - start-script: Opening delimiter for scriptlets (default: \"<%\")
   - tag-end: Closing delimiter (default: \"%>\")
   - package: Package to use when evaluating template code
   
   Returns: Rendered HTML as string"
  
  ;; Get or compile template
  (let ((compiled-fn (or (get-cached-template template-path package)
                         (compile-and-cache-template template-path
                                                     start-expr
                                                     start-script
                                                     tag-end
                                                     package))))
    
    ;; Execute template with data
    (let ((*view-context* data))
      (with-output-to-string (stream)
        (funcall compiled-fn stream)))))

(defun compile-and-cache-template (template-path start-expr start-script tag-end package)
  "Read, compile and cache a template.
   
   @param template-path [pathname] Path to template file
   @param template-path [string] Path to template file
   @param start-expr [string] Opening delimiter for expressions
   @param start-script [string] Opening delimiter for scriptlets
   @param tag-end [string] Closing delimiter
   @param package [package] Package to use for symbol resolution
   @return [function] Compiled template function
   "
  (let* ((template-string (uiop:read-file-string template-path
                                                 :external-format :utf-8))
         (compiled-fn (compile-template template-string
                                        :start-expr start-expr
                                        :start-script start-script
                                        :tag-end tag-end
                                        :package package)))
    (cache-template template-path package compiled-fn)))
