(in-package #:cl-user)
(defpackage #:clails/view/cache
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-environment*)
  (:export #:get-cached-template
           #:cache-template
           #:clear-template-cache
           #:should-recompile-p))

(in-package #:clails/view/cache)

;;; Global template cache: key=(template-path . package-name), value=(compiled-function . last-modified-time)
(defparameter *template-cache* (make-hash-table :test 'equal))

(defun make-cache-key (template-path package)
  "Create cache key from template path and package.
   
   @param template-path [pathname] Path to template file
   @param template-path [string] Path to template file
   @param package [package] Package object
   @return [cons] Cache key as (path . package-name)
   "
  (cons (namestring template-path)
        (package-name package)))

(defun get-cached-template (template-path package)
  "Get cached compiled template function for given path and package.
   Returns NIL if not cached or if recompilation is needed in development mode.
   
   @param template-path [pathname] Path to template file
   @param template-path [string] Path to template file
   @param package [package] Package used for compilation
   @return [function] Compiled template function or NIL
   "
  (let* ((cache-key (make-cache-key template-path package))
         (cached (gethash cache-key *template-cache*)))
    (when cached
      (destructuring-bind (compiled-fn . cached-time) cached
        (if (should-recompile-p template-path cached-time)
            nil
            compiled-fn)))))

(defun cache-template (template-path package compiled-fn)
  "Cache compiled template function with current file modification time.
   
   @param template-path [pathname] Path to template file
   @param template-path [string] Path to template file
   @param package [package] Package used for compilation
   @param compiled-fn [function] Compiled template function
   @return [function] The compiled function (for chaining)
   "
  (let ((mtime (file-write-date template-path))
        (cache-key (make-cache-key template-path package)))
    (setf (gethash cache-key *template-cache*)
          (cons compiled-fn mtime)))
  compiled-fn)

(defun clear-template-cache ()
  "Clear all cached templates"
  (clrhash *template-cache*))

(defun should-recompile-p (template-path cached-time)
  "Check if template should be recompiled.
   In development mode, check file modification time.
   In production mode, never recompile."
  (let ((env (or *project-environment* :develop)))
    (case env
      (:production
       ;; Never recompile in production
       nil)
      
      (t
       ;; In development, check if file has been modified
       (let ((current-time (file-write-date template-path)))
         (or (null current-time)
             (null cached-time)
             (> current-time cached-time)))))))
