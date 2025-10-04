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

;;; Global template cache: key=absolute-path, value=(compiled-function . last-modified-time)
(defparameter *template-cache* (make-hash-table :test 'equal))

(defun get-cached-template (template-path)
  "Get cached compiled template function for given path.
   Returns NIL if not cached or if recompilation is needed in development mode."
  (let ((cached (gethash template-path *template-cache*)))
    (when cached
      (destructuring-bind (compiled-fn . cached-time) cached
        (if (should-recompile-p template-path cached-time)
            nil
            compiled-fn)))))

(defun cache-template (template-path compiled-fn)
  "Cache compiled template function with current file modification time"
  (let ((mtime (file-write-date template-path)))
    (setf (gethash template-path *template-cache*)
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
