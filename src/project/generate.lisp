(in-package #:cl-user)
(defpackage #:clails/project/generate
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-dir*)
  (:export #:gen/model
           #:gen/migration
           #:gen/view
           #:gen/controller
           #:gen/scaffold
           #:gen/schema
           #:gen/task))
(in-package #:clails/project/generate)


(defun gen/template (base-name filename dir tmpl overwrite &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
  "Generate a file from a template.

   @param base-name [string] Base name for the generated item
   @param filename [string] Output filename
   @param dir [string] Relative directory path from project root
   @param tmpl [string] Path to template file (relative to clails system)
   @param overwrite [boolean] Whether to overwrite existing files
   @param start-delimiter [string] Template script start delimiter
   @param start-echo-delimiter [string] Template expression start delimiter
   @param end-delimiter [string] Template end delimiter
   @condition error Signaled when file exists and overwrite is false
   "
  (let* ((outfile (format nil "~A/~A/~A" *project-dir* dir filename))
         (template-file (asdf:system-relative-pathname :clails tmpl))
         (template-content (uiop:read-file-string template-file
                                                  :external-format :utf-8)))


    (format T "output: ~A~%" outfile)
    (when (and (not overwrite)
               (probe-file outfile))
      (error "already exists: ~A" outfile))
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :supersede)
      (format out "~A" (funcall (cl-template:compile-template template-content
                                                              :start-delimiter start-delimiter
                                                              :start-echo-delimiter start-echo-delimiter
                                                              :end-delimiter end-delimiter)
                                `(:project-name ,*project-name*
                                  :name ,base-name))))))


(defun add-routing (name)
  "Add routing configuration for a controller.

   @param name [string] Controller name
   "
  (let* ((outfile (format nil "~A/app/config/environment.lisp" *project-dir*))
         (template-file (asdf:system-relative-pathname :clails
                                                       "template/generate/config.lisp.tmpl"))
         (template-content (uiop:read-file-string template-file
                                                  :external-format :utf-8)))
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :append)
      (format out "~A" (funcall (cl-template:compile-template template-content)
                                `(:project-name ,*project-name*
                                  :name ,name
                                  :current-datetime ,(current-datetime)))))))

(defun add-import-model (name)
  "Add model import-from to application-loader.lisp.

   @param name [string] Model name
   "
  (let ((filepath (format nil "~A/app/application-loader.lisp" *project-dir*))
        (package-name (format nil "~A/models/~A" *project-name* name)))
    (add-import-to-defpackage filepath package-name)))

(defun add-import-controller (name)
  "Add controller import-from to application-loader.lisp.

   @param name [string] Controller name
   "
  (let ((filepath (format nil "~A/app/application-loader.lisp" *project-dir*))
        (package-name (format nil "~A/controllers/~A-controller" *project-name* name)))
    (add-import-to-defpackage filepath package-name)))


(defun add-import-view-package (name)
  "Add view package import-from to application-loader.lisp.

   @param name [string] View package name
   "
  (let ((filepath (format nil "~A/app/application-loader.lisp" *project-dir*))
        (package-name (format nil "~A/views/~A/package" *project-name* name)))
    (add-import-to-defpackage filepath package-name)))

(defun add-import-to-defpackage (filepath package-name)
  "Add an import-from clause to a defpackage form in a file.

   Reads the file, finds the defpackage form, adds the package to :import-from list,
   and rewrites the entire file.

   @param filepath [string] Path to the file containing defpackage
   @param package-name [string] Package name to import
   "
  (let* ((forms '()))
    ;; Read all forms from file
    (with-open-file (in filepath :direction :input)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            do (push form forms)))
    (setf forms (nreverse forms))

    ;; Find and modify defpackage form
    (let ((modified-forms
           (mapcar
            (lambda (form)
              (if (and (consp form)
                       (eq (first form) 'defpackage))
                  ;; This is a defpackage form
                  (let* ((pkg-name (second form))
                         (options (cddr form))
                         (new-options (copy-list options))
                         (import-found nil)
                         (pkg-symbol (intern (string-upcase package-name) :keyword)))
                    ;; Look for existing :import-from with the same package
                    (dolist (opt new-options)
                      (when (and (consp opt)
                                 (eq (first opt) :import-from)
                                 (string-equal (string (second opt)) package-name))
                        (setf import-found t)))
                    ;; Add new :import-from if not found
                    (unless import-found
                      (setf new-options (append new-options
                                                (list (list :import-from pkg-symbol)))))
                    (list* 'defpackage pkg-name new-options))
                  form))
            forms)))

      ;; Write back to file
      (with-open-file (out filepath
                           :direction :output
                           :if-exists :supersede)
        (format out "; -*- mode: lisp -*-~%")
        (dolist (form modified-forms)
          (write form :stream out :case :downcase :pretty t)
          (terpri out))))))

(defun add-test-import-model (name)
  "Add model test import-from to test/test-loader.lisp.

   @param name [string] Model name
   "
  (let ((filepath (format nil "~A/test/test-loader.lisp" *project-dir*))
        (package-name (format nil "~A-test/models/~A" *project-name* name)))
    (add-import-to-defpackage filepath package-name)))

(defun add-test-import-controller (name)
  "Add controller test import-from to test/test-loader.lisp.

   @param name [string] Controller name
   "
  (let ((filepath (format nil "~A/test/test-loader.lisp" *project-dir*))
        (package-name (format nil "~A-test/controllers/~A-controller" *project-name* name)))
    (add-import-to-defpackage filepath package-name)))

;; ----------------------------------------
;; model
(defun gen/model (model-name &key (overwrite T))
  "Generate a model file.

   @param model-name [string] Model name
   @param overwrite [boolean] Whether to overwrite existing file
   "
  (let ((filename (format nil "~A.lisp" model-name)))
    (gen/template model-name filename "/app/models/" "template/generate/model.lisp.tmpl" overwrite))
  (let ((test-filename (format nil "~A.lisp" model-name)))
    (gen/template model-name test-filename "/test/models/" "template/generate/test/model.lisp.tmpl" overwrite))
  ;; Add to application-loader.lisp
  (add-import-model model-name)
  ;; Add to test-loader.lisp
  (add-test-import-model model-name))

(defun gen/migration (migration-name &key (overwrite T))
  "Generate a migration file with unique timestamp prefix.

   @param migration-name [string] Migration name
   @param overwrite [boolean] Whether to overwrite existing file
   "
  (let* ((unique-name (gen-unique-name migration-name))
         (filename (format nil "~A.lisp" unique-name)))
    (gen/template unique-name filename "/db/migrate/" "template/generate/migration.lisp.tmpl" overwrite)))


;; ----------------------------------------
;; view
(defun gen/view (view-name &key (overwrite T))
  "Generate view files (package, show, new, edit, delete templates).

   @param view-name [string] View name
   @param overwrite [boolean] Whether to overwrite existing files
   "
  (let ((dir (format nil "app/views/~A/" view-name)))
    (ensure-directories-exist (format nil "~A/~A" *project-dir* dir))
    (gen/template view-name "package.lisp" dir "template/generate/views/package.lisp.tmpl"
                  overwrite
                  :start-delimiter "<%%"
                  :start-echo-delimiter "<%%="
                  :end-delimiter "%%>")
    (gen/template view-name "show.html" dir "template/generate/views/show.html.tmpl"
                  overwrite
                  :start-delimiter "<%%"
                  :start-echo-delimiter "<%%="
                  :end-delimiter "%%>")
    (gen/template view-name "new.html" dir "template/generate/views/new.html.tmpl"
                  overwrite
                  :start-delimiter "<%%"
                  :start-echo-delimiter "<%%="
                  :end-delimiter "%%>")
    (gen/template view-name "edit.html" dir "template/generate/views/edit.html.tmpl"
                  overwrite
                  :start-delimiter "<%%"
                  :start-echo-delimiter "<%%="
                  :end-delimiter "%%>")
    (gen/template view-name "delete.html" dir "template/generate/views/delete.html.tmpl"
                  overwrite
                  :start-delimiter "<%%"
                  :start-echo-delimiter "<%%="
                  :end-delimiter "%%>")
    (add-import-view-package view-name)))

;; ----------------------------------------
;; controller
(defun gen/controller (controller-name &key (overwrite T))
  "Generate a controller file.

   @param controller-name [string] Controller name
   @param overwrite [boolean] Whether to overwrite existing file
   "
  (let ((filename (format nil "~A-controller.lisp" controller-name)))
    (gen/template controller-name filename "/app/controllers/" "template/generate/controller.lisp.tmpl" overwrite))
  (let ((test-filename (format nil "~A-controller.lisp" controller-name)))
    (gen/template controller-name test-filename "/test/controllers/" "template/generate/test/controller.lisp.tmpl" overwrite))
  (add-routing controller-name)
  (add-import-controller controller-name)
  ;; Add to test-loader.lisp
  (add-test-import-controller controller-name))

;; ----------------------------------------
;; scaffold
(defun gen/scaffold (name &key (overwrite T))
  "Generate scaffold (model, migration, view, controller) for a resource.

   @param name [string] Resource name
   @param overwrite [boolean] Whether to overwrite existing files
   "
  (gen/model name :overwrite overwrite)
  (gen/migration name :overwrite overwrite)
  (gen/view name :overwrite overwrite)
  (gen/controller name :overwrite overwrite))


(defun gen-unique-name (base-name)
  "Generate unique name with timestamp prefix.

   @param base-name [string] Base name
   @return [string] Unique name in format YYYYMMDDHHMMSS_basename
   "
  (format NIL "~A_~A"
          (current-datetime)
          base-name))

(defun current-datetime ()
  "Get current datetime as formatted string.

   @return [string] Current datetime in format YYYYMMDDHHMMSS
   "
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))


;; ----------------------------------------
;; schema
(defun gen/schema (tables)
  "Generate database schema file from table definitions.

   @param tables [list] List of table definitions
   "
  (let* ((outfile (format nil "~A/db/schema.lisp" *project-dir*))
         (template-file (asdf:system-relative-pathname :clails "template/generate/db/schema.lisp.tmpl"))
         (template-content (uiop:read-file-string template-file
                                                  :external-format :utf-8)))

    (format T "output: ~A~%" outfile)
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :supersede)
      (format out "~A" (funcall (cl-template:compile-template template-content)
                                `(:project-name ,*project-name*
                                  :tables ,tables))))))

;; ----------------------------------------
;; task
(defun gen/task (task-name &key namespace)
  "Generate task file.

   @param task-name [string] Task name (e.g., 'cleanup', 'import')
   @param namespace [string or nil] Optional namespace (e.g., 'maintenance', 'data')
   "
  (let* ((task-dir (if namespace
                      (format nil "lib/tasks/~A" (string-downcase namespace))
                      "lib/tasks"))
         (package-name (if namespace
                          (format nil "~A/tasks/~A/~A"
                                  *project-name*
                                  (string-downcase namespace)
                                  (string-downcase task-name))
                          (format nil "~A/tasks/~A"
                                  *project-name*
                                  (string-downcase task-name))))
         (filename (format nil "~A.lisp" (string-downcase task-name)))
         (outfile (format nil "~A/~A/~A" *project-dir* task-dir filename))
         (template-file (asdf:system-relative-pathname :clails "template/generate/lib/tasks/task.lisp.tmpl"))
         (template-content (uiop:read-file-string template-file
                                                  :external-format :utf-8)))

    ;; Ensure directory exists
    (ensure-directories-exist (format nil "~A/~A/" *project-dir* task-dir))

    (format t "output: ~A~%" outfile)
    (when (probe-file outfile)
      (error "already exists: ~A" outfile))

    (with-open-file (out outfile
                         :direction :output
                         :if-exists :supersede)
      (format out "~A" (funcall (cl-template:compile-template template-content)
                                `(:project-name ,*project-name*
                                  :package-name ,package-name
                                  :task-name ,task-name
                                  :namespace ,(when namespace (string-upcase namespace))))))))

