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
           #:gen/schema))
(in-package #:clails/project/generate)


(defun gen/template (base-name filename dir tmpl overwrite &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
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

(defun add-load (comment load-package)
  (let ((outfile (format nil "~A/app/application.lisp" *project-dir*)))
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :append)
      (when comment
        (format out "~A~%" comment))
      (format out "~A~%" load-package))))

(defun add-load-controller (name)
  (let ((comment (format nil "; -- ~A : add ~A controller~%" (current-datetime) name))
        (load-statement (format nil "(asdf:load-system :~A/controllers/~A-controller)~%" *project-name* name)))
    (add-load comment load-statement)))


(defun add-load-view-package (name)
  (let ((comment (format nil "; -- ~A : add ~A view package~%" (current-datetime) name))
        (load-statement (format nil "(asdf:load-system :~A/views/~A/package)~%" *project-name* name)))
    (add-load comment load-statement)))

;; ----------------------------------------
;; model
(defun gen/model (model-name &key (overwrite T))
  (let ((filename (format nil "~A.lisp" model-name)))
    (gen/template model-name filename "/app/models/" "template/generate/model.lisp.tmpl" overwrite)))

(defun gen/migration (migration-name &key (overwrite T))
  (let* ((unique-name (gen-unique-name migration-name))
         (filename (format nil "~A.lisp" unique-name)))
    (gen/template unique-name filename "/db/migrate/" "template/generate/migration.lisp.tmpl" overwrite)))


;; ----------------------------------------
;; view
(defun gen/view (view-name &key (overwrite T))
  (let ((dir (format nil "app/views/~A/" view-name)))
    (ensure-directories-exist dir)
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
    (add-load-view-package view-name)))

;; ----------------------------------------
;; controller
(defun gen/controller (controller-name &key (overwrite T))
  (let ((filename (format nil "~A-controller.lisp" controller-name)))
    (gen/template controller-name filename "/app/controllers/" "template/generate/controller.lisp.tmpl" overwrite))
  (add-routing controller-name)
  (add-load-controller controller-name))

;; ----------------------------------------
;; scaffold
(defun gen/scaffold (name &key (overwrite T))
  (gen/model name :overwrite overwrite)
  (gen/migration name :overwrite overwrite)
  (gen/view name :overwrite overwrite)
  (gen/controller name :overwrite overwrite))


(defun gen-unique-name (base-name)
  (format NIL "~A_~A"
          (current-datetime)
          base-name))

(defun current-datetime ()
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))


;; ----------------------------------------
;; schema
(defun gen/schema (tables)
  (let* ((outfile (format nil "~A/db/schema.lisp" *project-dir*))
         (template-file (asdf:system-relative-pathname :clails "template/generate/db/schema.lisp.tmpl"))
         (template-content (uiop:read-file-string template-file
                                                  :external-format :utf-8)))

    (format T "output: ~A~%" outfile)
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :supersede)
      (format out "~A" (funcall (cl-template:compile-template template-content)
                                `(:tables ,tables))))))
