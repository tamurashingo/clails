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
           #:gen/scaffold))
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

(defun add-load (name)
  (let ((outfile (format nil "~A/app/application.lisp" *project-dir*)))
    (with-open-file (out outfile
                         :direction :output
                         :if-exists :append)
      (format out ";-- ~A : add ~A controller~%" (current-datetime) name)
      (format out "(asdf:load-system :~A/controllers/~A-controller)~%~%" *project-name* name))))

;; ----------------------------------------
;; model
(defun gen/model (model-name &key (overwrite T))
  (let ((filename (format nil "~A.lisp" model-name)))
    (gen/template model-name filename "/app/models/" "template/generate/model.lisp.tmpl" overwrite)))

(defun gen/migration (migration-name &key (overwrite T))
  (let ((filename (format nil "~A.lisp" (gen-unique-name migration-name))))
    (gen/template migration-name filename "/db/migrate/" "template/generate/migration.lisp.tmpl" overwrite)))


;; ----------------------------------------
;; view
(defun gen/view (view-name &key (overwrite T))
  (let ((dir (format nil "app/views/~A/" view-name)))
    (ensure-directories-exist dir)
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
                  :end-delimiter "%%>")))

;; ----------------------------------------
;; controller
(defun gen/controller (controller-name &key (overwrite T))
  (let ((filename (format nil "~A-controller.lisp" controller-name)))
    (gen/template controller-name filename "/app/controllers/" "template/generate/controller.lisp.tmpl" overwrite))
  (add-routing controller-name)
  (add-load controller-name))

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
