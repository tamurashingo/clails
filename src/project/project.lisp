(in-package #:cl-user)
(defpackage #:clails/project/project
  (:use #:cl)
  (:import-from #:clails/project/template
                #:<template>
                #:template
                #:path)
  (:export #:create-project))

(in-package #:clails/project/project)

(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "app/config"
    "db"
    "db/migrate"
    "tmp"
    "public"))

(defun create-project (project-name project-dir database)
  (create-directories project-dir)
  (create-initial-files project-name (namestring project-dir) database)
  (copy-asset-files project-dir))

(defun create-directories (project-dir)
  ;; TODO: log here
  (ensure-directories-exist project-dir)
  (loop for d in *PROJECT-DIRECTORIES*
        do (let ((dir (pathname (format NIL "~A/~A/" project-dir d))))
             ;; TODO: log here
             (format T "create directory: ~A~%" dir)
             (ensure-directories-exist dir))))


(defun create-file-with-template (filename project-name project-dir database template &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
  (format T "create initial files: ~A~%" filename)
  (with-open-file (out (format nil "~A~A" project-dir filename)
                       :direction :output)
    (format out "~A" (funcall (cl-template:compile-template template
                                                            :start-delimiter start-delimiter
                                                            :start-echo-delimiter start-echo-delimiter
                                                            :end-delimiter end-delimiter)
                              `(:project-name ,project-name
                                :project-dir ,project-dir
                                :database ,database)))))


(defun create-initial-files (project-name project-dir database)
  (flet ((read-template (filename)
           (let ((fullpath (asdf:system-relative-pathname
                            :clails
                            (format nil "template/project/~A" filename))))
             (uiop:read-file-string fullpath
                                    :external-format :utf-8))))

    ;; boot
    (create-file-with-template "clails.boot"
                               project-name
                               project-dir
                               database
                               (read-template "clails.boot.tmpl"))

    ;; asdf file
    (create-file-with-template (format nil "~A.asd" project-name)
                               project-name
                               project-dir
                               database
                               (read-template "base.asd.tmpl"))

    ;; appliation
    (create-file-with-template "app/application.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "app/application.lisp.tmpl"))

    ;; config
    (create-file-with-template "app/config/environment.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "app/config/environment.lisp.tmpl"))

    ;; logger
    (create-file-with-template "app/config/logger.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "app/config/logger.lisp.tmpl"))

    ;; database
    (let ((db-template (cond ((eq database :sqlite3)
                              "app/config/database-sqlite3.lisp.tmpl")
                             ((eq database :mysql)
                              "app/config/database-mysql.lisp.tmpl")
                             ((eq database :postgresql)
                              "app/config/database-postgresql.lisp.tmpl")
                             (t (error "unsupported database: ~A" database)))))
      (create-file-with-template "app/config/database.lisp"
                                 project-name
                                 project-dir
                                 database
                                 (read-template db-template)))

    ;; controller
    (create-file-with-template "app/controllers/application-controller.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "app/controllers/application-controller.lisp.tmpl"))


    ;; view package
    (create-file-with-template "app/views/package.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "app/views/packge.lisp.tmpl")
                               :start-delimiter "<%%"
                               :start-echo-delimiter "<%%="
                               :end-delimiter "%%>")

    ;; view
    (create-file-with-template "app/views/index.html"
                               project-name
                               project-dir
                               database
                               (read-template "app/views/index.html.tmpl")
                               :start-delimiter "<%%"
                               :start-echo-delimiter "<%%="
                               :end-delimiter "%%>")

    ;; migration package
    (create-file-with-template "db/package.lisp"
                               project-name
                               project-dir
                               database
                               (read-template "db/package.lisp.tmpl"))))


(defun copy-asset-files (project-dir)
  (let ((src (asdf:system-relative-pathname :clails "template/public/"))
        (dst (merge-pathnames "public/" project-dir)))
    (copy-recursive src dst)))


(defun copy-recursive (src dst)
  "cp -r src dst"
  (format t "copy ~A -> ~A~%" src dst)
  (cond
    ((cl-fad:directory-pathname-p src)
     (ensure-directories-exist dst)
     (dolist (item (cl-fad:list-directory src))
       (let ((relative-name (enough-namestring item src)))
         (copy-recursive item (merge-pathnames relative-name dst)))))

    ((probe-file src)
     (ensure-directories-exist dst)
     (cl-fad:copy-file src dst))

    (t
     (error "Source ~A does not exist" src))))
