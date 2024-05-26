(in-package #:cl-user)
(defpackage #:clails-cli/project
  (:use #:cl)
  (:import-from #:clails-cli/template
                #:path
                #:template)
  (:export #:create-project-exp))
(in-package #:clails-cli/project)

(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "config"
    "db"
    "db/migrate"))

(defun create-project-exp (project-name project-dir database)
    (create-directories project-dir)
    (create-initial-files project-name (namestring project-dir)))

(defun create-directories (project-dir)
  ;; TODO: log here
  (ensure-directories-exist project-dir)
  (loop for d in *PROJECT-DIRECTORIES*
        do (let ((dir (pathname (format NIL "~A/~A/" project-dir d))))
             ;; TODO: log here
             (format T "create directory: ~A~%" dir)
             (ensure-directories-exist dir))))

(defun create-initial-files (project-name project-dir)
  (flet ((dirname (template)
           (pathname (format NIL "~A/~A" project-name (path template)))))

    ;; asd file
    (create-file-with-template project-name
                               project-dir
                               (dirname clails-cli/template::asd-template)
                               (format NIL "~A.asd" project-name)
                               clails-cli/template::asd-template)
    ;; package
    (create-file-with-template project-name
                               project-dir
                               (dirname clails-cli/template::package-template)
                               "package.lisp"
                               clails-cli/template::package-template)

    ;; controller package
    (create-file-with-template project-name
                               project-dir
                               (dirname clails-cli/template::controller-package-template)
                               "package.lisp"
                               clails-cli/template::controller-package-template)
    ;; model package
    (create-file-with-template project-name
                               project-dir
                               (dirname clails-cli/template::model-package-template)
                               "package.lisp"
                               clails-cli/template::model-package-template)
    ;; view package
    (create-file-with-template project-name
                               project-dir
                               (dirname clails-cli/template::view-package-template)
                               "package.lisp"
                               clails-cli/template::view-package-template)
    ))

(defun create-file-with-template (project-name project-dir target-dir target-file template)
  (let ((filename (pathname (format NIL "~A/~A" target-dir target-file))))
    (format T "create initial files: ~A~%" filename)
    (with-open-file (out filename
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :project-dir ,project-dir))))))
