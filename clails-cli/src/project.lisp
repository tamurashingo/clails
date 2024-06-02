(in-package #:cl-user)
(defpackage #:clails-cli/project
  (:use #:cl)
  (:import-from #:clails-cli/template
                #:path
                #:template)
  (:import-from #:clails-cli/templates/project-template
                #:asd-template
                #:package-template
                #:controller-package-template
                #:model-package-template
                #:view-package-template)
  (:import-from #:clails-cli/templates/cli-template
                #:cli-main-template)
  (:export #:create-project-exp))
(in-package #:clails-cli/project)

(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "cli"
    "config"
    "db"
    "db/migrate"
    "tmp"))

(defun create-project-exp (project-name project-dir database)
    (create-directories project-dir)
    (create-initial-files project-name (namestring project-dir) database))

(defun create-directories (project-dir)
  ;; TODO: log here
  (ensure-directories-exist project-dir)
  (loop for d in *PROJECT-DIRECTORIES*
        do (let ((dir (pathname (format NIL "~A/~A/" project-dir d))))
             ;; TODO: log here
             (format T "create directory: ~A~%" dir)
             (ensure-directories-exist dir))))

(defun create-initial-files (project-name project-dir database)
  (flet ((dirname (template)
           (pathname (format NIL "~A/~A" project-name (path template)))))

    ;; asd file
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::asd-template)
                               (format NIL "~A.asd" project-name)
                               clails-cli/templates/project-template::asd-template)
    ;; cli
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/cli-template::cli-main-template)
                               (format NIL "~A-cli.lisp" project-name)
                               clails-cli/templates/cli-template::cli-main-template)

    ;; package
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::package-template)
                               "package.lisp"
                               clails-cli/templates/project-template::package-template)

    ;; controller package
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::app/controller/package-template)
                               "package.lisp"
                               clails-cli/templates/project-template::app/controller/package-template)
    ;; model package
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::app/model/package-template)
                               "package.lisp"
                               clails-cli/templates/project-template::app/model/package-template)
    ;; view package
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::app/view/package-template)
                               "package.lisp"
                               clails-cli/templates/project-template::app/view/package-template)

    ;; config
    (create-file-with-template project-name
                               project-dir
                               database
                               (dirname clails-cli/templates/project-template::config/package-template)
                               "package.lisp"
                               clails-cli/templates/project-template::config/package-template)

    ;; config/database
    (let ((db-template (cond ((eq database :mysql)
                              clails-cli/templates/project-template::config/database-mysql-template)
                             ((or (eq database :postgresql)
                                  (eq database :postgres))
                              clails-cli/templates/project-template::config/database-postgresql-template)
                             (t clails-cli/templates/project-template::config/database-sqlite-template))))
      ;; config/database
      (create-file-with-template project-name
                                 project-dir
                                 database
                                 (dirname db-template)
                                 "database.lisp"
                                 db-template))

    ))

(defun create-file-with-template (project-name project-dir database target-dir target-file template)
  (let ((filename (pathname (format NIL "~A/~A" target-dir target-file))))
    (format T "create initial files: ~A~%" filename)
    (with-open-file (out filename
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :project-dir ,project-dir
                                  :database ,database))))))
