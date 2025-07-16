(in-package #:cl-user)
(defpackage #:clails/project/project
  (:use #:cl)
  (:import-from #:clails/project/template
                #:<template>
                #:template
                #:path)
  (:export #:%create-project))

(in-package #:clails/project/project)

(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "config"
    "db"
    "db/migrate"
    "tmp"))

(defun %create-project (project-name project-dir database)
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

(defun create-file-with-template (project-name project-dir database target-dir target-file template)
  (let ((filename (pathname (format NIL "~A/~A" target-dir target-file))))
    (format T "create initial files: ~A~%" filename)
    (with-open-file (out filename
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :project-dir ,project-dir
                                  :database ,database))))))

(defun create-initial-files (project-name project-dir database)
  (flet ((%dirname (template)
            (pathname (format NIL "~A/~A" project-dir (path template)))))

    ;; project file
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::project-template)
                               "clails.boot"
                               clails/project/template::project-template)

    ;; asd file
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::asd-template)
                               (format NIL "~A.asd" project-name)
                               clails/project/template::asd-template)
    ;; package
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::package-template)
                               "package.lisp"
                               clails/project/template::package-template)

    ;; controller package
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::app/controller/package-template)
                               "package.lisp"
                               clails/project/template::app/controller/package-template)

    ;; model package
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::app/model/package-template)
                               "package.lisp"
                                clails/project/template::app/model/package-template)

    ;; view package
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::app/view/package-template)
                               "package.lisp"
                               clails/project/template::app/view/package-template)

    ;; config
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::config/package-template)
                               "package.lisp"
                               clails/project/template::config/package-template)

    ;; config/environment
    (create-file-with-template project-name
                               project-dir
                               database
                               (%dirname clails/project/template::config/environment-template)
                               "environment.lisp"
                               clails/project/template::config/environment-template)

    ;; config/database
    (let ((db-template (cond ((eq database :sqlite3)
                               clails/project/template::config/database-sqlite-template)
                             ((eq database :mysql)
                               clails/project/template::config/database-mysql-template)
                             ((eq database :postgresql)
                               clails/project/template::config/database-postgresql-template)
                             (t (error "unsupported database: ~A" database)))))
      (create-file-with-template project-name
                                 project-dir
                                 database
                                 (%dirname db-template)
                                 "database.lisp"
                                 db-template))
  ))

