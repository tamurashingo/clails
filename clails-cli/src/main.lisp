(in-package #:cl-user)
(defpackage #:clails-cli/main
  (:use #:cl)
  (:export :create-project))
(in-package #:clails-cli/main)

(defparameter *clails-project* "")
(defparameter *clails-directory* "")


(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "config"
    "db"
    "db/migrate"))


(defun create-project (project-name &key project-path (database :sqlite))
  (let ((project-dir (format NIL "~A/~A/" (if (null project-path)
                                              (truename #P"./")
                                              project-path)
                                          project-name)))
    (setf *clails-project* project-name)
    (setf *clails-directory* project-dir)
    (create-directories project-dir)
    (create-initial-files project-name project-dir)))

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
           (pathname (format NIL "~A/~A" project-name (clails-cli/template:path template)))))

    ;; asd file
    (create-file-with-template project-name
                               (dirname clails-cli/template::asd-template)
                               (format NIL "~A.asd" project-name)
                               clails-cli/template::asd-template)
    ;; controller package
    (create-file-with-template project-name
                               (dirname clails-cli/template::controller-package-template)
                               "package.lisp"
                               clails-cli/template::controller-package-template)
    ;; model package
    (create-file-with-template project-name
                               (dirname clails-cli/template::model-package-template)
                               "package.lisp"
                               clails-cli/template::model-package-template)
    ;; view package
    (create-file-with-template project-name
                               (dirname clails-cli/template::view-package-template)
                               "package.lisp"
                               clails-cli/template::view-package-template)
    ))


(defun create-file-with-template (project-name target-dir target-file template)
  (let ((filename (pathname (format NIL "~A/~A" target-dir target-file))))
    (format T "create initial files: ~A~%" filename)
    (with-open-file (out filename
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (clails-cli/template:template template))
                                `(:project-name ,project-name))))))







