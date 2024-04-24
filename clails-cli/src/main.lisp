(in-package #:cl-user)
(defpackage #:clails-cli
  (:use #:cl)
  (:export :create-project))
(in-package #:clails-cli)


(defparameter *PROJECT-DIRECTORIES*
  '("app"
    "app/controllers"
    "app/models"
    "app/views"
    "config"
    "db"))

(defun create-project (project-name &key project-path (database :sqlite))
  (let ((project-dir (format NIL "~A/~A/" (if (null project-path)
                                              (truename #P"./")
                                              project-path)
                                          project-name)))
    (create-directories project-dir)))

(defun create-directories (project-dir)
  ;; TODO: log here
  (ensure-directories-exist project-dir)
  (loop for d in *PROJECT-DIRECTORIES*
        do (let ((dir (format NIL "~A/~A/" project-dir d)))
             ;; TODO: log here
             (ensure-directories-exist dir))))

