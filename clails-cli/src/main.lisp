(in-package #:cl-user)
(defpackage #:clails-cli/main
  (:nicknames #:clails-cli)
  (:use #:cl)
  (:import-from #:clails-cli/project
                #:create-project-exp)
  (:import-from #:clails-cli/model
                #:generate-model-exp
                #:generate-migration-exp)
  (:export #:create-project
           #:generate-model
           #:generate-migration))
(in-package #:clails-cli/main)

(defparameter *clails-project* "")
(defparameter *clails-directory* "")


(defun create-project (project-name &key project-path (database :sqlite))
  (let ((project-dir (pathname (format NIL "~A/~A/" (if (null project-path)
                                              (truename #P"./")
                                              project-path)
                                          project-name))))
    (setf *clails-project* project-name)
    (setf *clails-directory* (namestring project-dir))
    (create-project-exp project-name project-dir database)))


(defun generate-model (model-name body &key no-migration)
  (generate-model-exp model-name *clails-project* *clails-directory*)
  (when (not no-migration)
    (generate-migration-exp model-name *clails-directory* :type :create :body body)))


(defun generate-migration (model-name &key type body)
  (when (not (or (eq type :create)
                 (eq type :add-column)))
    (error "type not supported:~A" type))
  (generate-migration-exp model-name *clails-directory* :type type :body body))
