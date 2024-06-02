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


(defun create-project (project-name &key project-path (database :sqlite3))
  (let ((project-dir (pathname (format NIL "~A/~A/" (if (null project-path)
                                              (truename #P"./")
                                              project-path)
                                          project-name))))
    (create-project-exp project-name project-dir database)))


