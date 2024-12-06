(in-package #:cl-user)
(defpackage #:clails/cmd
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-dir*)
  (:import-from #:clails/project/project
                #:%create-project)
  (:import-from #:clails/model/generate
                #:%generate-model
                #:%generate-migration)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate)
  (:export #:create-project
           #:generate/model
           #:generate/migration))
(in-package #:clails/cmd)

(defun create-project (project-name &key project-path (database :sqlite3))
  (let ((project-dir (pathname (format NIL "~A/~A/" (if (null project-path)
                                                        (truename #P"./")
                                                        project-path)
                                                    project-name))))
    (%create-project project-name project-dir database)))


(defun generate/model (model-name &key overwrite (no-migration NIl))
  (%generate-model model-name *project-name* *project-dir* :overwrite overwrite)
  (when (not no-migration)
    (%generate-migration model-name *project-name* *project-dir*)))

(defun generate/migration (migration-name)
  (%generate-migration migration-name *project-name* *project-dir*))


(defun db/create ()
  (db-create))

(defun db/migrate ()
  (db-migrate *project-dir*))

