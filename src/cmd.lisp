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
                #:migrate-all)
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


(defun generate/model (model-name body &key (no-migration NIl))
  (%generate-model model-name *project-name* *project-dir*)
  (when (not no-migration)
    (%generate-migration model-name *project-dir* :type :create :body body)))

(defun generate/migration (model-name &key type body)
  (when (not (or (eq type :create)
                 (eq type :add-column)))
    (error "type not supppoerted: ~A" type))
  (%generate-migration model-name *project-dir* :type type :body body))


(defun db/migrate ()
  (migrate-all))
