(in-package #:cl-user)
(defpackage #:clails-cli/model
  (:use #:cl)
  (:import-from #:clails-cli/template
                #:path
                #:prefix
                #:template)
  (:export #:generate-model-exp
           #:generate-migration-exp))
(in-package #:clails-cli/model)


(defun generate-model-exp (model-name project-name project-directory)
  (let* ((template clails-cli/template::model-template)
         (model-file (pathname (format NIL "~A/~A/~A.lisp"
                                       project-directory
                                       (path template)
                                       model-name))))
    (format T "generate model file: ~A~%" model-file)
    (with-open-file (out model-file
                         :direction :output
                         :if-exists :error)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-directory
                                  :model ,model-name))))))

(defun generate-migration-exp (model-name project-directory &key type body)
  (let* ((template (cond ((eq type :create) clails-cli/template::model-migration-create-template)
                         ((eq type :add-column) clails-cli/template::model-migration-addcolumn-template)
                         (t error "cmd error")))
         (migration-file (pathname (unique-filename model-name project-directory template))))
    (format T "generate migration file: ~A~%" migration-file)
    (with-open-file (out migration-file
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:model-name ,model-name
                                  :body ,body))))))

(defun current-datetime ()
  (multiple-value-bind
        (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))

(defun unique-filename (model-name project-directory template)
  (format NIL "~A/~A/~A-~A-~A.lisp"
          project-directory
          (path template)
          (current-datetime)
          (prefix template)
          model-name))

