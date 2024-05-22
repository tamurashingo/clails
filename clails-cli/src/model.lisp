(in-package #:cl-user)
(defpackage #:clails-cli/model
  (:use #:cl))
(in-package #:clails-cli/model)

(defun current-datetime ()
  (multiple-value-bind
        (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))

(defun unique-filename (model-name template)
  (format NIL "~A/~A/~A-~A-~A.lisp"
          clails-cli/main::*clails-directory*
          (clails-cli/template:path template)
          (current-datetime)
          (clails-cli/template:prefix template)
          model-name))


(defun generate-model (model-name)
  (let* ((template clails-cli/template::model-template)
         (model-file (pathname (format NIL "~A/~A/~A.lisp"
                                        clails-cli/main::*clails-directory*
                                        (clails-cli/template:path template)
                                        model-name))))
    (format T "generate model file: ~A~%" model-file)
    (with-open-file (out model-file
                         :direction :output
                         :if-exists :error)
      (format out "~A" (funcall (cl-template:compile-template (clails-cli/template:template template))
                                `(:project-name ,clails-cli/main::*clails-project*
                                  :model ,model-name))))))


(defun generate-migration (model-name &key type body)
  (let* ((template (cond ((eq type :create) clails-cli/template::model-migration-create-template)
                         ((eq type :add-column) clails-cli/template::model-migration-addcolumn-template)
                         (t error "cmd error")))
         (migration-file (pathname (unique-filename model-name template))))
    (format T "generate migration file: ~A~%" migration-file)
    (with-open-file (out migration-file
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (clails-cli/template:template template))
                                `(:model-name ,model-name
                                  :body ,body))))))


(defun generate-model-create (model-name body)
  (generate-model model-name)
  (generate-migration model-name :type :create :body body))q

(defun generate-model-add-column (model-name body)
  (generate-model model-name :type :add-column :body body))


