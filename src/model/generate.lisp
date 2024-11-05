(in-package #:cl-user)
(defpackage #:clails/model/generate
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-dir*)
  (:import-from #:clails/project/template
                #:template
                #:path)
  (:export #:%generate-model
           #:%generate-migration))
(in-package #:clails/model/generate)


(defun %generate-model (model-name project-name project-dir)
  (let* ((template clails/project/template::model-template)
         (model-file (pathname (format NIL "~A/~A/~A.lisp"
                                       project-dir
                                       (path template)
                                       model-name))))
    
    ; TODO: log
    (format T "generate model file: ~A~%" model-file)
    (with-open-file (out model-file
                         :direction :output
                         :if-exists :error)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :model ,model-name))))))


(defun %generate-migration (model-name project-dir &key type body)
  (let ((template (cond ((eq type :create) clails/project/template::model-migration-create-template)
                        ((eq type :add-column) clails/project/template::model-migration-add-column-template)
                        (t (error "type not supported: ~A" type))))
        (migration-file (pathname (unique-filename model-name project-dir template))))

    ; TODO: log
    (format T "generate migration file: ~A~%" migration-file)
    (with-open-file (out migration-file
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:model-name ,model-name
                                  :body ,body))))))

(defun unique-filename (model-name project-dir template)
  format NIL "~A/~A/~A-~A-~A.lisp"
         project-dir
         (path template)
         (current-datetime)
         (prefix template)
         model-name)

(defun current-datetime ()
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))
                               
    
