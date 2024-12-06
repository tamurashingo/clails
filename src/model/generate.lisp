(in-package #:cl-user)
(defpackage #:clails/model/generate
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-dir*)
  (:import-from #:clails/project/template
                #:template
                #:prefix
                #:path)
  (:export #:%generate-model
           #:%generate-migration))
(in-package #:clails/model/generate)


(defun %generate-model (model-name project-name project-dir &key overwrite)
  (let* ((template clails/project/template::model-template)
         (model-file (pathname (format NIL "~A/~A/~A.lisp"
                                       project-dir
                                       (path template)
                                       model-name))))

    ; TODO: log
    (format T "generate model file: ~A~%" model-file)
    (with-open-file (out model-file
                         :direction :output
                         :if-exists (if overwrite :overwrite
                                                  :error))
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :model ,model-name))))))


(defun %generate-migration (migration-name project-name project-dir)
  (let* ((template clails/project/template::model-migration-template)
         (unique-name (gen-unique-name migration-name))
         (migration-file (pathname (format NIL "~A/~A/~A.lisp" project-dir (path template) unique-name))))

    ; TODO: log
    (format T "generate migration file: ~A~%" migration-file)
    (with-open-file (out migration-file
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :migration-name ,unique-name))))))

(defun gen-unique-name (base-name)
  (format NIL "~A_~A"
          (current-datetime)
          base-name))

(defun current-datetime ()
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))

