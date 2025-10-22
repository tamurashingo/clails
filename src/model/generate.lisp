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
  (:import-from #:clails/logger
                #:log-package.info)
  (:export #:%generate-model
           #:%generate-migration))
(in-package #:clails/model/generate)


(defun %generate-model (model-name project-name project-dir &key overwrite)
  "Generate a model file from template.
   
   @param model-name [string] Name of the model to generate
   @param project-name [string] Name of the project
   @param project-dir [pathname] Project directory path
   @param overwrite [boolean] If T, overwrite existing file; otherwise signal error
   "
  (let* ((template clails/project/template::model-template)
         (model-file (pathname (format NIL "~A/~A/~A.lisp"
                                       project-dir
                                       (path template)
                                       model-name))))

    (log-package.info (format nil "generate model file: ~A" model-file))
    (with-open-file (out model-file
                         :direction :output
                         :if-exists (if overwrite :overwrite
                                                  :error))
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :model ,model-name))))))


(defun %generate-migration (migration-name project-name project-dir)
  "Generate a migration file with unique timestamp prefix.
   
   @param migration-name [string] Base name for the migration
   @param project-name [string] Name of the project
   @param project-dir [pathname] Project directory path
   "
  (let* ((template clails/project/template::model-migration-template)
         (unique-name (gen-unique-name migration-name))
         (migration-file (pathname (format NIL "~A/~A/~A.lisp" project-dir (path template) unique-name))))

    (log-package.info (format nil "generate migration file: ~A" migration-file))
    (with-open-file (out migration-file
                         :direction :output)
      (format out "~A" (funcall (cl-template:compile-template (template template))
                                `(:project-name ,project-name
                                  :migration-name ,unique-name))))))

(defun gen-unique-name (base-name)
  "Generate unique migration name with timestamp prefix.
   
   @param base-name [string] Base name for the migration
   @return [string] Unique name in format YYYYMMDDHHMMSS_basename
   "
  (format NIL "~A_~A"
          (current-datetime)
          base-name))

(defun current-datetime ()
  "Get current datetime as formatted string.
   
   @return [string] Current datetime in format YYYYMMDDHHMMSS
   "
  (multiple-value-bind (sec min hour day mon year)
      (get-decoded-time)
    (format NIL "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mon day hour min sec)))

