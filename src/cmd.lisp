(in-package #:cl-user)
(defpackage #:clails/cmd
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-dir*)
  (:import-from #:clails/project/generate
                #:gen/model
                #:gen/migration
                #:gen/view
                #:gen/controller
                #:gen/scaffold)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate)
  (:import-from #:clails/controller/base-controller
                #:initialize-routing-tables)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/util
                #:function-from-string)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:generate/view
           #:generate/controller
           #:generate/scaffold
           #:db/create
           #:db/migrate
           #:console
           #:server
           #:stop))
(in-package #:clails/cmd)

(defparameter *app* nil)
(defparameter *handler* nil)


(defun create-project (project-name &key project-path (database :sqlite3))
  (let ((project-dir (pathname (format NIL "~A/~A/" (if (null project-path)
                                                        (truename #P"./")
                                                        project-path)
                                                    project-name))))
    (clails/project/project:create-project project-name project-dir database)))


(defun generate/model (model-name &key (no-overwrite T) (no-migration NIl))
  (gen/model model-name :overwrite (not overwrite))
  (when (not no-migration)
    (gennerate/migration model-name)))

(defun generate/migration (migration-name)
  (gen/migration migration-name))

(defun generate/view (view-name &key (no-overwrite T))
  (gen/view view-name :overwrite (not no-overwrite)))

(defun generate/controller (controller-name &key (no-overwrite T))
  (gen/controller controller-name :overwrite (not no-overwrite)))

(defun generate/scaffold (name &key (no-overwrite T))
  (gen/scaffold name :overwrite (not no-overwrite)))


(defun db/create ()
  (db-create))

(defun db/migrate ()
  (db-migrate *project-dir*))


(defun console ()
  (error "Not yet implemented"))

(defun server ()
  (initialize-routing-tables)
  (setf *app*
    (lack:builder
      *lack-middleware-clails-controller*
      (:static :path "/"
               :root #P"./public/")
      *app*))
  (setf *handler*
        (clack:clackup *app*
                       :debug nil
                       :use-thread T))

  (call-startup-hooks)
  (clack::with-handle-interrupt
      (lambda ()
        (stop))
    (loop)))

(defun stop ()
  (when *handler*
    (call-shutdown-hooks)
    (clack:stop *handler*)
    (setf *handler* nil)))

(defun call-startup-hooks ()
  (loop for fn in clails/environment:*startup-hooks*
        do (format t "running startup hook...~A~%" fn)
        do (funcall (etypecase fn
                      (string
                       (function-from-string fn))
                      (function fn)))))

(defun call-shutdown-hooks ()
  (loop for fn in clails/environment:*shutdown-hooks*
        do (format t "running shutdown hook...~A~%" fn)
        do (funcall (etypecase fn
                      (string
                       (function-from-string fn))
                      (function fn)))))

