(in-package #:cl-user)
(defpackage #:clails/cmd
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten)
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
                #:db-migrate
                #:db-status)
  (:import-from #:clails/controller/base-controller
                #:initialize-routing-tables)
  (:import-from #:clails/middleware
                #:*clails-middleware-stack*
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
           #:db/status
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


(defun generate/model (model-name &key (no-overwrite T) (no-migration nil))
  (gen/model model-name :overwrite (not no-overwrite))
  (when (not no-migration)
    (generate/migration model-name)))

(defun generate/migration (migration-name)
  (gen/migration migration-name))

(defun generate/view (view-name &key (no-overwrite T))
  (gen/view view-name :overwrite (not no-overwrite)))

(defun generate/controller (controller-name &key (no-overwrite T))
  (gen/controller controller-name :overwrite (not no-overwrite)))

(defun generate/scaffold (name &key (no-overwrite T))
  (gen/scaffold name :overwrite (not no-overwrite)))


(defun db/create (&key (env :development))
  (db-create))

(defun db/migrate (&key (env :development))
  (db-migrate))

(defun db/status ()
  (db-status))

(defun db/rollback ()
  nil)


(defun console ()
  (error "Not yet implemented"))

(defun server (&key (port "5000") (bind "127.0.0.1"))
  (initialize-routing-tables)
  (let* ((args (append *clails-middleware-stack* (list *app*)))
         (builder `(lack:builder ,@(args))))
    (setf *app* (eval builder)))

  (setf *handler*
        (clack:clackup *app*
                       :debug nil
                       :use-thread T
                       :port (parse-integer port)
                       :address bind))

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

