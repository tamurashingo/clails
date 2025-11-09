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
                #:migrate-up-version
                #:migrate-down-version
                #:db-rollback
                #:db-status
                #:db-seed)
  (:import-from #:clails/controller/base-controller
                #:initialize-routing-tables)
  (:import-from #:clails/middleware
                #:*clails-middleware-stack*
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/util
                #:function-from-string)
  (:import-from #:clails/test
                #:run-suite-tests
                #:run-suite-tests-by-tags
                #:run-suite-tests-by-packages
                #:list-all-tags
                #:list-test-packages
                #:list-tests-by-tag
                #:list-tests-by-package
                #:ensure-test-modules-loaded)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool)
  (:import-from #:clails/model/base-model
                #:initialize-table-information)
  (:export #:create-project
           #:generate/model
           #:generate/migration
           #:generate/view
           #:generate/controller
           #:generate/scaffold
           #:db/create
           #:db/migrate
           #:db/migrate-up
           #:db/migrate-down
           #:db/rollback
           #:db/seed
           #:db/status
           #:console
           #:server
           #:stop
           #:test))
(in-package #:clails/cmd)

(defparameter *app* nil
  "Application instance for the web server.")

(defparameter *handler* nil
  "Server handler instance returned by clackup.")

(defparameter *swank-server* nil
  "Swank server instance for development.")

(defun start-swank-server (port)
  "Start swank server on the specified port.

   @param port [string] Port number for swank server
   @return [t] Swank server start result
   "
  (unless *swank-server*
    (handler-case
        (let ((swank-port (parse-integer port)))
          (setf *swank-server*
                (funcall (intern "CREATE-SERVER" :swank)
                         :port swank-port
                         :dont-close t))
          (format t "Swank server started on port ~A~%" swank-port))
      (error (e)
        (format *error-output* "Failed to start swank server: ~A~%" e)))))


(defun create-project (project-name &key project-path (database :sqlite3))
  "Create a new clails project with the specified name and database.

   @param project-name [string] Name of the project to create
   @param project-path [string] Directory path where project will be created (default: current directory)
   @param database [keyword] Database type to use (:sqlite3, :mysql, :postgresql, default: :sqlite3)
   @return [t] Project creation result
   "
  (let ((project-dir (pathname (format NIL "~A/~A/" (if (null project-path)
                                                        (truename #P"./")
                                                        project-path)
                                                    project-name))))
    (clails/project/project:create-project project-name project-dir database)))


(defun generate/model (model-name &key (no-overwrite T) (no-migration nil))
  "Generate a model file and optionally a migration file.

   @param model-name [string] Name of the model to generate
   @param no-overwrite [boolean] If T, do not overwrite existing files (default: T)
   @param no-migration [boolean] If T, skip migration file generation (default: nil)
   @return [t] Generation result
   "
  (gen/model model-name :overwrite (not no-overwrite))
  (when (not no-migration)
    (generate/migration model-name)))

(defun generate/migration (migration-name)
  "Generate a migration file with the specified name.

   @param migration-name [string] Name of the migration to generate
   @return [t] Generation result
   "
  (gen/migration migration-name))

(defun generate/view (view-name &key (no-overwrite T))
  "Generate a view template file.

   @param view-name [string] Name of the view to generate
   @param no-overwrite [boolean] If T, do not overwrite existing files (default: T)
   @return [t] Generation result
   "
  (gen/view view-name :overwrite (not no-overwrite)))

(defun generate/controller (controller-name &key (no-overwrite T))
  "Generate a controller file.

   @param controller-name [string] Name of the controller to generate
   @param no-overwrite [boolean] If T, do not overwrite existing files (default: T)
   @return [t] Generation result
   "
  (gen/controller controller-name :overwrite (not no-overwrite)))

(defun generate/scaffold (name &key (no-overwrite T))
  "Generate scaffold (model, view, controller) files.

   @param name [string] Name for the scaffold
   @param no-overwrite [boolean] If T, do not overwrite existing files (default: T)
   @return [t] Generation result
   "
  (gen/scaffold name :overwrite (not no-overwrite)))


(defun db/create (&key (env :development))
  "Create the database for the specified environment.

   @param env [keyword] Environment name (:development, :test, :production, default: :development)
   @return [t] Database creation result
   "
  (db-create))

(defun db/migrate (&key (env :development) version)
  "Run pending database migrations for the specified environment.

   @param env [keyword] Environment name (:development, :test, :production, default: :development)
   @param version [string] Migration name to migrate up to (optional). If nil, runs all pending migrations.
   @return [t] Migration execution result
   "
  (db-migrate :version version))

(defun db/status ()
  "Display the status of database migrations.

   @return [t] Migration status information
   "
  (db-status))

(defun db/migrate-up (version &key (env :development))
  "Run a specific database migration for the specified environment.

   @param version [string] Migration name to apply (required)
   @param env [keyword] Environment name (:development, :test, :production, default: :development)
   @return [t] Migration execution result
   @condition error Version parameter is required
   "
  (when (null version)
    (error "VERSION parameter is required for db:migrate:up"))
  (migrate-up-version version))

(defun db/migrate-down (version &key (env :development))
  "Rollback a specific database migration for the specified environment.

   @param version [string] Migration name to rollback (required)
   @param env [keyword] Environment name (:development, :test, :production, default: :development)
   @return [t] Rollback execution result
   @condition error Version parameter is required
   "
  (when (null version)
    (error "VERSION parameter is required for db:migrate:down"))
  (migrate-down-version version))

(defun db/rollback (&key (env :development) (step 1))
  "Rollback database migrations for the specified environment.

   @param env [keyword] Environment name (:development, :test, :production, default: :development)
   @param step [integer] Number of migrations to rollback (default: 1)
   @return [t] Rollback execution result
   "
  (db-rollback :step step))

(defun db/seed ()
  "Seed the database with initial data.

   @return [t] Seeding execution result
   "
  (startup-connection-pool)
  (initialize-table-information)
  (unwind-protect
      (db-seed)
    (shutdown-connection-pool)))


(defun console ()
  "Start an interactive console for the application.

   Not yet implemented.

   @condition error Not yet implemented
   "
  (error "Not yet implemented"))

(defun server (&key (port "5000") (bind "127.0.0.1") swank-port)
  "Start the web server with the specified port and bind address.

   Initializes routing tables, builds middleware stack, starts the server,
   and calls startup hooks. Blocks until server is stopped.

   @param port [string] Port number to listen on (default: \"5000\")
   @param bind [string] IP address to bind to (default: \"127.0.0.1\")
   @param swank-port [string] Port number for swank server (nil to disable)
   @return [nil] Does not return until server is stopped
   "
  (when swank-port
    (start-swank-server swank-port))

  (initialize-routing-tables)
  (let* ((args (append *clails-middleware-stack* (list *app*)))
         (builder `(lack:builder ,@args)))
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
  "Stop the running web server.

   Calls shutdown hooks and stops the server handler.

   @return [nil] Always returns nil
   "
  (when *handler*
    (call-shutdown-hooks)
    (clack:stop *handler*)
    (setf *handler* nil))
  (when *swank-server*
    (handler-case
        (progn
          (funcall (intern "STOP-SERVER" :swank) *swank-server*)
          (format t "Swank server stopped~%"))
      (error (e)
        (format *error-output* "Failed to stop swank server: ~A~%" e)))
    (setf *swank-server* nil)))

(defun call-startup-hooks ()
  "Execute all registered startup hooks.

   Iterates through *startup-hooks* and calls each function.
   Hooks can be specified as function objects or strings.

   @return [t] Completion status
   "
  (loop for fn in clails/environment:*startup-hooks*
        do (format t "running startup hook...~A~%" fn)
        do (funcall (etypecase fn
                      (string
                       (function-from-string fn))
                      (function fn)))))

(defun call-shutdown-hooks ()
  "Execute all registered shutdown hooks.

   Iterates through *shutdown-hooks* and calls each function.
   Hooks can be specified as function objects or strings.

   @return [t] Completion status
   "
  (loop for fn in clails/environment:*shutdown-hooks*
        do (format t "running shutdown hook...~A~%" fn)
        do (funcall (etypecase fn
                      (string
                       (function-from-string fn))
                      (function fn)))))

(defun test (&key packages tags exclude-tags list-tags list-packages
                  list-tests-tag list-tests-pkg asd-systems
                  (style :spec))
  "Run tests with optional filtering.

   @param packages [list] Package names to test (exact match)
   @param tags [list] Tags to include
   @param exclude-tags [list] Tags to exclude
   @param list-tags [boolean] List all available tags
   @param list-packages [boolean] List all available packages
   @param list-tests-tag [keyword] List tests with specific tag
   @param list-tests-pkg [string] List tests in specific package
   @param asd-systems [list] Specific ASD system names to load instead of auto-detection
   @param style [keyword] Reporter style (default: :spec)
   @return [boolean] Test results
   "
  ;; Ensure test modules are loaded before any test operations
  (ensure-test-modules-loaded :asd-systems asd-systems)

  (cond
    ;; List mode operations
    (list-tags
     (list-all-tags)
     t)
    (list-packages
     (list-test-packages)
     t)
    (list-tests-tag
     (list-tests-by-tag list-tests-tag)
     t)
    (list-tests-pkg
     (list-tests-by-package list-tests-pkg)
     t)
    ;; Run tests
    (t
     ;; Initialize database connection pool
     (startup-connection-pool)
     (initialize-table-information)
     (unwind-protect
         (run-suite-tests :tags tags
                          :excluded-tags exclude-tags
                          :packages packages
                          :style style)
       ;; Cleanup: shutdown connection pool
       (shutdown-connection-pool)))))

