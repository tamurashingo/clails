(in-package #:cl-user)
(defpackage #:clails/environment
  (:use #:cl)
  (:export #:*project-name*
           #:*project-dir*
           #:*project-environment*
           #:*database-config*
           #:*database-type*
           #:*migration-base-dir*
           #:*task-base-dir*
           #:*connection-pool*
           #:*routing-tables*
           #:*startup-hooks*
           #:*shutdown-hooks*
           #:*default-lock-mode*
           #:*sqlite3-busy-timeout*
           #:*sqlite3-lock-retry-count*
           #:*%sqlite3-transaction-mode*
           #:*%sqlite3-lock-module-loaded*
           #:*%table-information-initialized*
           #:*%query-initialization-callbacks*
           #:<database-type>
           #:<database-type-mysql>
           #:<database-type-postgresql>
           #:<database-type-sqlite3>
           #:<database-type-dummy>
           #:set-environment
           #:add-startup-hook
           #:add-shutdown-hook
           #:clails-framework-version
           #:warn-if-framework-version-mismatch
           #:resolve-project-environment))
(in-package #:clails/environment)

(defclass <database-type> ()
  ((database-type :initform nil
                  :accessor database-type
                  :documentation "Database type identifier"))
  (:documentation "Base class for database type specifications."))

(defclass <database-type-mysql> (<database-type>)
  ((database-type :initform :mysql))
  (:documentation "MySQL database type specification."))

(defclass <database-type-postgresql> (<database-type>)
  ((database-type :initform :postgresql))
  (:documentation "PostgreSQL database type specification."))

(defclass <database-type-sqlite3> (<database-type>)
  ((database-type :initform :sqlite3))
  (:documentation "SQLite3 database type specification."))

(defclass <database-type-dummy> (<database-type>)
  ((database-type :initform :dummy))
  (:documentation "Dummy database type specification for testing."))


(defvar *project-name* ""
  "Project name. Set in app/config/environment.lisp.")

(defvar *project-dir* ""
  "Project directory. Set at startup.")

(defvar *project-environment* :develop
  "Specify one of :develop, :test, or :production. Can be overridden in app/config/environment.lisp.")

(defvar *database-config* nil
  "Holds database connection information, etc. Set in app/config/database.lisp.")

(defvar *database-type* nil
  "Holds an instance of <database-type> to specify the database in use. Set in app/config/database.lisp.")

(defvar *migration-base-dir* ""
  "The base path for directories where migration files are placed. Usually set to *project-dir*. (May be set to a different directory for testing, etc.)")

(defvar *task-base-dir* ""
  "The base path for directories where task files are placed. Usually set to *project-dir*. (May be set to a different directory for testing, etc.)")

(defvar *connection-pool* nil
  "Database connection pool. Created when the application server statts and destroyed when it shuts down.")

(defvar *routing-tables*
  '((:path "/"
     :controller "clails/controller/base-controller:<default-controller>"))
  "Application routing table configuration.

   Each route entry is a plist with the following properties:

   Required properties:
   - :path [string]
     URI path pattern. Supports parameter placeholders like /users/:id
     Examples: \"/\", \"/users/:id\", \"/blog/:blog-id/comment/:comment-id\"

   - :controller [string]
     Fully qualified controller class name in format \"package::<class-name>\"
     Example: \"myapp/controllers/users:<user-controller>\"

   Optional properties (for custom routing patterns):
   - :scanner [string]
     Custom regex pattern string for matching request paths.
     When specified, this takes highest priority over automatic pattern generation.
     Examples: \"^/spa/.*$\" (catch-all), \"^/users/([0-9]+)$\" (numeric ID only)

   - :keys [list of strings]
     List of URL parameter names to extract from the matched path.
     Used with :scanner to specify which capture groups correspond to parameters.
     If :scanner is specified without :keys, keys defaults to NIL.
     Example: '(\"id\" \"post-id\")

   - :generate-scanner [function designator]
     Function to generate :scanner and :keys dynamically.
     Called with the entire route entry as argument.
     Must return a plist with :scanner (string) and :keys (list).
     Only used if :scanner is not specified.
     Example: (lambda (route-entry)
                (let ((path (getf route-entry :path)))
                  (list :scanner (format nil \"^~A.*$\" path)
                        :keys nil)))

   Priority order for scanner generation:
   1. :scanner (highest priority)
   2. :generate-scanner (only if :scanner not present)
   3. Default behavior using create-scanner-from-uri-path

   Example configurations:

   ;; Default behavior - automatic pattern generation
   (:path \"/users/:id\" :controller \"myapp/controllers/users:<user-controller>\")

   ;; Custom scanner for catch-all routes (SPA)
   (:path \"/spa/*\"
    :controller \"myapp/controllers/spa:<spa-controller>\"
    :scanner \"^/spa/.*$\")

   ;; Custom scanner with parameter extraction
   (:path \"/static/*\"
    :controller \"myapp/controllers/static:<static-controller>\"
    :scanner \"^/static/(.*)$\"
    :keys (\"filepath\"))

   ;; Custom scanner generator function
   (:path \"/api/*\"
    :controller \"myapp/controllers/api:<api-controller>\"
    :generate-scanner (lambda (route-entry)
                        (let ((path (getf route-entry :path)))
                          (list :scanner \"^/api/.*$\"
                                :keys nil))))

   Set in app/config/environment.lisp.")

(defvar *startup-hooks*
  '("clails/model/connection:startup-connection-pool")
  "List of functions (or function-name strings) to run at application startup,
   in list order. Use add-startup-hook to append to this list so registration
   order matches execution order; do not push onto it directly, since push
   prepends and would run the newly added hook before the framework's own
   default hooks (and before any hook registered earlier).")

(defvar *shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool")
  "List of functions (or function-name strings) to run at application shutdown,
   in list order. Use add-shutdown-hook to append to this list so registration
   order matches execution order; do not push onto it directly, since push
   prepends and would run the newly added hook before the framework's own
   default hooks (and before any hook registered earlier).")

(defvar *default-lock-mode* :for-update
  "Default lock mode for with-locked-transaction macro.

   Possible values:
   - :for-update        - Exclusive lock (default) - PostgreSQL/MySQL
   - :for-share         - Shared lock - PostgreSQL/MySQL
   - :for-no-key-update - PostgreSQL only
   - :for-key-share     - PostgreSQL only
   - :immediate         - SQLite3 only (BEGIN IMMEDIATE)
   - :exclusive         - SQLite3 only (BEGIN EXCLUSIVE)

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *sqlite3-busy-timeout* 50
  "SQLite3 busy timeout in milliseconds.

   When SQLite3 encounters a locked database, it will wait up to this
   many milliseconds before returning SQLITE_BUSY error.
   Default is 50ms.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *sqlite3-lock-retry-count* 3
  "Number of retry attempts for SQLite3 locked database errors.

   When BEGIN IMMEDIATE fails due to database lock, the transaction
   will be retried up to this many times with exponential backoff.
   Default is 3 retries.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *%sqlite3-transaction-mode* nil
  "SQLite3 transaction mode for the current dynamic context.

   This is a special variable used to pass the transaction mode
   to the begin-transaction method override in sqlite3-lock module.

   Possible values:
   - nil          - Normal transaction (BEGIN TRANSACTION)
   - :immediate   - Immediate lock (BEGIN IMMEDIATE)
   - :exclusive   - Exclusive lock (BEGIN EXCLUSIVE)

   This variable is set by with-locked-transaction macro and should not
   be set directly by user code.

   NOTE: The leading % in the name marks this as an internal-only control
   variable (per clails' naming convention for such variables). Do not read
   or set it from application code.")

(defvar *%sqlite3-lock-module-loaded* nil
  "Flag indicating whether sqlite3-lock module has been loaded.

   Set to T after src/model/impl/sqlite3-lock.lisp is successfully loaded.
   Used to ensure the module is loaded only once.

   NOTE: The leading % in the name marks this as an internal-only control
   variable (per clails' naming convention for such variables). Do not read
   or set it from application code.")

(defvar *%table-information-initialized* nil
  "Flag indicating whether initialize-table-information has been executed.

   Set to T after initialize-table-information completes successfully.
   Used by query macro to determine whether to create actual query instances
   or placeholder instances for lazy initialization.

   NOTE: The leading % in the name marks this as an internal-only control
   variable (per clails' naming convention for such variables). Do not read
   or set it from application code.")

(defvar *%query-initialization-callbacks* nil
  "List of callback functions to initialize query placeholders.

   When query macro is expanded before initialize-table-information is called,
   callback functions are registered here to initialize query placeholders later.
   Each callback takes no arguments and sets the actual query instance to the
   corresponding placeholder's actual-query slot.
   Cleared after initialize-table-information executes all callbacks.

   NOTE: The leading % in the name marks this as an internal-only control
   variable (per clails' naming convention for such variables). Do not read
   or set it from application code.")

(defparameter +ENVIRONMENT-NAMES+ '("DEVELOP" "TEST" "PRODUCTION")
  "List of valid environment names.")

(defun check-environment-name (env-name)
  "Check if the given environment name is valid.

   @param env-name [string] Environment name to validate
   @return [boolean] T if valid, NIL otherwise
   "
  (not (null (member env-name +ENVIRONMENT-NAMES+ :test #'string-equal))))

(defun set-environment (env-name)
  "Set the project environment if the name is valid.

   Valid environment names are DEVELOP, TEST, and PRODUCTION (case-insensitive).
   Updates *project-environment* with the keyword version of the name.

   @param env-name [string] Environment name to set
   @return [keyword] The set environment keyword, or NIL if invalid
   "
  (let ((env (string-upcase env-name)))
    (when (check-environment-name env)
      (setf *project-environment* (intern env :KEYWORD)))))

(defun add-startup-hook (hook)
  "Register a hook to run at application startup.

   Appends to *startup-hooks*, so hooks run in the order they were
   registered (after any hook already present, including the framework's
   own default). Prefer this over pushing onto *startup-hooks* directly,
   which would reverse the intended execution order.

   @param hook [string or function] Function name (e.g. \"package:function-name\") or a function object
   @return [list] The updated *startup-hooks* list
   "
  (setf *startup-hooks* (append *startup-hooks* (list hook))))

(defun add-shutdown-hook (hook)
  "Register a hook to run at application shutdown.

   Appends to *shutdown-hooks*, so hooks run in the order they were
   registered (after any hook already present, including the framework's
   own default). Prefer this over pushing onto *shutdown-hooks* directly,
   which would reverse the intended execution order.

   @param hook [string or function] Function name (e.g. \"package:function-name\") or a function object
   @return [list] The updated *shutdown-hooks* list
   "
  (setf *shutdown-hooks* (append *shutdown-hooks* (list hook))))

(defun clails-framework-version ()
  "Return the version of the currently loaded clails framework.

   This reflects the :version recorded in clails.asd for whatever clails
   system is actually loaded in the running Lisp image -- not necessarily
   the version a given project's clails.boot was originally generated with.

   @return [string] Version string, e.g. \"0.0.4\"
   "
  (asdf:component-version (asdf:find-system :clails)))

(defun warn-if-framework-version-mismatch (generated-version)
  "Warn (non-fatally) when a project's clails.boot was generated by a
   different clails framework version than the one currently running.

   clails.boot is generated once from a template at `clails new` time and is
   never automatically kept in sync with framework changes. Comparing the
   version recorded at generation time against clails-framework-version lets
   us surface that drift instead of silently letting it accumulate.

   @param generated-version [string] The clails version recorded when this
          project's clails.boot was generated
   @return [boolean] T if a mismatch warning was printed, NIL if versions match
   "
  (let ((current-version (clails-framework-version)))
    (unless (string= generated-version current-version)
      (format *error-output*
              "~&;; WARNING: this project's clails.boot was generated with clails ~A, but the clails framework currently installed is ~A.~%~
               ;; The boot sequence may have changed since this project was created. This is not fatal, but if something looks wrong at startup, compare clails.boot against template/project/clails.boot.tmpl in the clails source for the version you have installed.~%~%"
              generated-version current-version)
      t)))

(defun resolve-project-environment (&key env-var forced)
  "Resolve the effective *project-environment* from its layered inputs and
   log which source determined the final value.

   *project-environment* is decided by up to three layered inputs, listed
   here from lowest to highest precedence:

   1. default  - whatever *project-environment* already holds when this
                 function is called (normally set in the project's
                 app/config/environment.lisp, e.g. :develop).
   2. env-var  - the value of the CLAILS_ENV environment variable, passed
                 in via the ENV-VAR argument (e.g. from clails.boot).
   3. forced   - a forced override, passed in via the FORCED argument
                 (e.g. the \"test\" command always forcing :test).

   The highest-precedence non-nil input wins; *project-environment* is
   updated only when ENV-VAR or FORCED is supplied. Each call is
   independent, so this function can be invoked more than once as inputs
   become available at different points during startup (default first,
   then env-var, then a possible forced override) without changing when
   each input becomes available.

   @param env-var [string or null] Value of CLAILS_ENV, if any
   @param forced [string or null] A forced environment name override, if any
   @return [keyword] The resolved *project-environment* value
   "
  (let ((source
          (cond
            (forced
             (set-environment forced)
             "forced override")
            (env-var
             (set-environment env-var)
             "CLAILS_ENV")
            (t
             "default"))))
    (format t "project environment resolved to ~A (source: ~A)~%" *project-environment* source)
    *project-environment*))
