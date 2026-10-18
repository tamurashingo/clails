(in-package #:cl-user)
(defpackage #:clails-test/project/templates
  (:use #:cl
        #:rove
        ;; HTTP round trips over a real stack, with no server and no database.
        #:clails-test/support/http)
  (:import-from #:cl-ppcre
                #:do-register-groups
                #:scan-to-strings)
  ;; The templates are inspected through the real thing: a project is
  ;; generated and the generated files are read back.
  (:import-from #:clails/project/project
                #:create-project)
  (:import-from #:clails-test/support/temp-project
                #:make-test-project-root
                #:delete-test-project-root)
  ;; Everything the generated configuration assigns is rebound before it is
  ;; loaded, so loading it cannot reconfigure the test image.
  (:import-from #:clails/environment
                #:*project-name*
                #:*project-environment*
                #:*routing-tables*
                #:*startup-hooks*
                #:*shutdown-hooks*)
  ;; The framework-side defaults the templates are expected to override (or to
  ;; leave alone).
  (:import-from #:clails/middleware/session-middleware
                #:*session-cookie-secure*
                #:*enable-session-middleware*
                #:*session-store*
                #:*session-state*
                #:*session-idle-timeout*
                #:*session-sweep-enabled*
                #:*session-sweep-interval*))
(in-package #:clails-test/project/templates)


;;; What `clails new' writes (M8-T1 goal A, M8-T4, M8-T5).
;;;
;;; Three unrelated-looking groups of assertions live together here because
;;; they all answer the same question -- "what does a generated project
;;; contain?" -- and all need one generated project to answer it:
;;;
;;;   * app/config/environment.lisp: sessions are on, the store is named, the
;;;     cookie attributes and the idle timeout and its sweeping are written out
;;;     with their defaults, and the Secure flag is set per environment.
;;;   * app/config/logger.lisp: the log destinations of all three environments
;;;     are registered once each, with no name written by mistake.
;;;   * README.md / Makefile: the session table is created by the command that
;;;     actually exists.
;;;
;;; The CSRF stage is deliberately NOT configured by the templates yet: the
;;; stage is in the stack but off, and switching it on belongs with the e2e
;;; script that has to start sending tokens.

(defvar *test-project-root* nil)
(defvar *project-dir* nil)

(defparameter *test-project-name* "tmplproj")

(defvar *environment-lisp* nil)
(defvar *logger-lisp* nil)
(defvar *readme-md* nil)
(defvar *makefile* nil)
(defvar *boot* nil)

(defvar *generated-settings* nil
  "What the generated app/config/environment.lisp actually sets, as a plist.
   Collected by LOAD-GENERATED-CONFIG in SETUP.")

(defun read-project-file (relative-path)
  (uiop:read-file-string (merge-pathnames relative-path *project-dir*)))

(defun load-generated-config ()
  "Load the generated app/config/environment.lisp and report what it sets.

   The file is loaded for real, because reading it as text can only show that
   a line is present -- not that the project ends up configured by it.  Every
   variable it assigns is rebound first, so loading it configures nothing in
   the test image; the values are read inside that extent and returned as a
   plist.  A variable the file does not touch is reported as :NOT-SET, which
   is how \"the template leaves this default alone\" is told apart from \"the
   template sets it to NIL\".

   @return [list] plist of what the generated configuration sets
   "
  (let ((*project-name* "")
        (*project-environment* :develop)
        (*routing-tables* nil)
        (*startup-hooks* '())
        (*shutdown-hooks* '())
        (*enable-session-middleware* :not-set)
        (*session-store* :not-set)
        (*session-state* :not-set)
        (*session-cookie-secure* :not-set)
        (*session-idle-timeout* :not-set)
        (*session-sweep-enabled* :not-set)
        (*session-sweep-interval* :not-set))
    (handler-bind ((warning (lambda (condition)
                              (declare (ignore condition))
                              (when (find-restart 'muffle-warning)
                                (muffle-warning)))))
      (load (merge-pathnames "app/config/environment.lisp" *project-dir*)))
    (let* ((package (find-package (string-upcase
                                   (format nil "~A/config/environment"
                                           *test-project-name*))))
           (initialize (and package
                            (find-symbol "INITIALIZE-SESSION-CONFIG" package))))
      (flet ((secure-for (environment)
               (if initialize
                   (progn (funcall initialize environment) *session-cookie-secure*)
                   :no-initializer)))
        (list :enabled *enable-session-middleware*
              :store *session-store*
              :state *session-state*
              :idle-timeout *session-idle-timeout*
              :sweep-enabled *session-sweep-enabled*
              :sweep-interval *session-sweep-interval*
              :secure-develop (secure-for :develop)
              :secure-test (secure-for :test)
              :secure-production (secure-for :production))))))

(setup
  (setf *test-project-root* (make-test-project-root "clails-test-templates"))
  (setf *project-dir* (merge-pathnames (format nil "~A/" *test-project-name*)
                                       *test-project-root*))
  (ensure-directories-exist *project-dir*)
  ;; create-project prints what it writes; the suite is not interested in it.
  (let ((*standard-output* (make-broadcast-stream)))
    (create-project *test-project-name* *project-dir* :sqlite3))
  (setf *environment-lisp* (read-project-file "app/config/environment.lisp"))
  (setf *logger-lisp* (read-project-file "app/config/logger.lisp"))
  (setf *readme-md* (read-project-file "README.md"))
  (setf *makefile* (read-project-file "Makefile"))
  (setf *boot* (read-project-file "clails.boot"))
  (setf *generated-settings* (load-generated-config)))

(teardown
  (delete-test-project-root *test-project-root*))


;;; ------------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------------

(defun section (text start-marker end-marker)
  "Return the part of TEXT from START-MARKER up to the next END-MARKER after
   it (or to the end of TEXT), or NIL when START-MARKER is not there."
  (let ((start (search start-marker text)))
    (when start
      (let ((end (search end-marker text :start2 (+ start (length start-marker)))))
        (subseq text start (or end (length text)))))))

(defun logger-setup-section (environment)
  "The body of the SETUP-LOGGER/<ENVIRONMENT> function of the generated
   app/config/logger.lisp."
  (section *logger-lisp*
           (format nil "(defun setup-logger/~A " environment)
           "(defun "))

(defun session-setup-section (environment)
  "The body of the SETUP-SESSION/<ENVIRONMENT> function of the generated
   app/config/environment.lisp."
  (section *environment-lisp*
           (format nil "(defun setup-session/~A " environment)
           "(defun "))

(defun registered-logger-names (text)
  "The logger names TEXT registers, in registration order, downcased."
  (let ((names '()))
    (do-register-groups (name)
        ("\\(register-logger\\s+:([A-Za-z0-9/-]+)" text)
      (push (string-downcase name) names))
    (nreverse names)))

(defun logger-registration (text name)
  "The text of the (register-logger :NAME ...) form in TEXT, up to its level."
  (scan-to-strings (format nil "(?s)\\(register-logger\\s+:~A\\b.*?:level\\s+:[a-z]+" name)
                   text))

(defun same-names-p (expected actual)
  (and (= (length expected) (length actual))
       (null (set-difference expected actual :test #'string=))
       (null (set-difference actual expected :test #'string=))))

(defun cli-command-name ()
  "The name of the CLI subcommand that creates the session table, derived from
   the function that implements it rather than written out by hand."
  (let ((symbol (find-symbol "DB/CREATE-SESSION-TABLE" :clails/cmd)))
    (when symbol
      (substitute #\: #\/ (string-downcase (symbol-name symbol))))))


;;; ------------------------------------------------------------------
;;; app/config/environment.lisp: sessions (M8-T1, goal A)
;;; ------------------------------------------------------------------

(deftest generated-config-enables-sessions-test
  (testing "the generated configuration switches the session stage on"
    (ok (search "(setf *enable-session-middleware* t)" *environment-lisp*)
        "sessions are enabled in a generated project")
    (ok (search "clails/middleware/session-middleware" *environment-lisp*)
        "and the package the setting comes from is named"))

  (testing "the framework's own default stays 'disabled', so existing projects do not change"
    (ok (null *enable-session-middleware*)
        "*enable-session-middleware* is NIL in the framework")))

(deftest generated-config-names-the-store-test
  (testing "the store is written out, with the database store as the alternative"
    (ok (search "(setf *session-store* nil)" *environment-lisp*)
        "the in-memory default is stated rather than left implicit")
    (ok (search "make-db-session-store" *environment-lisp*)
        "and the database store is shown")
    (ok (search "db:create-session-table" *environment-lisp*)
        "together with the command that creates its table"))

  (testing "the restriction on what a database-backed session may hold is stated"
    (ok (search "printed and read" *environment-lisp*)
        "only printable/readable values can be stored")
    (ok (search "Class instances" *environment-lisp*)
        "and the kinds of value that cannot are named")))

(deftest generated-config-states-cookie-attributes-test
  (testing "the cookie attributes are written out"
    (ok (search "HttpOnly" *environment-lisp*) "HttpOnly")
    (ok (search "SameSite" *environment-lisp*) "SameSite")
    (ok (search "NO expiry date" *environment-lisp*)
        "and the deliberate absence of an expiry date")
    (ok (search "make-clails-cookie-state" *environment-lisp*)
        "with the way to override them")))

(deftest generated-config-states-expiry-and-sweeping-test
  (testing "the idle timeout is written out with its default"
    (ok (search "(setf *session-idle-timeout* 1800)" *environment-lisp*)
        "1800 seconds")
    (ok (search "30 minutes" *environment-lisp*)
        "spelled out as 30 minutes")
    (ok (= 1800 *session-idle-timeout*)
        "which is the framework default the template repeats"))

  (testing "the periodic cleanup is written out with its defaults"
    (ok (search "(setf *session-sweep-enabled* t)" *environment-lisp*)
        "sweeping is on")
    (ok (search "(setf *session-sweep-interval* 300)" *environment-lisp*)
        "every 300 seconds")
    (ok (and (eq t *session-sweep-enabled*)
             (= 300 *session-sweep-interval*))
        "which are the framework defaults the template repeats")))

(deftest generated-config-sets-secure-per-environment-test
  (testing "production marks the session cookie Secure"
    (let ((production (session-setup-section "production")))
      (ok production "setup-session/production exists")
      (when production
        (ok (search "(setf *session-cookie-secure* t)" production)
            "and turns the Secure flag on"))))

  (testing "development and test leave it off, so a plain-HTTP session still works"
    (dolist (environment '("develop" "test"))
      (let ((text (session-setup-section environment)))
        (ok text (format nil "setup-session/~A exists" environment))
        (when text
          (ok (search "(setf *session-cookie-secure* nil)" text)
              (format nil "~A leaves the Secure flag off" environment))))))

  (testing "the framework default is NIL and the template is what turns it on"
    (ok (null *session-cookie-secure*)
        "*session-cookie-secure* is NIL in the framework"))

  (testing "what the flag means, and why it is not on everywhere, is explained"
    (ok (search "HTTPS ONLY" *environment-lisp*)
        "T means the cookie is sent over HTTPS only")
    (ok (search "plain HTTP" *environment-lisp*)
        "and a development environment without TLS would lose its sessions")
    (ok (search "stops sessions from working" *environment-lisp*)
        "stated as the consequence, not only as a warning"))

  (testing "the environment-dependent settings are applied after the environment is known"
    (ok (search "initialize-session-config" *environment-lisp*)
        "the dispatcher is defined in the configuration")
    (ok (search "initialize-session-config" *boot*)
        "and clails.boot calls it, after CLAILS_ENV is resolved and before the stack is built")))


;;; ------------------------------------------------------------------
;;; app/config/logger.lisp: the log destinations of all three environments
;;; (M8-T4)
;;; ------------------------------------------------------------------

(deftest generated-logger-registers-every-destination-once-test
  (testing "development registers each destination exactly once"
    (let ((names (registered-logger-names (logger-setup-section "develop"))))
      (ok (same-names-p '("root" "clails" "sql" "web-access" "audit" "task") names)
          (format nil "development registers ~A" names))))

  (testing "test registers each destination exactly once"
    (let ((names (registered-logger-names (logger-setup-section "test"))))
      (ok (same-names-p (list *test-project-name* "clails" "sql" "web-access" "audit" "task")
                        names)
          (format nil "test registers ~A" names))))

  (testing "production registers each destination exactly once"
    ;; :clails is not among them on purpose: with no registration of its own it
    ;; is looked up through the name hierarchy and lands on :root, so framework
    ;; warnings are kept rather than dropped.
    (let ((names (registered-logger-names (logger-setup-section "production"))))
      (ok (same-names-p '("root" "sql" "web-access" "audit" "task") names)
          (format nil "production registers ~A" names))))

  (testing "no environment registers the same name twice"
    ;; A second registration of a name REPLACES the first, so a duplicate is
    ;; not a harmless extra line: it silently discards the earlier one.
    (dolist (environment '("develop" "test" "production"))
      (let ((names (registered-logger-names (logger-setup-section environment))))
        (ok (= (length names) (length (remove-duplicates names :test #'string=)))
            (format nil "~A has no duplicate registration" environment))))))

(deftest generated-logger-registers-audit-everywhere-test
  (testing "the audit destination is registered in all three environments"
    ;; Rejections and configuration warnings are recorded there, and the
    ;; framework-internal logger is at :error in development and test, so an
    ;; unregistered :audit would lose them silently.
    (dolist (environment '("develop" "test" "production"))
      (let ((names (registered-logger-names (logger-setup-section environment))))
        (ok (member "audit" names :test #'string=)
            (format nil "~A registers :audit" environment))))))

(deftest generated-logger-production-files-test
  (testing "each production destination writes to its own file"
    ;; The regression this pins down: the web-access destination used to be
    ;; registered under the name :sql, which both left :web-access
    ;; unregistered and sent the SQL records to logs/access.log, leaving
    ;; logs/sql.log empty.
    (let ((production (logger-setup-section "production")))
      (ok production "setup-logger/production exists")
      (when production
        (let ((sql (logger-registration production "sql"))
              (access (logger-registration production "web-access"))
              (audit (logger-registration production "audit"))
              (task (logger-registration production "task")))
          (ok (and sql (search "logs/sql.log" sql))
              ":sql writes to logs/sql.log")
          (ok (and access (search "logs/access.log" access))
              ":web-access writes to logs/access.log")
          (ok (and audit (search "logs/audit.log" audit))
              ":audit writes to logs/audit.log")
          (ok (and task (search "logs/task.log" task))
              ":task writes to logs/task.log")
          (ok (not (search "logs/access.log" sql))
              "and logs/access.log is not attached to :sql"))))))


;;; ------------------------------------------------------------------
;;; README.md and Makefile: creating the session table (M8-T5)
;;; ------------------------------------------------------------------

(deftest generated-readme-documents-session-table-test
  (testing "the database setup section says how to create the session table"
    (let ((setup (section *readme-md* "### 3. Setup Database" "### 4.")))
      (ok setup "the Setup Database section exists")
      (when setup
        (ok (search "make db.create-session-table" setup)
            "and it names the make target")
        (ok (search "db/session-table.lisp" setup)
            "and the definition file the command reads")
        (ok (search "only needed" setup)
            "noting that the step is only needed with the database store")
        (ok (search "migration history" setup)
            "and that the table is not part of the migration history"))))

  (testing "the make command table has a row for it"
    (let ((commands (section *readme-md* "### Available Make Commands" "### Project Structure")))
      (ok commands "the command table exists")
      (when commands
        (ok (search "| `make db.create-session-table` |" commands)
            "with a row for the session table")))))

(deftest generated-makefile-has-session-table-target-test
  (testing "the Makefile has the target the README tells the reader to run"
    (ok (search (format nil "~%db.create-session-table:") *makefile*)
        "the target is defined")
    (ok (search "db:create-session-table" *makefile*)
        "and it runs the CLI subcommand")
    (ok (search ".PHONY: db.create db.migrate db.rollback db.seed db.create-session-table"
                *makefile*)
        "and it is listed as .PHONY with the other db targets")))

(deftest session-table-command-name-matches-the-cli-test
  (testing "the documented command name is the one that is implemented"
    (let ((command (cli-command-name)))
      (ok (equal command "db:create-session-table")
          (format nil "clails/cmd implements ~A" command))
      (when command
        (ok (search command *readme-md*)
            "the README uses that exact name")
        (ok (search command *makefile*)
            "the Makefile runs that exact name")
        (ok (search command (read-project-file "db/session-table.lisp"))
            "and the definition file points at it as well")))))


;;; ------------------------------------------------------------------
;;; The generated configuration, loaded and run (M8-T1, goal A)
;;; ------------------------------------------------------------------

(deftest generated-config-configures-the-session-stage-test
  (testing "loading the generated configuration really enables sessions"
    (ok (eq t (getf *generated-settings* :enabled))
        "*enable-session-middleware* ends up T")
    (ok (null (getf *generated-settings* :store))
        "and the store stays NIL, i.e. clails' in-memory store"))

  (testing "the cookie is left to the framework's own state"
    ;; NIL would mean "the template set it to NIL"; :NOT-SET means the
    ;; template does not assign it at all, which is what makes the cookie
    ;; attributes (and the Secure flag below) the framework's.
    (ok (eq :not-set (getf *generated-settings* :state))
        "*session-state* is not assigned by the template"))

  (testing "expiry and its cleanup end up at the documented values"
    (ok (eql 1800 (getf *generated-settings* :idle-timeout)))
    (ok (eq t (getf *generated-settings* :sweep-enabled)))
    (ok (eql 300 (getf *generated-settings* :sweep-interval))))

  (testing "the Secure flag of the cookie follows the environment"
    (ok (null (getf *generated-settings* :secure-develop))
        "development: off, so a plain-HTTP session works")
    (ok (null (getf *generated-settings* :secure-test))
        "test: off as well")
    (ok (eq t (getf *generated-settings* :secure-production))
        "production: on")))

(defun generated-cookie-state ()
  "The cookie state a generated project runs with in development."
  (make-clails-cookie-state :secure (getf *generated-settings* :secure-develop)))

(deftest generated-config-session-works-over-plain-http-test
  (testing "a session written on one request is read back on the next"
    ;; The configuration as generated: the session stage enabled, no store of
    ;; its own (so clails' in-memory store), and the development cookie.  This
    ;; is the shipped-product half of the session round-trip suite.
    (with-middleware-stack (app :session (getf *generated-settings* :enabled)
                                :store nil
                                :state (generated-cookie-state)
                                :inner (make-session-counter-app))
      (let* ((client (make-session-client app))
             (first-response (client-request client "/"))
             (set-cookie (session-client-last-set-cookie client)))
        (ok (= (response-status first-response) 200))
        (ok (string= (response-body-string first-response) "1"))
        (ok (string= (response-body-string (client-request client "/")) "2")
            "the second request sees what the first one wrote")
        (ok (string= (response-body-string (client-request client "/")) "3"))

        (testing "and the cookie it sent back is the safe-but-plain-HTTP one"
          (ok set-cookie "the session-creating response carries a Set-Cookie")
          (when set-cookie
            (ok (not (set-cookie-attribute-present-p set-cookie "secure"))
                "no Secure attribute, which is why the session works without TLS")
            (ok (set-cookie-attribute-present-p set-cookie "httponly")
                "HttpOnly")
            (ok (equal (set-cookie-attribute set-cookie "samesite") "Lax")
                "SameSite stated")
            (ok (not (set-cookie-attribute-present-p set-cookie "expires"))
                "and no expiry date: the deadline is the server-side idle timeout")))))))

(deftest generated-config-survives-concurrent-requests-test
  (testing "concurrent requests on one session leave the session intact (G9)"
    ;; A single page load fans out several requests carrying one cookie, so
    ;; this is the normal case for the generated configuration rather than an
    ;; edge case.  Only the container's integrity is asserted: an action that
    ;; reads and writes without taking the session lock MAY lose an increment,
    ;; which is specified behaviour (see clails-test/middleware/session-lock
    ;; for the atomic variant).
    (let* ((result (run-concurrent-session-requests (make-session-counter-app)
                                                    :state (generated-cookie-state)
                                                    :threads 2
                                                    :iterations 25))
           (responses (getf result :responses))
           (session (getf result :session)))
      (ok (null (getf result :timed-out)) "all worker threads finished")
      (ok (eql (getf result :abandoned) 0) "no worker thread had to be destroyed")
      (ok (null (getf result :errors)) "no request signalled an error")
      (ok (and responses
               (every (lambda (response) (= (response-status response) 200)) responses))
          (format nil "~D response(s) came back, all of them 200" (length responses)))
      (ok (hash-table-p session) "the session survives as a hash table")
      (if (hash-table-p session)
          (multiple-value-bind (sane reason) (hash-table-sane-p session)
            (ok sane (or reason "the session container is intact")))
          (ok nil "there is a session container to check")))))
