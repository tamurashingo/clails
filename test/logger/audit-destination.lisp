(in-package #:cl-user)
(defpackage #:clails-test/logger/audit-destination
  (:use #:cl
        #:rove)
  (:import-from #:cl-template
                #:compile-template)
  (:import-from #:clails/logger
                #:*logger-registry*
                #:get-logger
                #:logger-name
                #:logger-level
                #:log-level-enabled-p
                #:log.audit
                #:log.web-access))
(in-package #:clails-test/logger/audit-destination)


;;; Is the audit logger actually live in a generated project?
;;; ---------------------------------------------------------
;;; The rejection path (src/middleware/rejection.lisp) records on the :audit
;;; logger, and that is the first use of :audit from the framework itself.  A
;;; record sent to a logger that the application never registered is silently
;;; dropped, so "the framework writes an audit record" is only worth anything
;;; if the destination exists in a generated project -- in each of the three
;;; environments its logger configuration knows about.
;;;
;;; This suite is that check, kept as a test rather than as a note: it
;;; renders template/project/app/config/logger.lisp.tmpl exactly the way
;;; clails/project/project:create-file-with-template does, loads the result,
;;; runs each of the three setup functions in turn, writes one audit record
;;; through clails/logger:log.audit, and reads back what arrived at the
;;; destination that environment configured.
;;;
;;; Both the logger registry and *default-pathname-defaults* are rebound for
;;; the duration, so neither the suite's logger configuration nor the working
;;; directory is disturbed: the production configuration writes real files
;;; (logs/audit.log and friends), and they are written into a temporary
;;; directory that is deleted afterwards.
;;;
;;; The result, for the record (confirmed by this suite):
;;;
;;;   develop     :audit registered, level :info, console appender,
;;;               <text-formatter>                                   -- live
;;;   test        :audit registered, level :info, console appender,
;;;               <text-formatter>                                   -- live
;;;   production  :audit registered, level :info, file appender on
;;;               logs/audit.log, <json-formatter>                   -- live
;;;
;;; The same configuration's :web-access block USED to register the name :sql
;;; (a copy-and-paste slip), which left :web-access unregistered in
;;; production: records written to it fell back to :root -- configured at
;;; :warn there -- so an :info record was dropped without a trace, and the SQL
;;; destination was overwritten with logs/access.log, leaving logs/sql.log
;;; empty.  M8-T4 repaired the template, and the second suite below is what
;;; says so: every purpose-built destination is now registered in all three
;;; environments, and a production web-access record really arrives in
;;; logs/access.log.
;;;
;;; A duplicate registration is not visible from here, because registering a
;;; name twice simply replaces the first logger: that half of the check reads
;;; the generated file as text and lives in clails-test/project/templates.

(defparameter +project-name+ "m2auditcheck"
  "Project name the template is rendered with.  Only used to spell the
   generated package name, which must not collide with anything else in the
   image.")

(defvar *work-dir* nil
  "Temporary directory the generated configuration is written into and run
   in.  Stands in for a generated project's root.")

(defun render-logger-config ()
  "Render the generated project's logger configuration from its template.

   The delimiters are the ones clails/project/project:create-file-with-template
   passes, so what comes out is byte for byte what `clails new` writes."
  (let* ((template-path (asdf:system-relative-pathname
                         :clails "template/project/app/config/logger.lisp.tmpl"))
         (template (uiop:read-file-string template-path :external-format :utf-8)))
    (funcall (compile-template template
                               :start-delimiter "<%"
                               :start-echo-delimiter "<%="
                               :end-delimiter "%>")
             (list :project-name +project-name+
                   :project-dir (namestring *work-dir*)
                   :database :sqlite3))))

(defun load-generated-logger-config ()
  "Write the rendered configuration into the work directory and load it."
  (let ((file (merge-pathnames "logger.lisp" *work-dir*)))
    (with-open-file (out file :direction :output
                              :if-exists :supersede
                              :external-format :utf-8)
      (write-string (render-logger-config) out))
    (handler-bind ((warning #'muffle-warning))
      (load file))))

(defun config-function (name)
  "Look up NAME in the package the generated configuration defines."
  (let* ((package-name (string-upcase (format nil "~A/config/logger" +project-name+)))
         (symbol (find-symbol (string-upcase name) package-name)))
    (unless symbol
      (error "The generated logger configuration defines no ~A" name))
    (symbol-function symbol)))

(defun run-environment (setup-name message)
  "Run the generated configuration's SETUP-NAME, write one audit record and
   report what happened.

   Returns (values console-output audit-logger), where console-output is
   everything that was printed while the record was written.  The file
   destinations an environment may have configured are left in *work-dir*
   for the caller to read, with every appender closed."
  (let ((*logger-registry* (make-hash-table :test 'equal))
        (*default-pathname-defaults* *work-dir*))
    (unwind-protect
         (let* ((output (with-output-to-string (*standard-output*)
                          (funcall (config-function setup-name))
                          (log.audit message :status 403 :path "/todos")))
                (logger (get-logger :audit)))
           (values output logger))
      ;; closes the file streams the production configuration opens
      (funcall (config-function "finalize-logger")))))

(setup
  (setf *work-dir*
        (ensure-directories-exist
         (merge-pathnames (format nil "clails-test-audit-destination-~A-~A/"
                                  (get-universal-time)
                                  (random 100000 (make-random-state t)))
                          (uiop:temporary-directory))))
  (load-generated-logger-config))

(teardown
  (when *work-dir*
    (uiop:delete-directory-tree *work-dir* :validate t :if-does-not-exist :ignore)
    (setf *work-dir* nil)))


(defun log-file-contents (relative-path)
  "Read back one of the file destinations the production configuration writes,
   named relative to the generated project's root."
  (let ((file (merge-pathnames relative-path *work-dir*)))
    (if (probe-file file)
        (uiop:read-file-string file)
        "")))

(defun audit-log-contents ()
  "Read back the production configuration's audit destination."
  (log-file-contents "logs/audit.log"))


(deftest audit-logger-is-live-in-every-environment-test
  (testing "develop: registered, and the record reaches its console appender"
    (multiple-value-bind (output logger)
        (run-environment "setup-logger/develop" "M2 audit destination check (develop)")
      (ok logger "develop registers a logger reachable as :audit")
      ;; not a fallback to :root or to a parent: the exact name is registered
      (ok (eq (logger-name logger) :audit))
      (ok (eq (logger-level logger) :info))
      (ok (search "M2 audit destination check (develop)" output)
          "the record arrived at the configured destination")
      (ok (search "INFO" output))
      (ok (search "status=403" output))))

  (testing "test: registered, and the record reaches its console appender"
    (multiple-value-bind (output logger)
        (run-environment "setup-logger/test" "M2 audit destination check (test)")
      (ok logger "test registers a logger reachable as :audit")
      (ok (eq (logger-name logger) :audit))
      (ok (eq (logger-level logger) :info))
      (ok (search "M2 audit destination check (test)" output))
      (ok (search "INFO" output))
      (ok (search "status=403" output))))

  (testing "production: registered, and the record reaches logs/audit.log"
    (multiple-value-bind (output logger)
        (run-environment "setup-logger/production" "M2 audit destination check (production)")
      (ok logger "production registers a logger reachable as :audit")
      (ok (eq (logger-name logger) :audit))
      (ok (eq (logger-level logger) :info))
      (ok (equal output "") "production's audit destination is a file, not the console")
      (let ((contents (audit-log-contents)))
        (ok (search "M2 audit destination check (production)" contents)
            "the record arrived in logs/audit.log")
        (ok (search "\"STATUS\":403" contents)
            "and it is the structured (JSON) format production configures")))))


(defparameter +purpose-built-loggers+ '(:sql :web-access :audit :task)
  "The destinations a generated configuration is expected to register in
   every environment, one logger per purpose.")

(deftest purpose-built-loggers-are-live-in-every-environment-test
  (testing "every environment registers each purpose-built destination itself"
    ;; \"Itself\" is the point: GET-LOGGER walks up the name hierarchy, so an
    ;; unregistered name answers with :root instead of signalling.  That is
    ;; how the pre-M8-T4 production configuration lost its web-access
    ;; records -- the name was never registered, the fallback was :root at
    ;; :warn, and an :info record vanished.  Comparing the logger's own name
    ;; is what distinguishes "registered" from "fell back".
    (dolist (setup-name '("setup-logger/develop"
                          "setup-logger/test"
                          "setup-logger/production"))
      (let ((*logger-registry* (make-hash-table :test 'equal))
            (*default-pathname-defaults* *work-dir*))
        (unwind-protect
             (progn
               (funcall (config-function setup-name))
               (dolist (name +purpose-built-loggers+)
                 (let ((logger (get-logger name)))
                   (ok (and logger (eq (logger-name logger) name))
                       (format nil "~A registers ~S" setup-name name))))
               ;; The level is only asserted for :audit, because that is the
               ;; one every environment has to accept :info on: M2's
               ;; rejections and M6's configuration warnings are recorded
               ;; there, and the framework-internal logger is at :error in
               ;; development and test.  The other three are deliberately set
               ;; per environment (production keeps :sql quiet, for instance).
               (ok (log-level-enabled-p :info :audit)
                   (format nil "~A accepts an :info record on :audit" setup-name)))
          (funcall (config-function "finalize-logger")))))))

(deftest production-web-access-record-reaches-its-file-test
  (testing "a production web-access record arrives in logs/access.log (M8-T4)"
    ;; The regression this pins down: before M8-T4 the web-access block
    ;; registered the name :sql, so this record was written to a logger that
    ;; did not exist, fell back to :root at :warn and was dropped -- while
    ;; logs/access.log received the SQL records instead.
    (let ((*logger-registry* (make-hash-table :test 'equal))
          (*default-pathname-defaults* *work-dir*))
      (unwind-protect
           (progn
             (funcall (config-function "setup-logger/production"))
             (log.web-access "M8-T4 web-access destination check"
                             :method "GET" :path "/todos" :status 200))
        (funcall (config-function "finalize-logger")))
      (let ((contents (log-file-contents "logs/access.log")))
        (ok (search "M8-T4 web-access destination check" contents)
            "the record arrived in logs/access.log")
        (ok (search "\"STATUS\":200" contents)
            "and it is the structured (JSON) format production configures")))))
