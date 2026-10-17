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
                #:log.audit))
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
;;; The same configuration's :web-access block is NOT live in production: it
;;; registers the name :sql (a copy-and-paste slip), so :web-access stays
;;; unregistered there, falls back to :root -- which production configures at
;;; :warn -- and an :info record written to it is dropped without a trace.
;;; That is asserted below too, because it is the reason the rejection path
;;; must not use that logger.  Repairing the template belongs to M8-T4; this
;;; suite only pins down what is true today.

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


(defun audit-log-contents ()
  "Read back the production configuration's audit destination."
  (let ((file (merge-pathnames "logs/audit.log" *work-dir*)))
    (if (probe-file file)
        (uiop:read-file-string file)
        "")))


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


(deftest web-access-logger-is-not-live-in-production-test
  (testing "production leaves :web-access unregistered (A6)"
    ;; The web-access block of the production configuration registers :sql,
    ;; so :web-access itself is never registered there.  A record written to
    ;; it falls back up the hierarchy to :root, which production configures
    ;; at :warn -- so an :info record (which is the level every purpose-built
    ;; logger macro writes at) is dropped without a trace.  This is why the
    ;; rejection path records on :audit instead.  When M8-T4 repairs the
    ;; template, this test is the one that will say so.
    (let ((*logger-registry* (make-hash-table :test 'equal))
          (*default-pathname-defaults* *work-dir*))
      (unwind-protect
           (progn
             (funcall (config-function "setup-logger/production"))
             (ok (not (eq (logger-name (get-logger :web-access)) :web-access))
                 ":web-access has no logger of its own in production")
             (ok (null (log-level-enabled-p :info :web-access))
                 "so an :info web-access record is dropped")
             (ok (eq (logger-name (get-logger :audit)) :audit)
                 "while :audit does have one")
             (ok (log-level-enabled-p :info :audit)
                 "and it accepts :info records"))
        (funcall (config-function "finalize-logger")))))

  (testing "develop and test do register :web-access"
    (dolist (setup-name '("setup-logger/develop" "setup-logger/test"))
      (let ((*logger-registry* (make-hash-table :test 'equal))
            (*default-pathname-defaults* *work-dir*))
        (unwind-protect
             (progn
               (funcall (config-function setup-name))
               (let ((logger (get-logger :web-access)))
                 (ok (and logger (eq (logger-name logger) :web-access))
                     (format nil "~A registers :web-access" setup-name))))
          (funcall (config-function "finalize-logger")))))))
