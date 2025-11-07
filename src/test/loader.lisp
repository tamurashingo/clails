(in-package #:cl-user)

(defpackage #:clails/test/loader
  (:use #:cl)
  (:import-from #:clails/environment
                #:*project-name*)
  (:import-from #:clails/test/registry
                #:clear-registry)
  (:export #:*test-modules-loaded*
           #:ensure-test-modules-loaded))

(in-package #:clails/test/loader)

(defvar *test-modules-loaded* nil
  "Flag to track if test modules have been loaded.")

(defun extract-test-systems-from-in-order-to (main-system)
  "Extract test system names from :in-order-to clause.

   @param main-system [asdf:system] Main system object
   @return [list] List of test system name strings
   "
  (let ((in-order-to (asdf/component:component-in-order-to main-system)))
    (loop for entry in in-order-to
          when (and (consp entry)
                    (or (eq (first entry) 'asdf:test-op)
                        (eq (first entry) 'asdf/lisp-action:test-op)))
          append (loop for dep in (rest entry)
                       when (and (consp dep)
                                 (stringp (second dep)))
                       collect (second dep)))))

(defun find-test-systems (project-name)
  "Find test systems for the given project name.

   First tries to extract from :in-order-to clause, then falls back to
   <project-name>-test convention.

   @param project-name [string] Project name
   @return [list] List of test system name strings
   @condition warning If no test systems found
   "
  (handler-case
      (let* ((main-system (asdf:find-system project-name))
             (test-systems (extract-test-systems-from-in-order-to main-system)))
        (if test-systems
            test-systems
            (let ((fallback (format nil "~a-test" project-name)))
              (handler-case
                  (progn
                    (asdf:find-system fallback)
                    (list fallback))
                (error ()
                  (warn "No test systems found for project ~a. Neither :in-order-to test-op nor ~a system exists."
                        project-name fallback)
                  nil)))))
    (error (e)
      (warn "Failed to find main system ~a: ~a" project-name e)
      nil)))

(defun load-test-systems (test-systems)
  "Load all test systems and return success status.
   
   @param test-systems [list] List of test system name strings
   @return [boolean] T if all systems loaded successfully, NIL otherwise
   "
  (when test-systems
    (format t "~&Loading test system(s): ~{~a~^, ~}~%" test-systems)
    (handler-case
        (progn
          (dolist (system-name test-systems)
            (format t "~&  Loading ~a...~%" system-name)
            (asdf:load-system system-name))
          (format t "~&Test modules loaded successfully.~%")
          t)
      (error (e)
        (format *error-output* "~&Error loading test systems: ~a~%" e)
        nil))))

(defun ensure-test-modules-loaded (&key (force nil) asd-systems)
  "Ensure test modules are loaded.
   
   Loads test systems based on project configuration or specified ASD systems.
   Uses *test-modules-loaded* flag to avoid duplicate loading unless force is T.
   
   @param force [boolean] If T, reload test modules even if already loaded
   @param asd-systems [list] List of specific ASD system names to load (overrides auto-detection)
   @return [boolean] T if test modules are loaded, NIL otherwise
   "
  (when (or force (not *test-modules-loaded*))
    (clear-registry)
    
    (let ((test-systems (or asd-systems
                            (find-test-systems *project-name*))))
      (when test-systems
        (let ((success (load-test-systems test-systems)))
          (when success
            (setf *test-modules-loaded* t))
          success)))))
