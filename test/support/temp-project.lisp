(in-package #:cl-user)
(defpackage #:clails-test/support/temp-project
  (:use #:cl)
  (:export #:make-test-project-root
           #:delete-test-project-root))
(in-package #:clails-test/support/temp-project)


;;; A throwaway project tree for the suites that need
;;; CLAILS/ENVIRONMENT:*PROJECT-DIR* to point at something writable.
;;;
;;; The suites used to use the system temporary directory itself as the
;;; project root: UIOP:SETUP-TEMPORARY-DIRECTORY creates no directory of its
;;; own, it only assigns $TMPDIR (or /tmp/) to UIOP:*TEMPORARY-DIRECTORY*, so
;;; *PROJECT-DIR* became /tmp/ and the matching TEARDOWN handed /tmp/ to
;;; UIOP:DELETE-DIRECTORY-TREE -- that is, it tried to delete everything the
;;; machine happened to have under /tmp.  It went unnoticed because the
;;; deletion dies partway through on a directory it may not read
;;; (/tmp/systemd-private-*), so what survived depended on the machine.
;;;
;;; Deleting the tree is the right thing to do -- the suites assert on
;;; generated files, and leftovers from a previous run change what
;;; --no-overwrite and CHECK-UNREGISTERED-MODELS see -- it just has to be
;;; aimed at a directory the suite owns.  So each call here hands out one
;;; uniquely named subdirectory of the temporary directory, and the deletion
;;; is aimed at that subdirectory only.
;;;
;;; UIOP:*TEMPORARY-DIRECTORY* is read but never assigned: other code may
;;; rely on it still naming the real temporary directory.

(defvar *root-name-random-state* (make-random-state t)
  "Random state used to name the directories handed out by
   MAKE-TEST-PROJECT-ROOT.

   Seeded from the environment (MAKE-RANDOM-STATE with T) rather than from
   the image's deterministic initial state, so that two processes started at
   the same moment do not draw the same sequence.")

(defconstant +root-name-attempts+ 100
  "How many names MAKE-TEST-PROJECT-ROOT tries before giving up.")

(defun make-test-project-root (&optional (prefix "clails-test"))
  "Create a fresh, empty directory under the system temporary directory and
   return its pathname.  The caller owns the whole tree and is expected to
   hand it to DELETE-TEST-PROJECT-ROOT when it is done with it.

   The name mixes PREFIX with the current universal time and a random draw.
   A timestamp alone is not enough: two suites, or two runs on the same
   machine, can start inside the same second and would then share a root.
   UIOP (3.3.1 is what qlfile pins) exposes no portable process id to mix in
   instead, so uniqueness is not left to the name at all -- the name only
   makes a clash unlikely, and the loop below makes it harmless.  A candidate
   is accepted only when ENSURE-DIRECTORIES-EXIST reports that it was the one
   that created the directory, so a name already taken is simply retried."
  (uiop:setup-temporary-directory)
  (loop repeat +root-name-attempts+
        for candidate = (uiop:ensure-directory-pathname
                         (merge-pathnames
                          (format nil "~A-~D-~36R/"
                                  prefix
                                  (get-universal-time)
                                  (random (expt 2 48) *root-name-random-state*))
                          uiop:*temporary-directory*))
        do (multiple-value-bind (pathname createdp)
               (ensure-directories-exist candidate)
             (when createdp
               (return pathname)))
        finally (error "Could not create a unique test project root under ~S."
                       uiop:*temporary-directory*)))

(defun delete-test-project-root (root)
  "Delete the directory tree ROOT that MAKE-TEST-PROJECT-ROOT handed out.

   Does nothing when ROOT is NIL -- a suite whose SETUP failed before it got
   a root still runs its TEARDOWN -- and nothing when the directory is
   already gone.  Refuses to delete the system temporary directory itself,
   which is the mistake this file exists to prevent."
  (when root
    (let ((directory (uiop:ensure-directory-pathname root)))
      (when (and uiop:*temporary-directory*
                 (equal directory
                        (uiop:ensure-directory-pathname uiop:*temporary-directory*)))
        (error "Refusing to delete the system temporary directory ~S."
               directory))
      (uiop:delete-directory-tree directory
                                  :if-does-not-exist :ignore
                                  :validate t))))
