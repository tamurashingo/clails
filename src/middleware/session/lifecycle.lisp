(in-package #:cl-user)
(defpackage #:clails/middleware/session/lifecycle
  (:use #:cl)
  ;; SESSION-LOCK-ENV is the existing extension point that turns "something
  ;; that knows a request" into the request environment itself; the method for
  ;; a controller lives in clails/controller/base-controller.  Reusing it
  ;; rather than adding a second generic function of the same shape is what
  ;; keeps a controller acceptable to every function below without this
  ;; package knowing that controllers exist.
  (:import-from #:clails/middleware/session/store/wrapper
                #:session-lock-env)
  (:export #:+session-reserved-key-prefix+
           #:session-reserved-key-p
           #:session-source-env
           #:session-table
           #:session-options
           #:session-id
           #:session-value
           #:session-delete
           #:session-clear
           #:session-destroy
           #:session-regenerate-id
           #:session-destroyed-p
           #:session-id-changing-p
           #:session-new-p
           #:clear-session-table))
(in-package #:clails/middleware/session/lifecycle)


;;; ------------------------------------------------------------------
;;; The session lifecycle API (M5-T1, M5-T2)
;;; ------------------------------------------------------------------
;;;
;;; Up to this milestone a clails action could reach the session only as a
;;; hash-table -- (SESSION CONTROLLER) -- and had to express everything in
;;; terms of GETHASH.  Reading and writing work that way; the four operations
;;; that do not are what this file adds:
;;;
;;;   SESSION-DELETE         forget one key
;;;   SESSION-CLEAR          forget every key, keeping the session itself
;;;   SESSION-DESTROY        end the session: drop it from the store and tell
;;;                          the client to delete its cookie
;;;   SESSION-REGENERATE-ID  keep the contents, but under a brand new id
;;;
;;; Every function here takes a SOURCE, which is either the request
;;; environment (what a middleware stage holds) or an object that knows it --
;;; a controller, which is what an action holds.  So an action writes
;;; (SESSION-CLEAR CONTROLLER) and a stage writes (SESSION-CLEAR ENV), with
;;; one implementation.
;;;
;;; Destroy or regenerate: which one, and when
;;; ------------------------------------------
;;; The two look similar and are not interchangeable.
;;;
;;;   * SESSION-DESTROY is for an EXPLICIT LOGOUT.  It ends the session: the
;;;     store entry goes, and the response carries a Set-Cookie that deletes
;;;     the cookie.  Note what that means for the request it is called in:
;;;     the session is gone when the request finishes, so anything written
;;;     into it afterwards (a farewell flash message, say) is lost.  Write
;;;     the response first, destroy last.  That is also why destroy is NOT
;;;     the mechanism behind idle expiry -- an expiring request still has to
;;;     work normally, with a usable (if empty) session.
;;;
;;;   * SESSION-REGENERATE-ID keeps the session and changes its name.  It is
;;;     the answer to session fixation: call it the instant a user's
;;;     privileges change (right after a successful login, right after
;;;     elevating a role), so that an id an attacker planted in the victim's
;;;     browser beforehand is not the id the authenticated session answers
;;;     to.  The contents survive the change, so a login can set the user and
;;;     regenerate the id in either order.  It is also what the idle-timeout
;;;     path uses: an expired session is emptied and renamed rather than
;;;     continued under the id the client presented (see
;;;     clails/middleware/session/expiry).
;;;
;;; Reserved keys
;;; -------------
;;; The framework itself keeps a few things in the session, and every one of
;;; them is named with the +SESSION-RESERVED-KEY-PREFIX+ ("clails.") so that
;;; an application key can never collide with one: "clails.current-user" is
;;; the existing example, and the CSRF token joins it later.
;;;
;;; SESSION-CLEAR REMOVES RESERVED KEYS TOO, by default.  The question had to
;;; be decided either way, and "clear everything" is the safe default: the
;;; call sites are logout and privilege changes, and a clear that quietly
;;; preserved "clails.current-user" would leave the user logged in after an
;;; action that reads as "throw the session away".  A caller that wants the
;;; framework's own bookkeeping to survive passes :KEEP-RESERVED T -- useful
;;; when clearing application state mid-session without ending the visit.

(defparameter +session-reserved-key-prefix+ "clails."
  "Prefix every session key the FRAMEWORK owns carries.

   An application must not write keys starting with it, and in exchange no
   framework key will ever collide with an application key.  Known reserved
   keys: \"clails.current-user\".")

(defun session-reserved-key-p (key)
  "True when KEY names a framework-owned session entry.

   @param key [t] A session key
   @return [boolean] T when KEY is reserved for the framework
   "
  (and (stringp key)
       (<= (length +session-reserved-key-prefix+) (length key))
       (string= +session-reserved-key-prefix+ key
                :end2 (length +session-reserved-key-prefix+))))


;;; ------------------------------------------------------------------
;;; Finding the request behind a controller
;;; ------------------------------------------------------------------

(defun session-source-env (source)
  "Return the request environment SOURCE describes.

   A plist is taken to be the environment itself; anything else is passed
   through SESSION-LOCK-ENV, which is how a controller (or any other object
   that knows its request) is accepted.

   @param source [t] A request environment plist, or an object that knows one
   @return [list] A request environment plist
   "
  (if (listp source)
      source
      (session-lock-env source)))

(defun %session-unavailable ()
  (error "Session is not available. Enable session middleware by setting ~
          clails/middleware/session-middleware:*enable-session-middleware* ~
          to T in app/config/environment.lisp"))

(defun session-table (source)
  "Return the session hash-table of the request SOURCE describes.

   The same object (SESSION CONTROLLER) returns; the application reads and
   writes it directly.

   @param source [t] A request environment plist, or a controller
   @return [hash-table] The session
   @condition error Signaled when the session stage is not enabled
   "
  (or (getf (session-source-env source) :lack.session)
      (%session-unavailable)))

(defun session-options (source)
  "Return the session options plist of the request SOURCE describes.

   The plist the session stage keeps its per-request decisions in: the id,
   whether the session is new, whether its id has to change, whether it is
   being destroyed.  Writing into it is how SESSION-DESTROY and
   SESSION-REGENERATE-ID work.

   @param source [t] A request environment plist, or a controller
   @return [list] The session options plist
   @condition error Signaled when the session stage is not enabled
   "
  (or (getf (session-source-env source) :lack.session.options)
      (%session-unavailable)))

(defun session-id (source)
  "Return the id of the session of the request SOURCE describes.

   For a request whose id is about to change (SESSION-REGENERATE-ID, or an
   idle-expired session) this is still the OLD id: the new one is minted when
   the response is finalised, so it does not exist yet while the action runs.

   @param source [t] A request environment plist, or a controller
   @return [string] The session id
   @condition error Signaled when the session stage is not enabled
   "
  (getf (session-options source) :id))

(defun session-new-p (source)
  "True when the session of this request was created by this request.

   @param source [t] A request environment plist, or a controller
   @return [boolean] T for a session-creating request
   "
  (and (getf (session-options source) :new-session) t))


;;; ------------------------------------------------------------------
;;; Reading, writing, deleting, clearing (M5-T1)
;;; ------------------------------------------------------------------

(defun session-value (source key &optional default)
  "Return the session value stored under KEY, or DEFAULT when there is none.

   @param source [t] A request environment plist, or a controller
   @param key [t] Session key (a string, by convention)
   @param default [t] Returned when KEY is not in the session
   @return [t] The stored value, or DEFAULT
   "
  (multiple-value-bind (value present-p) (gethash key (session-table source))
    (if present-p value default)))

(defun (setf session-value) (value source key &optional default)
  "Store VALUE in the session under KEY.

   DEFAULT is accepted and ignored, so that (SETF (SESSION-VALUE c k d) v) is
   legal -- a reader with an optional argument needs a writer with the same
   lambda list.

   @param value [t] Value to store
   @param source [t] A request environment plist, or a controller
   @param key [t] Session key (a string, by convention)
   @param default [t] Ignored
   @return [t] VALUE
   "
  (declare (ignore default))
  (setf (gethash key (session-table source)) value))

(defun session-delete (source key)
  "Remove KEY from the session.

   Deleting a key that is not there is not an error.

   @param source [t] A request environment plist, or a controller
   @param key [t] Session key
   @return [boolean] T when something was removed
   "
  (remhash key (session-table source)))

(defun clear-session-table (table &key keep-reserved)
  "Remove every entry from the session TABLE.

   The low-level half of SESSION-CLEAR, also used by the expiry stage, which
   has the table but may have no controller.

   With KEEP-RESERVED true, keys carrying +SESSION-RESERVED-KEY-PREFIX+ are
   left in place; the default removes them as well (see the file header for
   why that is the default).

   @param table [hash-table] A session
   @param keep-reserved [boolean] Keep framework-owned keys
   @return [hash-table] TABLE
   "
  (if keep-reserved
      ;; The keys are collected first: removing entries while MAPHASH walks
      ;; the table is not defined behaviour.
      (let ((doomed '()))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (unless (session-reserved-key-p key)
                     (push key doomed)))
                 table)
        (dolist (key doomed)
          (remhash key table)))
      (clrhash table))
  table)

(defun session-clear (source &key keep-reserved)
  "Remove every value from this request's session, keeping the session itself.

   The session stays usable afterwards -- same id, same cookie, empty
   contents -- so this is the right call for \"forget what the user did\"
   without ending the visit.  To end the visit, use SESSION-DESTROY; to keep
   the contents but change the id, SESSION-REGENERATE-ID.

   @param source [t] A request environment plist, or a controller
   @param keep-reserved [boolean] Keep framework-owned keys (\"clails.*\")
   @return [hash-table] The now (almost) empty session
   "
  (clear-session-table (session-table source) :keep-reserved keep-reserved))


;;; ------------------------------------------------------------------
;;; Destroying and renaming (M5-T2)
;;; ------------------------------------------------------------------

(defun session-destroy (source)
  "End this request's session: drop it from the store and delete the cookie.

   For an explicit logout.  The contents are cleared immediately so that
   nothing in the rest of the request can read them back, and the response
   carries a Set-Cookie that expires the cookie, so the client forgets the id
   as well.

   Anything written into the session after this call is lost, because the
   store entry is removed when the request finishes -- write the response
   first, destroy last.

   @param source [t] A request environment plist, or a controller
   @return [string] The id of the session that was destroyed
   "
  (let ((options (session-options source)))
    (clear-session-table (session-table source))
    (setf (getf options :expire) t)
    ;; :CHANGE-ID and :EXPIRE together would be contradictory (one renames
    ;; the session, the other ends it), and the stage gives :EXPIRE
    ;; precedence; clearing it keeps the request's intent unambiguous for
    ;; anyone reading the options.
    (setf (getf options :change-id) nil)
    (getf options :id)))

(defun session-regenerate-id (source)
  "Give this request's session a brand new id, keeping its contents.

   The defence against session fixation: call it as soon as the user's
   privileges change (after a successful login, after elevating a role), so
   that an id planted in the browser beforehand cannot be the id of the
   authenticated session.

   What happens at the end of the request: a new id is minted, the session is
   stored under it, the OLD ID IS REMOVED FROM THE STORE, and the response
   carries a Set-Cookie with the new id.  (SESSION-ID SOURCE) keeps answering
   the old id until then, because the new one does not exist yet.

   Calling it on a session that is being destroyed does nothing: destroying
   wins, because a session that is ending has no use for a new name.

   @param source [t] A request environment plist, or a controller
   @return [boolean] T when the id will change
   "
  (let ((options (session-options source)))
    (cond ((getf options :expire) nil)
          (t (setf (getf options :change-id) t)
             t))))

(defun session-destroyed-p (source)
  "True when this request's session is being destroyed.

   @param source [t] A request environment plist, or a controller
   @return [boolean] T when SESSION-DESTROY was called for this request
   "
  (and (getf (session-options source) :expire) t))

(defun session-id-changing-p (source)
  "True when this request's session id is going to change.

   @param source [t] A request environment plist, or a controller
   @return [boolean] T when a new id will be minted for this response
   "
  (and (getf (session-options source) :change-id) t))
