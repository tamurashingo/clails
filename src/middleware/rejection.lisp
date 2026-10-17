(in-package #:cl-user)
(defpackage #:clails/middleware/rejection
  (:use #:cl)
  (:import-from #:babel
                #:string-to-octets)
  ;; LOG-MESSAGE-TO is the function the log.audit macro expands into.  The
  ;; function is used here instead of the macro because the context plist is
  ;; built at run time; see LOG-REJECTION.
  (:import-from #:clails/logger
                #:log-message-to)
  (:export #:+forbidden-status+
           #:*forbidden-message*
           #:*forbidden-content-type*
           #:forbidden-response
           #:log-rejection
           #:reject-forbidden
           #:forbidden-app))
(in-package #:clails/middleware/rejection)

;;; ------------------------------------------------------------------
;;; Rejecting a request from a middleware stage
;;; ------------------------------------------------------------------
;;;
;;; How a stage rejects a request
;;; -----------------------------
;;; A middleware stage that decides a request must not be served answers it
;;; itself: it returns a Lack response triple (status headers body) and never
;;; calls the application it wraps.  REJECT-FORBIDDEN builds that triple and
;;; records the rejection; FORBIDDEN-APP is the same thing shaped as a Lack
;;; application, which is what a stage that takes a "blocking application"
;;; wants (lack/middleware/csrf takes one as :block-app, for instance).
;;;
;;;     (lambda (app)
;;;       (lambda (env)
;;;         (if (allowed-p env)
;;;             (funcall app env)
;;;             (reject-forbidden env :reason "no token"))))
;;;
;;; Why not the condition classes and the error controller
;;; ------------------------------------------------------
;;; clails/condition defines 403/forbidden, and
;;; clails/controller/error-handle-controller plus the <error-handle-controller>
;;; method of clails/view/view-resolver:resolve-view look like the component
;;; that should turn such a condition into a response.  Neither is used here,
;;; for two reasons:
;;;
;;;   * nothing instantiates <error-handle-controller> anywhere in the tree,
;;;     so reviving it means deciding how error pages are rendered in general
;;;     -- a request-wide condition-to-response mechanism, which is a much
;;;     larger change than returning one status code;
;;;   * a rejecting stage sits OUTSIDE the controller stage (it has to reject
;;;     before routing, since the request must never reach an action), so at
;;;     the point of rejection there is no controller to carry a status code
;;;     and no route to render a view for.  The triple is the only thing the
;;;     stage can return.
;;;
;;; The status code is still the one 403/forbidden carries, and
;;; clails-test/middleware/forbidden-response asserts the two agree, so the
;;; condition class stays the single description of "403" for the day a
;;; condition-to-response mechanism does arrive.
;;;
;;; Where a rejection is recorded
;;; -----------------------------
;;; On the :audit logger, and only there.  Its documented purpose is
;;; "security-related events and user actions", which is exactly what a
;;; rejection is, and the generated project registers it in all three
;;; environments (develop: console, test: console, production:
;;; logs/audit.log, all at :info).  That was verified by actually running the
;;; three setup functions of the generated logger configuration and reading
;;; back what arrived at the destination -- see
;;; clails-test/logger/audit-destination, which is that verification kept as
;;; a test.
;;;
;;; The :web-access logger MUST NOT be used for rejections.  In the generated
;;; project's production configuration
;;; (template/project/app/config/logger.lisp.tmpl) the web-access block
;;; registers the name :sql by mistake, so :web-access is left unregistered
;;; in production -- and logging to an unregistered logger is silently a
;;; no-op.  A rejection recorded there would simply vanish in production.
;;; Fixing that template is M8-T4's business; this module stays away from
;;; that logger either way, because an audit trail does not belong in an
;;; access log.
;;;
;;; This is the framework's first use of the :audit logger.

(defparameter +forbidden-status+ 403
  "The HTTP status code of a rejection response.

   Kept as a name so that the stages doing the rejecting do not each spell
   out the number.  It is the same code clails/condition:403/forbidden
   carries.")

(defvar *forbidden-message* "Forbidden"
  "Default body of a rejection response.

   The decided body format of a 403 built by this module is: one plain-text
   line, no markup, no serialisation format.  A stage may pass a more
   specific text as REJECT-FORBIDDEN's :message, and an application may
   change the default here, but the shape stays the same -- a short string
   that is readable both in a browser and in a terminal.

   This can be overridden in <project>/app/config/environment.lisp")

(defvar *forbidden-content-type* "text/plain; charset=utf-8"
  "Content-Type of a rejection response.

   The charset is spelled out because the body is encoded as UTF-8 (which is
   also how its Content-Length is counted).

   This can be overridden in <project>/app/config/environment.lisp")

(defparameter +forbidden-audit-message+ "Request rejected"
  "Default message of the audit record written for a rejection.

   The response body and the audit message are deliberately separate: the
   body is what the client is told, the audit message is what the operator
   reads.  A stage should pass a reason precise enough to act on
   (\"CSRF token missing\") as REJECT-FORBIDDEN's :reason.")


(defun forbidden-response (&key (message *forbidden-message*)
                                (content-type *forbidden-content-type*)
                                headers)
  "Build the Lack response triple of an HTTP 403 rejection.

   This is the decided way for a middleware stage to answer with a status
   code, headers and a body of its own: the triple is returned to the stage
   above it, exactly as an application's response would be, so the rest of
   the stack (the session stage's Set-Cookie, for instance) still sees it.

   The triple is

     (403 (:content-type <content-type>
           :content-length <byte length of message>
           . <headers>)
          (<message>))

   Content-Length counts UTF-8 bytes, not characters, so a non-ASCII message
   is still announced correctly.

   @param message [string] Response body (default: *forbidden-message*)
   @param content-type [string] Content-Type header (default: *forbidden-content-type*)
   @param headers [plist] Extra response headers, appended after the two above
   @return [list] Lack response as (status-code headers body)
   "
  (check-type message string)
  (list +forbidden-status+
        (append (list :content-type content-type
                      :content-length (length (string-to-octets message
                                                                 :encoding :utf-8)))
                headers)
        (list message)))


(defun request-audit-context (env)
  "Build the audit context plist describing the request ENV.

   Keys whose value is missing from ENV are left out entirely, so an audit
   record never carries a key with no information in it.  :remote-addr is
   reported as :ip-address, which is the key name the audit logger's
   documentation uses.

   @param env [list] Clack/Lack request environment plist (may be NIL)
   @return [plist] Context to hand to the audit logger
   "
  (let ((context '()))
    (flet ((add (key value)
             (when value
               (setf context (append context (list key value))))))
      (add :request-method (getf env :request-method))
      (add :path (getf env :path-info))
      (add :ip-address (getf env :remote-addr)))
    context))


(defun log-rejection (env &key (reason +forbidden-audit-message+)
                               (status +forbidden-status+)
                               context)
  "Record one rejection on the :audit logger.

   The record is written with the function the clails/logger:log.audit macro
   expands into (log-message-to :audit :info ...), because the context plist
   is assembled at run time and so cannot be passed through that macro's
   &rest arguments.  Destination and level are therefore identical to
   log.audit's.

   Never record a rejection on the :web-access logger: in the generated
   project's production configuration that logger is left unregistered (the
   template registers :sql twice), and a record sent to an unregistered
   logger is silently dropped.

   @param env [list] Clack/Lack request environment plist (may be NIL)
   @param reason [string] Why the request was rejected; becomes the record's message
   @param status [integer] Status code reported in the record
   @param context [plist] Extra context, appended after the request's own
   @return [null] nil
   "
  (apply #'log-message-to :audit :info reason
         (append (list :status status)
                 (request-audit-context env)
                 context))
  nil)


(defun reject-forbidden (env &key (reason +forbidden-audit-message+)
                                  (message *forbidden-message*)
                                  (content-type *forbidden-content-type*)
                                  headers
                                  context)
  "Record a rejection on the :audit logger and return the 403 response triple.

   This is what a middleware stage calls instead of calling the application
   it wraps.  The request is not served, the rejection is auditable, and the
   client is told as little as possible about why.

     (lambda (app)
       (lambda (env)
         (if (allowed-p env)
             (funcall app env)
             (reject-forbidden env :reason \"no token\"))))

   @param env [list] Clack/Lack request environment plist (may be NIL)
   @param reason [string] Why the request was rejected (audit record message)
   @param message [string] Response body (default: *forbidden-message*)
   @param content-type [string] Content-Type header (default: *forbidden-content-type*)
   @param headers [plist] Extra response headers
   @param context [plist] Extra audit context, e.g. (:user-id 42 :action \"delete\")
   @return [list] Lack response as (status-code headers body)
   "
  (log-rejection env :reason reason :context context)
  (forbidden-response :message message
                      :content-type content-type
                      :headers headers))


(defun forbidden-app (env)
  "A Lack application that rejects every request it is given with 403.

   The same thing REJECT-FORBIDDEN does, shaped as an application, for the
   stages that are configured with one instead of deciding themselves what a
   rejection looks like -- lack/middleware/csrf's :block-app, for example:

     (funcall *lack-middleware-csrf* app :block-app #'forbidden-app)

   @param env [list] Clack/Lack request environment plist
   @return [list] Lack response as (status-code headers body)
   "
  (reject-forbidden env))
