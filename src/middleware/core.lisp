(in-package #:cl-user)
(defpackage #:clails/middleware/core
  (:use #:cl)
  (:import-from #:lack.middleware.static
                #:*lack-middleware-static*)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/transaction-middleware
                #:*lack-middleware-transaction*)
  (:import-from #:clails/middleware/session-middleware
                #:*lack-middleware-clails-session*)
  (:import-from #:clails/middleware/csrf-middleware
                #:*lack-middleware-clails-csrf-body-guard*
                #:*lack-middleware-clails-csrf*)
  (:import-from #:clails/middleware/method-override-middleware
                #:*lack-middleware-clails-method-override*)
  (:export #:*clails-middleware-stack*
           #:add-middleware-before
           #:add-middleware-after
           #:show-middleware-stack))
(in-package #:clails/middleware/core)

(defvar *clails-middleware-stack* (list
                                          *lack-middleware-clails-csrf-body-guard*
                                          *lack-middleware-clails-session*
                                          *lack-middleware-clails-csrf*
                                          *lack-middleware-clails-method-override*
                                          *lack-middleware-transaction*
                                          *lack-middleware-clails-controller*
                                          #'(lambda (app)
                                              (funcall *lack-middleware-static*
                                                       app
                                                       :path "/"
                                                       :root #P"./public/")))
  "A list of middleware functions to be applied to the application.
   Each middleware function should take a single argument,
   which is the next application in the stack,
   and return a new application that wraps the next one.
   see: https://github.com/fukamachi/lack

   The framework's own stages are listed here rather than added with
   ADD-MIDDLEWARE-BEFORE / ADD-MIDDLEWARE-AFTER, because those two reach only
   the ends of the list and the position of a stage is part of what makes it
   correct. The CSRF stage has to sit inside the session stage (the token
   lives in the session) and outside the routing stage (a request must be
   refused before it can reach an action), which is neither end; the same is
   true of the transaction stage. ADD-MIDDLEWARE-BEFORE / ADD-MIDDLEWARE-AFTER
   stay the documented way for an application to add stages of its own.

   The request-method override stage is the strongest case of that: its
   position is a safety requirement, not a preference. It must sit INSIDE the
   CSRF stage, so that a request cannot forge its method before its token is
   verified, and OUTSIDE the controller stage, so that the rewritten method is
   the one the route is matched against. Placed outside the CSRF stage it
   silently opens a bypass, which is why the stage checks at run time that the
   CSRF stage really did run outside it (see
   src/middleware/method-override-middleware.lisp).

   The CSRF body guard is the one framework stage that does belong at the
   outer end, and for a reason worth knowing: the session stage parses the
   request body on its way to the Cookie header, so a body that cannot be
   parsed signals there -- outside the CSRF stage, where no wrapper of the
   CSRF stage could catch it. See src/middleware/csrf-middleware.lisp.

   Both CSRF stages are a pass-through until CSRF is enabled in
   app/config/environment.lisp (as the session stage is until sessions are),
   so adding them to this list does not change how an existing project
   answers anything.

   The override stage is the one framework stage that is enabled by default,
   because it acts only on a request that asks for it -- a POST carrying the
   \"_method\" field, which is a convention only an application's own forms
   use. The one existing project it does change is one whose hand-written
   routing table declares a POST route and a PUT / PATCH / DELETE route on the
   same path; *ENABLE-METHOD-OVERRIDE* turns it off for that case.")


(defun add-middleware-before (middleware)
  "Add middleware at the beginning of the stack.
   
   The middleware will be executed before all existing middleware.
   
   @param middleware [function] Middleware function to add
   "
  (setf *clails-middleware-stack*
        (cons middleware *clails-middleware-stack*)))

(defun add-middleware-after (middleware)
  "Add middleware at the end of the stack.
   
   The middleware will be executed after all existing middleware.
   
   @param middleware [function] Middleware function to add
   "
  (setf *clails-middleware-stack*
        (append *clails-middleware-stack*
                (list middleware))))

(defun show-middleware-stack (&optional (stream T))
  "Display the current middleware stack.
   
   @param stream [stream] Output stream (default: *standard-output*)
   "
  (dolist (mw *clails-middleware-stack*)
    (format stream "~A~%" mw)))
