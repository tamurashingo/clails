(in-package #:cl-user)
(defpackage #:clails/middleware
  (:use #:cl)
  (:import-from #:clails/middleware/core
                #:*clails-middleware-stack*
                #:add-middleware-before
                #:add-middleware-after
                #:show-middleware-stack)
  (:import-from #:clails/middleware/clails-middleware
                #:*lack-middleware-clails-controller*)
  (:import-from #:clails/middleware/transaction-middleware
                #:*lack-middleware-transaction*
                #:*enable-transaction-middleware*)
  (:import-from #:clails/middleware/session-middleware
                #:*lack-middleware-clails-session*
                #:*enable-session-middleware*
                #:*session-store*
                #:*session-state*
                ;; Cookie safety, server-side expiry and its cleanup (M5).
                ;; Each of these is declared in a session-specific package and
                ;; re-exported by clails/middleware/session-middleware; this
                ;; is the second re-export, so that an application can reach
                ;; every session setting through clails/middleware as well.
                #:*session-cookie-secure*
                #:*session-idle-timeout*
                #:*session-sweep-enabled*
                #:*session-sweep-interval*
                #:+default-session-cookie-name+)
  ;; The session cookie clails emits by default, and the state that can be
  ;; built by hand to change its attributes.
  (:import-from #:clails/middleware/session/state/cookie
                #:clails-cookie-state
                #:make-clails-cookie-state)
  ;; Server-side idle expiry: the predicate, and the stage that applies it.
  (:import-from #:clails/middleware/session/expiry
                #:session-idle-expired-p
                #:wrap-session-expiry)
  ;; The periodic sweeper, started and stopped by the framework's own hooks.
  (:import-from #:clails/middleware/session/sweeper
                #:sweep-idle-sessions
                #:start-session-sweeper
                #:stop-session-sweeper
                #:session-sweeper-running-p)
  ;; The session lifecycle API an action uses (also re-exported by the
  ;; controller base, which is where an action normally finds it).
  (:import-from #:clails/middleware/session/lifecycle
                #:+session-reserved-key-prefix+
                #:session-reserved-key-p
                #:session-id
                #:session-value
                #:session-delete
                #:session-clear
                #:session-destroy
                #:session-regenerate-id)
  ;; The clock every session-related timestamp is read through, so that a
  ;; test can freeze "now" for the whole session layer.
  (:import-from #:clails/middleware/session/clock
                #:*session-clock*
                #:session-clock-now)
  ;; The session store abstraction: an application that wants to keep sessions
  ;; somewhere else subclasses <session-store> and implements these generic
  ;; functions, then puts an instance in *session-store*.
  (:import-from #:clails/middleware/session/store
                #:<session-store>
                #:fetch-session
                #:store-session
                #:remove-session
                #:touch-session
                #:session-last-access-time)
  (:import-from #:clails/middleware/session/store/memory
                #:<memory-session-store>
                #:make-memory-session-store)
  ;; The persistent derivation, and the table it needs: the recommended
  ;; production store.  The table is created by `clails db:create-session-table'
  ;; from db/session-table.lisp, and the store takes its column names from the
  ;; same definition, so the two cannot disagree.
  (:import-from #:clails/middleware/session/store/db
                #:<db-session-store>
                #:make-db-session-store)
  (:import-from #:clails/middleware/session/table
                #:session-table-definition
                #:session-table-ddl
                #:ensure-session-table)
  ;; The wrapping store the session stage puts around the configured store:
  ;; it serialises access to one session and is where an application asks for
  ;; an atomic read-modify-write (WITH-SESSION-LOCK).
  (:import-from #:clails/middleware/session/store/wrapper
                #:<session-store-wrapper>
                #:make-session-store-wrapper
                #:ensure-session-store-wrapper
                #:session-store-wrapper-inner
                #:with-session-lock
                #:call-with-session-lock)
  ;; Rejecting a request from inside a stage: a stage that must not let a
  ;; request through answers it itself with a 403 triple and records the
  ;; rejection on the :audit logger.  See src/middleware/rejection.lisp.
  (:import-from #:clails/middleware/rejection
                #:+forbidden-status+
                #:*forbidden-message*
                #:*forbidden-content-type*
                #:forbidden-response
                #:log-rejection
                #:reject-forbidden
                #:forbidden-app)
  (:export #:*clails-middleware-stack*
           #:*lack-middleware-clails-controller*
           #:*lack-middleware-transaction*
           #:*enable-transaction-middleware*
           #:*lack-middleware-clails-session*
           #:*enable-session-middleware*
           #:*session-store*
           #:*session-state*
           #:*session-cookie-secure*
           #:*session-idle-timeout*
           #:*session-sweep-enabled*
           #:*session-sweep-interval*
           #:+default-session-cookie-name+
           #:clails-cookie-state
           #:make-clails-cookie-state
           #:session-idle-expired-p
           #:wrap-session-expiry
           #:sweep-idle-sessions
           #:start-session-sweeper
           #:stop-session-sweeper
           #:session-sweeper-running-p
           #:+session-reserved-key-prefix+
           #:session-reserved-key-p
           #:session-id
           #:session-value
           #:session-delete
           #:session-clear
           #:session-destroy
           #:session-regenerate-id
           #:*session-clock*
           #:session-clock-now
           #:<session-store>
           #:fetch-session
           #:store-session
           #:remove-session
           #:touch-session
           #:session-last-access-time
           #:<memory-session-store>
           #:make-memory-session-store
           #:<db-session-store>
           #:make-db-session-store
           #:session-table-definition
           #:session-table-ddl
           #:ensure-session-table
           #:<session-store-wrapper>
           #:make-session-store-wrapper
           #:ensure-session-store-wrapper
           #:session-store-wrapper-inner
           #:with-session-lock
           #:call-with-session-lock
           #:+forbidden-status+
           #:*forbidden-message*
           #:*forbidden-content-type*
           #:forbidden-response
           #:log-rejection
           #:reject-forbidden
           #:forbidden-app
           #:add-middleware-before
           #:add-middleware-after
           #:show-middleware-stack))

