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
                #:*session-state*)
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
  (:export #:*clails-middleware-stack*
           #:*lack-middleware-clails-controller*
           #:*lack-middleware-transaction*
           #:*enable-transaction-middleware*
           #:*lack-middleware-clails-session*
           #:*enable-session-middleware*
           #:*session-store*
           #:*session-state*
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
           #:add-middleware-before
           #:add-middleware-after
           #:show-middleware-stack))

