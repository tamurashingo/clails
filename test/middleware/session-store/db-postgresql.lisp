(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store/db-postgresql
  (:use #:cl
        #:rove
        #:clails-test/support/session-store-db)
  (:import-from #:clails-test/support/session-store
                #:assert-bulk-write-survives
                #:assert-locked-increment-is-lossless)
  (:import-from #:clails/model/connection
                #:shutdown-connection-pool)
  (:import-from #:clails/middleware/session/store/db
                #:make-db-session-store)
  (:import-from #:clails/util
                #:env-or-default))
(in-package #:clails-test/middleware/session-store/db-postgresql)


;;; M3-T3: the database session store and the table it needs, on PostgreSQL.
;;;
;;; The assertions are in clails-test/support/session-store-db, shared by the
;;; three dialect suites: the store, the definition file, the renderer and the
;;; command are one implementation, and the only per-dialect part is the type
;;; name a column gets (timestamp here, where the other two say datetime).  What this
;;; file supplies is the configuration and the table -- created in SETUP by
;;; calling the same function the `db:create-session-table' command calls,
;;; which is all the preparation a database suite needs here (the test
;;; containers run no initialisation scripts, and no migration is involved).


(setup
  ;; A suite that ran before this one may have left a pool open against a
  ;; different database.
  (shutdown-connection-pool)
  (setf clails/environment:*database-type*
        (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                 :username ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                 :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                 :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                 :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (uiop:setup-temporary-directory)
  (prepare-session-table-database))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory*
                              :if-does-not-exist :ignore
                              :validate t))


(deftest session-table-and-store-agree-test-postgresql
  (testing "the store's columns are the table's columns (PostgreSQL)"
    (assert-session-table-and-store-agree "PostgreSQL")))

(deftest session-table-idempotent-test-postgresql
  (testing "creating the table twice is harmless (PostgreSQL)"
    (assert-session-table-creation-is-idempotent "PostgreSQL")))

(deftest session-table-constraints-test-postgresql
  (testing "the id is unique and the columns are not nullable (PostgreSQL)"
    (assert-session-table-constraints "PostgreSQL")))

(deftest db-session-store-operations-test-postgresql
  (testing "store, fetch and remove against a real table (PostgreSQL)"
    (assert-session-store-three-operations "PostgreSQL")))

(deftest db-session-store-touch-test-postgresql
  (testing "the fourth operation updates the time column and nothing else (PostgreSQL)"
    (assert-session-store-touches-only-the-time-column "PostgreSQL")))

(deftest db-session-store-timestamp-test-postgresql
  (testing "a time can be stored, read back and compared (PostgreSQL)"
    (assert-session-timestamp-survives-the-database "PostgreSQL")))

(deftest db-session-store-roundtrip-test-postgresql
  (testing "a full HTTP round trip through the session stage (PostgreSQL)"
    (assert-session-store-roundtrip "PostgreSQL")))

(deftest db-session-store-without-pool-test-postgresql
  (testing "the store and the table work with no connection pool (PostgreSQL)"
    (assert-session-store-works-without-the-pool "PostgreSQL")))

(deftest db-session-store-connections-test-postgresql
  (testing "many round trips do not use up connections (PostgreSQL)"
    (assert-session-store-connections-do-not-accumulate "PostgreSQL")))

(deftest db-session-store-definition-file-test-postgresql
  (testing "a project's db/session-table.lisp decides the table (PostgreSQL)"
    (assert-definition-file-is-used "PostgreSQL")))

(deftest session-table-migration-independence-test-postgresql
  (testing "the session table does not disturb migrate / status / the schema dump (PostgreSQL)"
    (assert-session-table-does-not-disturb-migrations "PostgreSQL")))


;;; ------------------------------------------------------------------
;;; M4 with this store underneath
;;; ------------------------------------------------------------------
;;;
;;; The two acceptance scenarios of M4 are required to hold whichever store is
;;; configured, and the database derivation is the one that could not be
;;; checked when they were written.  Both bodies are the shared ones in
;;; clails-test/support/session-store, run here because they need a database.
;;;
;;; :SHARES-CONTAINER NIL is what makes the expectation differ, and it is a
;;; property of this store rather than a weaker test: a store that rebuilds the
;;; session on every fetch hands each request its own table, so a request
;;; writes back the whole session it fetched and the later write wins.  What
;;; must still hold -- and is asserted -- is that nothing signals, the
;;; container is never damaged, every surviving write-back is a complete one,
;;; and the session is still usable afterwards.

(deftest db-session-store-wrapped-concurrency-test-postgresql
  (testing "4 concurrent writers do not damage a session kept in the table (PostgreSQL)"
    (clear-session-rows)
    (assert-bulk-write-survives (make-db-session-store) "PostgreSQL db store"
                                :shares-container nil)))

(deftest db-session-store-wrapped-lock-test-postgresql
  (testing "WITH-SESSION-LOCK works with the table underneath (PostgreSQL)"
    (clear-session-rows)
    (assert-locked-increment-is-lossless (make-db-session-store) "PostgreSQL db store"
                                         :shares-container nil)))
