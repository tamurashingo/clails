(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store/db-sqlite3
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
(in-package #:clails-test/middleware/session-store/db-sqlite3)


;;; M3-T3: the database session store and the table it needs, on SQLite3.
;;;
;;; The assertions are in clails-test/support/session-store-db, shared by the
;;; three dialect suites: the store, the definition file, the renderer and the
;;; command are one implementation, and the only per-dialect part is the type
;;; name a column gets (datetime here).  What this
;;; file supplies is the configuration and the table -- created in SETUP by
;;; calling the same function the `db:create-session-table' command calls,
;;; which is all the preparation a database suite needs here (the test
;;; containers run no initialisation scripts, and no migration is involved).


(setup
  ;; A suite that ran before this one may have left a pool open against a
  ;; different database.
  (shutdown-connection-pool)
  (setf clails/environment:*database-type*
        (make-instance 'clails/environment::<database-type-sqlite3>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(format NIL "~A/volumes/clails_test.sqlite3"
                                            (env-or-default "CLAILS_SQLITE3_DATABASE" "/app")))))
  (uiop:setup-temporary-directory)
  (prepare-session-table-database))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory*
                              :if-does-not-exist :ignore
                              :validate t))


(deftest session-table-and-store-agree-test-sqlite3
  (testing "the store's columns are the table's columns (SQLite3)"
    (assert-session-table-and-store-agree "SQLite3")))

(deftest session-table-idempotent-test-sqlite3
  (testing "creating the table twice is harmless (SQLite3)"
    (assert-session-table-creation-is-idempotent "SQLite3")))

(deftest session-table-constraints-test-sqlite3
  (testing "the id is unique and the columns are not nullable (SQLite3)"
    (assert-session-table-constraints "SQLite3")))

(deftest db-session-store-operations-test-sqlite3
  (testing "store, fetch and remove against a real table (SQLite3)"
    (assert-session-store-three-operations "SQLite3")))

(deftest db-session-store-touch-test-sqlite3
  (testing "the fourth operation updates the time column and nothing else (SQLite3)"
    (assert-session-store-touches-only-the-time-column "SQLite3")))

(deftest db-session-store-timestamp-test-sqlite3
  (testing "a time can be stored, read back and compared (SQLite3)"
    (assert-session-timestamp-survives-the-database "SQLite3")))

(deftest db-session-store-roundtrip-test-sqlite3
  (testing "a full HTTP round trip through the session stage (SQLite3)"
    (assert-session-store-roundtrip "SQLite3")))

(deftest db-session-store-without-pool-test-sqlite3
  (testing "the store and the table work with no connection pool (SQLite3)"
    (assert-session-store-works-without-the-pool "SQLite3")))

(deftest db-session-store-connections-test-sqlite3
  (testing "many round trips do not use up connections (SQLite3)"
    (assert-session-store-connections-do-not-accumulate "SQLite3")))

(deftest db-session-store-definition-file-test-sqlite3
  (testing "a project's db/session-table.lisp decides the table (SQLite3)"
    (assert-definition-file-is-used "SQLite3")))

(deftest session-table-migration-independence-test-sqlite3
  (testing "the session table does not disturb migrate / status / the schema dump (SQLite3)"
    (assert-session-table-does-not-disturb-migrations "SQLite3")))


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

(deftest db-session-store-wrapped-concurrency-test-sqlite3
  (testing "4 concurrent writers do not damage a session kept in the table (SQLite3)"
    (clear-session-rows)
    (assert-bulk-write-survives (make-db-session-store) "SQLite3 db store"
                                :shares-container nil)))

(deftest db-session-store-wrapped-lock-test-sqlite3
  (testing "WITH-SESSION-LOCK works with the table underneath (SQLite3)"
    (clear-session-rows)
    (assert-locked-increment-is-lossless (make-db-session-store) "SQLite3 db store"
                                         :shares-container nil)))
