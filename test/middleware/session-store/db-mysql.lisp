(in-package #:cl-user)
(defpackage #:clails-test/middleware/session-store/db-mysql
  (:use #:cl
        #:rove
        #:clails-test/support/session-store-db)
  (:import-from #:clails-test/support/session-store
                #:assert-bulk-write-survives
                #:assert-locked-increment-is-lossless
                ;; M5-T3: idle expiry and the periodic sweep, required to work
                ;; "whichever store is configured" -- this is the derivation
                ;; that needs a database to prove it.
                #:assert-idle-expiry-works
                #:assert-sweep-removes-idle-sessions)
  (:import-from #:clails/model/connection
                #:shutdown-connection-pool)
  (:import-from #:clails/middleware/session/store/db
                #:make-db-session-store)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails-test/support/temp-project
                #:make-test-project-root
                #:delete-test-project-root))
(in-package #:clails-test/middleware/session-store/db-mysql)


(defvar *test-project-root* nil
  "This suite's own directory under the system temporary directory, standing in
   for a project root.  Scoped to the suite so that TEARDOWN deletes only what
   the suite created.")


;;; M3-T3: the database session store and the table it needs, on MySQL.
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
        (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                 :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                 :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                 :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                 :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf *test-project-root* (make-test-project-root))
  (prepare-session-table-database *test-project-root*))

(teardown
  (shutdown-connection-pool)
  (delete-test-project-root *test-project-root*))


(deftest session-table-and-store-agree-test-mysql
  (testing "the store's columns are the table's columns (MySQL)"
    (assert-session-table-and-store-agree "MySQL")))

(deftest session-table-idempotent-test-mysql
  (testing "creating the table twice is harmless (MySQL)"
    (assert-session-table-creation-is-idempotent "MySQL")))

(deftest session-table-constraints-test-mysql
  (testing "the id is unique and the columns are not nullable (MySQL)"
    (assert-session-table-constraints "MySQL")))

(deftest db-session-store-operations-test-mysql
  (testing "store, fetch and remove against a real table (MySQL)"
    (assert-session-store-three-operations "MySQL")))

(deftest db-session-store-touch-test-mysql
  (testing "the fourth operation updates the time column and nothing else (MySQL)"
    (assert-session-store-touches-only-the-time-column "MySQL")))

(deftest db-session-store-timestamp-test-mysql
  (testing "a time can be stored, read back and compared (MySQL)"
    (assert-session-timestamp-survives-the-database "MySQL")))

(deftest db-session-store-roundtrip-test-mysql
  (testing "a full HTTP round trip through the session stage (MySQL)"
    (assert-session-store-roundtrip "MySQL")))

(deftest db-session-store-without-pool-test-mysql
  (testing "the store and the table work with no connection pool (MySQL)"
    (assert-session-store-works-without-the-pool "MySQL")))

(deftest db-session-store-connections-test-mysql
  (testing "many round trips do not use up connections (MySQL)"
    (assert-session-store-connections-do-not-accumulate "MySQL")))

(deftest db-session-store-definition-file-test-mysql
  (testing "a project's db/session-table.lisp decides the table (MySQL)"
    (assert-definition-file-is-used "MySQL")))

(deftest session-table-migration-independence-test-mysql
  (testing "the session table does not disturb migrate / status / the schema dump (MySQL)"
    (assert-session-table-does-not-disturb-migrations "MySQL")))


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

(deftest db-session-store-wrapped-concurrency-test-mysql
  (testing "4 concurrent writers do not damage a session kept in the table (MySQL)"
    (clear-session-rows)
    (assert-bulk-write-survives (make-db-session-store) "MySQL db store"
                                :shares-container nil)))

(deftest db-session-store-wrapped-lock-test-mysql
  (testing "WITH-SESSION-LOCK works with the table underneath (MySQL)"
    (clear-session-rows)
    (assert-locked-increment-is-lossless (make-db-session-store) "MySQL db store"
                                         :shares-container nil)))


;;; ------------------------------------------------------------------
;;; M5-T3 with this store underneath
;;; ------------------------------------------------------------------
;;;
;;; Server-side idle expiry and the periodic sweep have to hold for the
;;; database derivation as well as for the in-memory one, and this is the
;;; suite that can prove it: both bodies are the shared ones in
;;; clails-test/support/session-store, run here because they need a table.
;;;
;;; Neither waits for real time: the clock the whole session layer reads is
;;; frozen, so the timestamps this store writes into the table -- and the
;;; comparison the stage and the sweeper make against them -- all move
;;; together.  (That the last-access column is a column of this store's own
;;; rather than a borrowed updated_at is asserted by
;;; SESSION-TABLE-AND-STORE-AGREE-TEST above.)

(deftest db-session-store-idle-expiry-test-mysql
  (testing "an idle session is emptied, renamed and deleted from the table (MySQL)"
    (clear-session-rows)
    (assert-idle-expiry-works (make-db-session-store) "MySQL db store")))

(deftest db-session-store-sweep-test-mysql
  (testing "a sweep deletes the idle rows and leaves the live ones (MySQL)"
    (clear-session-rows)
    (assert-sweep-removes-idle-sessions (make-db-session-store) "MySQL db store")))
