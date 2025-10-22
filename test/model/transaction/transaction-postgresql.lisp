(in-package #:cl-user)
(defpackage #:clails-test/model/transaction/transaction-postgresql
  (:use #:cl
        #:rove
        #:clails/model)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool
                #:get-connection
                #:release-connection
                #:<connection>)
  (:import-from #:clails/model/transaction
                #:with-transaction)
  (:import-from #:clails/model/migration
                #:create-table
                #:drop-table
                #:db-create
                #:db-migrate)
  (:import-from #:clails/logger
                #:clear-loggers
                #:register-logger
                #:make-console-appender
                #:<text-formatter>)
  (:import-from #:clails/util
                #:env-or-default))

(defpackage #:clails-test/model/transaction/db/transaction-postgresql
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/transaction/transaction-postgresql)



(setup
  (shutdown-connection-pool)
  (clear-loggers)
  (register-logger
   :sql
   :appender (make-console-appender
              :formatter (make-instance '<text-formatter>))
   :level :trace)
 
  (clrhash clails/model/base-model::*table-information*)
 ;; Test model definition
 (defmodel <transaction-test-postgresql> (<base-model>)
   (:table "transaction_tests"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                                                      :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                                                      :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0006" "/app/test/data/0006-transaction-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (db-create)
  (db-migrate)
  (startup-connection-pool)
  (initialize-table-information))



(deftest nested-transaction-savepoint-test-postgresql
  (testing "Nested transactions - both commit should persist all data (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Outer transaction with nested transaction
            (dbi-cp:with-transaction connection
              (let ((inst1 (make-record '<transaction-test-postgresql> :name "outer" :value 100)))
                (save inst1))
              
              ;; Nested transaction (savepoint)
              (dbi-cp:with-transaction connection
                (let ((inst2 (make-record '<transaction-test-postgresql> :name "inner" :value 200)))
                  (save inst2))))
            
            ;; Verify both records were saved
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name, value FROM transaction_tests ORDER BY value")
                              '()))))
                (ok (= (length result) 2))
                (ok (string= (getf (first result) :|name|) "outer"))
                (ok (= (getf (first result) :|value|) 100))
                (ok (string= (getf (second result) :|name|) "inner"))
                (ok (= (getf (second result) :|value|) 200)))))
        (release-connection connection))))
  
  (testing "Nested transactions - inner rollback should rollback only inner (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Outer transaction with nested transaction that fails
            (dbi-cp:with-transaction connection
              (let ((inst1 (make-record '<transaction-test-postgresql> :name "outer" :value 100)))
                (save inst1))
              
              ;; Nested transaction (savepoint) that will rollback
              (handler-case
                  (dbi-cp:with-transaction connection
                    (let ((inst2 (make-record '<transaction-test-postgresql> :name "inner" :value 200)))
                      (save inst2))
                    (error "Rollback inner transaction"))
                (error (c)
                  (declare (ignore c))
                  nil)))
            
            ;; Verify only outer record was saved
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name, value FROM transaction_tests")
                              '()))))
                (ok (= (length result) 1))
                (ok (string= (getf (first result) :|name|) "outer"))
                (ok (= (getf (first result) :|value|) 100)))))
        (release-connection connection))))
  
  (testing "Nested transactions - outer rollback should rollback all (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Outer transaction that will fail
            (handler-case
                (dbi-cp:with-transaction connection
                  (let ((inst1 (make-record '<transaction-test-postgresql> :name "outer" :value 100)))
                    (save inst1))
                  
                  ;; Nested transaction (savepoint)
                  (dbi-cp:with-transaction connection
                    (let ((inst2 (make-record '<transaction-test-postgresql> :name "inner" :value 200)))
                      (save inst2)))
                  
                  (error "Rollback outer transaction"))
              (error (c)
                (declare (ignore c))
                nil))
            
            ;; Verify no data was saved
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name FROM transaction_tests")
                              '()))))
                (ok (= (length result) 0)))))
        (release-connection connection)))))


(deftest with-transaction-commit-test-postgresql
  (testing "with-transaction should commit on success (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Test with-transaction
            (with-transaction
              (let ((inst (make-record '<transaction-test-postgresql> :name "test1" :value 100)))
                (save inst)))
            
            ;; Verify data was committed
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name, value FROM transaction_tests WHERE name = ?")
                              '("test1")))))
                (ok (= (length result) 1))
                (ok (string= (getf (first result) :|name|) "test1"))
                (ok (= (getf (first result) :|value|) 100)))))
        (release-connection connection)))))


(deftest with-transaction-rollback-test-postgresql
  (testing "with-transaction should rollback on error (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Test with-transaction with error
            (handler-case
                (with-transaction
                  (let ((inst (make-record '<transaction-test-postgresql> :name "test2" :value 200)))
                    (save inst))
                  (error "Intentional error"))
              (error (c)
                (declare (ignore c))
                nil))
            
            ;; Verify data was NOT committed (rolled back)
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name FROM transaction_tests WHERE name = ?")
                              '("test2")))))
                (ok (= (length result) 0)))))
        (release-connection connection)))))


(deftest manual-commit-test-postgresql
  (testing "Manual transaction using dbi-cp:with-transaction should persist data (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Manual transaction using dbi-cp:with-transaction
            (dbi-cp:with-transaction connection
              (let ((inst (make-record '<transaction-test-postgresql> :name "test3" :value 300)))
                (save inst)))
            
            ;; Verify
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT value FROM transaction_tests WHERE name = ?")
                              '("test3")))))
                (ok (= (length result) 1))
                (ok (= (getf (first result) :|value|) 300)))))
        (release-connection connection)))))


(deftest manual-rollback-test-postgresql
  (testing "Manual transaction with error using dbi-cp:with-transaction should discard data (PostgreSQL)"
    (let ((connection (get-connection)))
      (unwind-protect
          (progn
            ;; Clean up
            (dbi-cp:with-transaction connection
              (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM transaction_tests") '()))
            
            ;; Manual transaction with rollback
            (handler-case
                (dbi-cp:with-transaction connection
                  (let ((inst (make-record '<transaction-test-postgresql> :name "test4" :value 400)))
                    (save inst))
                  (error "Force rollback"))
              (error (c)
                (declare (ignore c))
                nil))
            
            ;; Verify data was not saved
            (dbi-cp:with-transaction connection
              (let ((result (dbi-cp:fetch-all
                             (dbi-cp:execute
                              (dbi-cp:prepare connection "SELECT name FROM transaction_tests WHERE name = ?")
                              '("test4")))))
                (ok (= (length result) 0)))))
        (release-connection connection)))))


(deftest connection-reuse-test-postgresql
  (testing "Multiple operations should reuse the same connection (PostgreSQL)"
    (let ((connection1 (get-connection))
          (connection2 (get-connection)))
      (unwind-protect
          (progn
            ;; Should return the same connection
            (ok (eq connection1 connection2)))
        (release-connection connection1)))))
