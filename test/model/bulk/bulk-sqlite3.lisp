(in-package #:cl-user)
(defpackage #:clails-test/model/bulk/bulk-sqlite3
  (:use #:cl
        #:rove
        #:clails/model)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool
                #:get-connection
                #:release-connection)
  (:import-from #:clails/model/transaction
                #:with-transaction
                #:with-transaction-using-connection)
  (:import-from #:clails/model/bulk
                #:with-query-cursor
                #:show-query-sql)
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

(defpackage #:clails-test/model/db/bulk
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/bulk/bulk-sqlite3)

(defun cleanup-table ()
  "Delete all records from users table."
  (let ((connection (get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM users2") '())))

(defun insert-test-data (count)
  "Insert test data into users table.
   
   @param count [integer] Number of records to insert
   "
  (let ((connection (get-connection)))
    (loop for i from 1 to count
          do (dbi-cp:execute
              (dbi-cp:prepare connection 
                             "INSERT INTO users2 (name, age, status, created_at, updated_at) VALUES (?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)")
              (list (format nil "user~3,'0d" i)
                    (+ 20 (mod i 50))
                    (if (evenp i) "active" "inactive"))))))

(defun count-records ()
  "Count total records in users table."
  (let* ((connection (get-connection))
         (result (dbi-cp:fetch-all
                  (dbi-cp:execute
                   (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM users2")
                   '()))))
    (getf (first result) :|count|)))


(setup
  (clrhash clails/model/base-model::*table-information*)
  
  ;; Test model definition
  (defmodel <user> (<base-model>)
    (:table "users2"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
  (setf clails/environment:*project-environment* :test)
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment:*database-config* 
        `(:test (:database-name ,(namestring (merge-pathnames "db/clails_bulk_test.sqlite3" uiop:*temporary-directory*)))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0010" "/app/test/data/0010-bulk-test"))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool)
  (initialize-table-information))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


;;;; ----------------------------------------
;;;; Basic Tests
;;;; ----------------------------------------

(deftest sqlite3-test-with-query-cursor-basic
  (testing "with-query-cursor processes all records"
    (cleanup-table)
    (insert-test-data 100)
    
    (let ((total-count 0)
          (batch-count 0))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 30)
        (incf batch-count)
        (incf total-count (length rows)))
      
      (ok (= total-count 100) "Should process all 100 records")
      (ok (= batch-count 4) "Should process in 4 batches (30+30+30+10)"))))


(deftest sqlite3-test-with-query-cursor-small-batch
  (testing "with-query-cursor with small batch size"
    (cleanup-table)
    (insert-test-data 100)
    
    (let ((total-count 0)
          (batch-count 0))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 10)
        (incf batch-count)
        (incf total-count (length rows)))
      
      (ok (= total-count 100) "Should process all 100 records")
      (ok (= batch-count 10) "Should process in 10 batches (10x10)"))))


(deftest sqlite3-test-with-query-cursor-large-batch
  (testing "with-query-cursor with large batch size"
    (cleanup-table)
    (insert-test-data 100)
    
    (let ((total-count 0)
          (batch-count 0))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 200)
        (incf batch-count)
        (incf total-count (length rows)))
      
      (ok (= total-count 100) "Should process all 100 records")
      (ok (= batch-count 1) "Should process in 1 batch (all at once)"))))


(deftest sqlite3-test-with-query-cursor-with-where
  (testing "with-query-cursor with WHERE clause"
    (cleanup-table)
    (insert-test-data 100)
    
    (let ((total-count 0))
      (with-query-cursor (rows
                          (query <user> 
                                 :as :u 
                                 :where (:= (:u :status) :status)
                                 :order-by ((:u :id)))
                          '(:status "active")
                          :batch-size 20)
        (incf total-count (length rows)))
      
      (ok (= total-count 50) "Should process 50 active records"))))


(deftest sqlite3-test-with-query-cursor-plist-table-format
  (testing "with-query-cursor returns plist"
    (cleanup-table)
    (insert-test-data 10)
    
    (let ((first-row nil))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 5)
        (when (null first-row)
          (setf first-row (first rows))))
      
      (format t "first-row: ~S~%" first-row)
      (ok (eq (type-of (car first-row)) 'KEYWORD)  "Each row should be a plist")
      (ok (getf first-row :u.name) "Should have name key")
      (ok (getf first-row :u.age) "Should have age key")
      (ok (getf first-row :u.status) "Should have status key"))))


(deftest sqlite3-test-with-query-cursor-empty-result
  (testing "with-query-cursor with no matching records"
    (cleanup-table)
    (insert-test-data 10)
    
    (let ((total-count 0)
          (batch-count 0))
      (with-query-cursor (rows
                          (query <user> 
                                 :as :u 
                                 :where (:= (:u :status) :status)
                                 :order-by ((:u :id)))
                          '(:status "nonexistent")
                          :batch-size 20)
        (incf batch-count)
        (incf total-count (length rows)))
      
      (ok (= total-count 0) "Should process 0 records")
      (ok (= batch-count 0) "Should not call callback for empty result"))))


;;;; ----------------------------------------
;;;; Transaction Tests
;;;; ----------------------------------------

(deftest sqlite3-test-with-query-cursor-without-transaction
  (testing "with-query-cursor without explicit transaction"
    (cleanup-table)
    (insert-test-data 50)
    
    (let ((total-count 0))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 20)
        (incf total-count (length rows)))
      
      (ok (= total-count 50) "Should process all records without transaction"))))


(deftest sqlite3-test-with-query-cursor-with-transaction
  (testing "with-query-cursor within explicit transaction"
    (cleanup-table)
    (insert-test-data 50)
    
    (let ((total-count 0))
      (with-transaction
        (with-query-cursor (rows
                            (query <user> :as :u :order-by ((:u :id)))
                            nil
                            :batch-size 20)
          (incf total-count (length rows))))
      
      (ok (= total-count 50) "Should process all records within transaction"))))


(deftest sqlite3-test-with-query-cursor-with-external-connection
  (testing "with-query-cursor with external connection"
    (cleanup-table)
    (insert-test-data 50)
    
    (let ((total-count 0)
          (conn (get-connection)))
      (unwind-protect
           (with-transaction-using-connection conn
             (with-query-cursor (rows
                                 (query <user> :as :u :order-by ((:u :id)))
                                 nil
                                 :batch-size 20
                                 :connection conn)
               (incf total-count (length rows))))
        (release-connection conn))
      
      (ok (= total-count 50) "Should process all records with external connection"))))


;;;; ----------------------------------------
;;;; Data Integrity Tests
;;;; ----------------------------------------

(deftest sqlite3-test-with-query-cursor-data-order
  (testing "with-query-cursor maintains order with ORDER BY"
    (cleanup-table)
    (insert-test-data 30)
    
    (let ((collected-names '()))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :name)))
                          nil
                          :batch-size 10)
        (dolist (row rows)
          (push (getf row :u.name) collected-names)))
      
      (setf collected-names (nreverse collected-names))
      (ok (string= (first collected-names) "user001") "First name should be user001")
      (ok (string= (nth 14 collected-names) "user015") "15th name should be user015")
      (ok (string= (car (last collected-names)) "user030") "Last name should be user030"))))


(deftest sqlite3-test-with-query-cursor-no-duplicates
  (testing "with-query-cursor does not return duplicate records"
    (cleanup-table)
    (insert-test-data 100)
    
    (let ((seen-ids (make-hash-table :test 'equal)))
      (with-query-cursor (rows
                          (query <user> :as :u :order-by ((:u :id)))
                          nil
                          :batch-size 25)
        (dolist (row rows)
          (let ((id (getf row :u.id)))
            (ok (null (gethash id seen-ids)) 
                (format nil "ID ~A should not be duplicated" id))
            (setf (gethash id seen-ids) t))))
      
      (ok (= (hash-table-count seen-ids) 100) 
          "Should see exactly 100 unique IDs"))))



;;;; ----------------------------------------
;;;; Debug Function Tests
;;;; ----------------------------------------

(deftest sqlite3-test-show-query-sql-with-query
  (testing "show-query-sql with <query> object"
    (cleanup-table)
    (insert-test-data 10)
    
    (let* ((output nil)
           (sql (with-output-to-string (*standard-output*)
                  (setf output (show-query-sql 
                                (query <user> 
                                       :as :u 
                                       :where (:= (:u :status) :status)
                                       :order-by ((:u :id)))
                                '(:status "active"))))))
      (ok (stringp output) "Should return SQL string")
      (ok (search "SELECT" output) "Should contain SELECT")
      (ok (search "users" output) "Should contain table name")
      (ok (search "active" output) "Should contain parameter value"))))
