(in-package #:cl-user)
(defpackage #:clails-test/model/bulk/insert-postgresql
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
                #:insert-all
                #:insert-bulk)
  (:import-from #:clails/model/query
                #:make-record)
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

(defpackage #:clails-test/model/db/bulk-insert
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/bulk/insert-postgresql)

(defun cleanup-table ()
  "Delete all records from products table."
  (let ((connection (get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM products") '())))

(defun count-records ()
  "Count total records in products table."
  (let* ((connection (get-connection))
         (result (dbi-cp:fetch-all
                  (dbi-cp:execute
                   (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM products")
                   '()))))
    (getf (first result) :|count|)))


(setup
  (clrhash clails/model/base-model::*table-information*)

  ;; Test model definition
  (defmodel <product> (<base-model>)
    (:table "products"))

  (defmodel <customer> (<base-model>)
    (:table "customers"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment:*database-config*
        `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "claisl_test")
                 :username ,(env-or-default "CLAILS_POSTGRES_USERNAME" "clails")
                 :password ,(env-or-default "CLAILS_POSTGRES_PASSWORD" "password")
                 :host ,(env-or-default "CLAILS_POSTGRES_HOST" "postgresql-test")
                 :port ,(env-or-default "CLAILS_POSTGRES_PORT" "5432"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0011" "/app/test/data/0011-bulk-insert-test"))
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool)
  (initialize-table-information))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


;;;; ----------------------------------------
;;;; insert-all Tests
;;;; ----------------------------------------

(deftest postgresql-test-insert-all-single-record
  (testing "insert-all with single record"
    (cleanup-table)

    (let* ((product (make-record '<product>
                                :name "Laptop"
                                :price 1500
                                :stock 10))
           (result (insert-all (list product))))

      (ok (= (length result) 1) "Should return 1 record")
      (ok (ref (first result) :id) "Should have id set")
      (ok (ref (first result) :created-at) "Should have created-at set")
      (ok (ref (first result) :updated-at) "Should have updated-at set")
      (ok (= (count-records) 1) "Should have 1 record in database")

      ;; Verify by selecting
      (let* ((query-result (execute-query
                           (query <product>
                                  :as :p
                                  :where (:= (:p :id) :id))
                           `(:id ,(ref (first result) :id))))
             (saved-product (first query-result)))
        (ok saved-product "Should find the inserted record")
        (ok (string= (ref saved-product :name) "Laptop") "Name should match")
        (ok (= (ref saved-product :price) 1500) "Price should match")
        (ok (= (ref saved-product :stock) 10) "Stock should match")))))


(deftest postgresql-test-insert-all-multiple-records
  (testing "insert-all with multiple records"
    (cleanup-table)

    (let* ((products (list
                     (make-record '<product> :name "Mouse" :price 25 :stock 100)
                     (make-record '<product> :name "Keyboard" :price 75 :stock 50)
                     (make-record '<product> :name "Monitor" :price 300 :stock 20)))
           (result (insert-all products)))

      (ok (= (length result) 3) "Should return 3 records")
      (ok (every (lambda (p) (ref p :id)) result) "All should have id set")
      (ok (= (count-records) 3) "Should have 3 records in database"))))


(deftest postgresql-test-insert-all-empty-list
  (testing "insert-all with empty list"
    (cleanup-table)

    (let ((result (insert-all '())))
      (ok (null result) "Should return nil for empty list")
      (ok (= (count-records) 0) "Should have 0 records in database"))))


;;;; ----------------------------------------
;;;; insert-bulk Tests
;;;; ----------------------------------------

(deftest postgresql-test-insert-bulk-plist-single-record
  (testing "insert-bulk with single plist record"
    (cleanup-table)

    (let ((count (insert-bulk '<product>
                             '(:name :price :stock)
                             '((:name "Tablet" :price 500 :stock 30)))))

      (ok (= count 1) "Should return count of 1")
      (ok (= (count-records) 1) "Should have 1 record in database")

      ;; Verify by selecting
      (let* ((query-result (execute-query
                           (query <product>
                                  :as :p
                                  :where (:= (:p :name) :name))
                           '(:name "Tablet")))
             (saved-product (first query-result)))
        (ok saved-product "Should find the inserted record")
        (ok (string= (ref saved-product :name) "Tablet") "Name should match")
        (ok (= (ref saved-product :price) 500) "Price should match")
        (ok (= (ref saved-product :stock) 30) "Stock should match")
        (ok (ref saved-product :created-at) "Should have created-at set")
        (ok (ref saved-product :updated-at) "Should have updated-at set")))))


(deftest postgresql-test-insert-bulk-plist-multiple-records
  (testing "insert-bulk with multiple plist records"
    (cleanup-table)

    (let ((count (insert-bulk '<product>
                             '(:name :price :stock)
                             '((:name "Phone" :price 800 :stock 15)
                               (:name "Charger" :price 20 :stock 200)
                               (:name "Cable" :price 10 :stock 300)))))

      (ok (= count 3) "Should return count of 3")
      (ok (= (count-records) 3) "Should have 3 records in database"))))


(deftest postgresql-test-insert-bulk-model-instances
  (testing "insert-bulk with model instances"
    (cleanup-table)

    (let* ((products (list
                     (make-record '<product> :name "Speaker" :price 150 :stock 40)
                     (make-record '<product> :name "Webcam" :price 100 :stock 25)))
           (count (insert-bulk '<product>
                              '(:name :price :stock)
                              products)))

      (ok (= count 2) "Should return count of 2")
      (ok (= (count-records) 2) "Should have 2 records in database"))))


(deftest postgresql-test-insert-bulk-large-batch
  (testing "insert-bulk with large number of records"
    (cleanup-table)

    (let* ((records (loop for i from 1 to 250
                         collect `(:name ,(format nil "Product~3,'0d" i)
                                  :price ,(+ 10 (* i 5))
                                  :stock ,(mod (* i 7) 100))))
           (count (insert-bulk '<product>
                              '(:name :price :stock)
                              records
                              :batch-size 100)))

      (ok (= count 250) "Should return count of 250")
      (ok (= (count-records) 250) "Should have 250 records in database"))))


(deftest postgresql-test-insert-bulk-with-default-values
  (testing "insert-bulk with default values"
    (cleanup-table)

    (let ((count (insert-bulk '<product>
                             '(:name :price)
                             '((:name "Headphones" :price 80)))))

      (ok (= count 1) "Should return count of 1")

      ;; Verify default value for stock
      (let* ((query-result (execute-query
                           (query <product>
                                  :as :p
                                  :where (:= (:p :name) :name))
                           '(:name "Headphones")))
             (saved-product (first query-result)))
        (ok saved-product "Should find the inserted record")
        (ok (= (ref saved-product :stock) 0) "Stock should have default value of 0")))))


(deftest postgresql-test-insert-bulk-empty-list
  (testing "insert-bulk with empty list"
    (cleanup-table)

    (let ((count (insert-bulk '<product>
                             '(:name :price :stock)
                             '())))

      (ok (= count 0) "Should return count of 0")
      (ok (= (count-records) 0) "Should have 0 records in database"))))


(deftest postgresql-test-insert-bulk-with-transaction
  (testing "insert-bulk with transaction"
    (cleanup-table)

    (with-transaction
      (insert-bulk '<product>
                  '(:name :price :stock)
                  '((:name "Product1" :price 100 :stock 10)
                    (:name "Product2" :price 200 :stock 20))))

    (ok (= (count-records) 2) "Should have 2 records after transaction")))


(deftest postgresql-test-insert-bulk-without-transaction
  (testing "insert-bulk without transaction"
    (cleanup-table)

    (insert-bulk '<product>
                '(:name :price :stock)
                '((:name "Product1" :price 100 :stock 10)
                  (:name "Product2" :price 200 :stock 20))
                :use-transaction nil)

    (ok (= (count-records) 2) "Should have 2 records without transaction")))


;;;; ----------------------------------------
;;;; Edge Case Tests
;;;; ----------------------------------------

(deftest postgresql-test-insert-all-mixed-models
  (testing "insert-all with multiple model types"
    (cleanup-table)

    ;; Clean up customers table
    (let ((connection (get-connection)))
      (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM customers") '()))

    (let* ((product1 (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (product2 (make-record '<product> :name "Mouse" :price 25 :stock 100))
           (customer1 (make-record '<customer> :name "John Doe" :email "john@example.com" :phone "123-456"))
           (customer2 (make-record '<customer> :name "Jane Smith" :email "jane@example.com" :phone "789-012"))
           (all-records (list product1 customer1 product2 customer2)))

      ;; insert-all should handle each instance individually
      (insert-all all-records)

      ;; Verify products were inserted
      (ok (= (count-records) 2) "Should have 2 products")

      ;; Verify customers were inserted
      (let* ((connection (get-connection))
             (result (dbi-cp:fetch-all
                      (dbi-cp:execute
                       (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM customers")
                       '()))))
        (ok (= (getf (first result) :|count|) 2) "Should have 2 customers"))

      ;; Verify all have IDs set
      (ok (every (lambda (rec) (ref rec :id)) all-records) "All records should have IDs"))))


(deftest postgresql-test-insert-bulk-wrong-model-type-error
  (testing "insert-bulk with wrong model type should raise error"
    (cleanup-table)

    ;; Define a simple non-model class
    (defclass <dummy-class> ()
      ((name :initarg :name)))

    (let* ((dummy1 (make-instance '<dummy-class> :name "Dummy1"))
           (dummy2 (make-instance '<dummy-class> :name "Dummy2")))

      (ok (signals (insert-bulk '<product>
                               '(:name :price :stock)
                               (list dummy1 dummy2))
                   'error)
          "Should raise error for non-model instances"))))


(deftest postgresql-test-insert-bulk-wrong-model-type-skip
  (testing "insert-bulk with wrong model type should skip with :skip option"
    (cleanup-table)

    ;; Define a simple non-model class
    (defclass <dummy-class> ()
      ((name :initarg :name)))

    (let* ((product1 (make-record '<product> :name "Valid1" :price 100 :stock 10))
           (dummy1 (make-instance '<dummy-class> :name "Dummy1"))
           (product2 (make-record '<product> :name "Valid2" :price 200 :stock 20))
           (dummy2 (make-instance '<dummy-class> :name "Dummy2")))

      ;; Should skip dummy instances and insert only valid products
      (let ((count (insert-bulk '<product>
                               '(:name :price :stock)
                               (list product1 dummy1 product2 dummy2)
                               :on-type-mismatch :skip)))

        (ok (= count 2) "Should return count of 2 (only valid products)")
        (ok (= (count-records) 2) "Should have 2 records in database")

        ;; Verify both valid products were inserted
        (let* ((all-products (execute-query (query <product> :as :p :order-by ((:p :id))) nil)))
          (ok (= (length all-products) 2) "Should have 2 products")
          (ok (string= (ref (first all-products) :name) "Valid1") "First product name should match")
          (ok (string= (ref (second all-products) :name) "Valid2") "Second product name should match"))))))


(deftest postgresql-test-insert-bulk-mixed-model-types-error
  (testing "insert-bulk with mixed model types should raise error"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com")))

      (ok (signals (insert-bulk '<product>
                               '(:name :price :stock)
                               (list product customer))
                   'error)
          "Should raise error when list contains different model types"))))


(deftest postgresql-test-insert-bulk-mixed-model-types-skip
  (testing "insert-bulk with mixed model types should skip with :skip option"
    (cleanup-table)

    (let* ((product1 (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer1 (make-record '<customer> :name "John" :email "john@example.com"))
           (product2 (make-record '<product> :name "Mouse" :price 25 :stock 100))
           (customer2 (make-record '<customer> :name "Jane" :email "jane@example.com"))
           (product3 (make-record '<product> :name "Keyboard" :price 75 :stock 50)))

      ;; Should skip customer instances and insert only products
      (let ((count (insert-bulk '<product>
                               '(:name :price :stock)
                               (list product1 customer1 product2 customer2 product3)
                               :on-type-mismatch :skip)))

        (ok (= count 3) "Should return count of 3 (only products)")
        (ok (= (count-records) 3) "Should have 3 records in database")

        ;; Verify all products were inserted
        (let* ((all-products (execute-query (query <product> :as :p :order-by ((:p :id))) nil)))
          (ok (= (length all-products) 3) "Should have 3 products")
          (ok (string= (ref (first all-products) :name) "Laptop") "First product should be Laptop")
          (ok (string= (ref (second all-products) :name) "Mouse") "Second product should be Mouse")
          (ok (string= (ref (third all-products) :name) "Keyboard") "Third product should be Keyboard"))))))


(deftest postgresql-test-insert-bulk-plist-missing-column
  (testing "insert-bulk with plist missing specified column"
    (cleanup-table)

    ;; Specify :stock in columns but don't provide it in plist
    (let ((count (insert-bulk '<product>
                             '(:name :price :stock)
                             '((:name "Headphones" :price 80)))))

      (ok (= count 1) "Should return count of 1")

      ;; Verify the record was inserted with NULL for stock
      (let* ((query-result (execute-query
                           (query <product>
                                  :as :p
                                  :where (:= (:p :name) :name))
                           '(:name "Headphones")))
             (saved-product (first query-result)))
        (ok saved-product "Should find the inserted record")
        (ok (null (ref saved-product :stock)) "Stock should be NULL when not provided in plist")))))
