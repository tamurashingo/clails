(in-package #:cl-user)
(defpackage #:clails-test/model/bulk/delete-mysql
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
                #:delete-all
                #:delete-bulk)
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

(in-package #:clails-test/model/bulk/delete-mysql)

(defun cleanup-table ()
  "Delete all records from products table."
  (let ((connection (get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM products") '())))

(defun cleanup-orders ()
  "Delete all records from orders table."
  (let ((connection (get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM orders") '())))

(defun cleanup-customers ()
  "Delete all records from customers table."
  (let ((connection (get-connection)))
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM customers") '())))

(defun count-records ()
  "Count total records in products table."
  (let* ((connection (get-connection))
         (result (dbi-cp:fetch-all
                  (dbi-cp:execute
                   (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM products")
                   '()))))
    (getf (first result) :|count|)))

(defun count-orders ()
  "Count total records in orders table."
  (let* ((connection (get-connection))
         (result (dbi-cp:fetch-all
                  (dbi-cp:execute
                   (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM orders")
                   '()))))
    (getf (first result) :|count|)))

(defun count-customers ()
  "Count total records in customers table."
  (let* ((connection (get-connection))
         (result (dbi-cp:fetch-all
                  (dbi-cp:execute
                   (dbi-cp:prepare connection "SELECT COUNT(*) as count FROM customers")
                   '()))))
    (getf (first result) :|count|)))


(setup
  (clrhash clails/model/base-model::*table-information*)

  ;; Test model definitions
  (defmodel <product> (<base-model>)
    (:table "products"))

  (defmodel <customer> (<base-model>)
    (:table "customers"
     :relations ((:has-many "clails-test/model/bulk/delete-mysql::<order>"
                   :as :orders
                   :foreign-key :customer-id))))

  (defmodel <order> (<base-model>)
    (:table "orders"
     :relations ((:belongs-to "clails-test/model/bulk/delete-mysql::<customer>"
                   :column :customer
                   :key :customer-id))))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment:*database-config*
        `(:test (:database-name ,(env-or-default "MYSQL_TEST_DATABASE" "clails_test")
                 :username ,(env-or-default "MYSQL_TEST_USERNAME" "root")
                 :password ,(env-or-default "MYSQL_TEST_PASSWORD" "password")
                 :host ,(env-or-default "MYSQL_TEST_HOST" "mysql-test")
                 :port ,(env-or-default "MYSQL_TEST_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0011" "/app/test/data/0011-bulk-insert-test"))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (startup-connection-pool)
  (initialize-table-information))

(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


;;;; ----------------------------------------
;;;; delete-all Tests
;;;; ----------------------------------------

(deftest mysql-test-delete-all-single-record
  (testing "delete-all with single record"
    (cleanup-table)

    ;; Insert a record
    (let* ((product (make-record '<product>
                                :name "Laptop"
                                :price 1500
                                :stock 10))
           (inserted (insert-all (list product))))

      ;; Verify insert
      (ok (= (count-records) 1) "Should have 1 record before delete")

      ;; Delete the record
      (let ((result (delete-all inserted)))
        (ok (= (length result) 1) "Should return 1 record")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-all-multiple-records
  (testing "delete-all with multiple records"
    (cleanup-table)

    ;; Insert multiple records
    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)
                      (make-record '<product> :name "Keyboard" :price 80 :stock 50)))
           (inserted (insert-all products)))

      ;; Verify insert
      (ok (= (count-records) 3) "Should have 3 records before delete")

      ;; Delete all records
      (let ((result (delete-all inserted)))
        (ok (= (length result) 3) "Should return 3 records")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-all-empty-list
  (testing "delete-all with empty list"
    (let ((result (delete-all nil)))
      (ok (null result) "Should return nil for empty list"))))

(deftest mysql-test-delete-all-with-cascade
  (testing "delete-all with cascade deletes related records"
    (cleanup-customers)
    (cleanup-orders)

    ;; Insert customer with orders
    (let* ((customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-customer (first (insert-all (list customer))))
           (customer-id (ref inserted-customer :id)))

      ;; Insert orders for this customer
      (let* ((orders (list
                      (make-record '<order>
                                  :customer-id customer-id
                                  :product-id 1
                                  :quantity 2
                                  :total-price 3000
                                  :order-date "2024-01-01")
                      (make-record '<order>
                                  :customer-id customer-id
                                  :product-id 2
                                  :quantity 1
                                  :total-price 50
                                  :order-date "2024-01-02")))
             (inserted-orders (insert-all orders)))

        ;; Verify data is inserted
        (ok (= (count-customers) 1) "Should have 1 customer before delete")
        (ok (= (count-orders) 2) "Should have 2 orders before delete")

        ;; Load related records into customer instance (required for cascade delete)
        (setf (ref inserted-customer :orders) inserted-orders)

        ;; Delete customer with cascade
        (let ((result (delete-all (list inserted-customer) :cascade t)))
          (ok (= (length result) 1) "Should return 1 deleted customer")
          (ok (= (count-customers) 0) "Should have 0 customers after delete")
          (ok (= (count-orders) 0) "Should have 0 orders after cascade delete"))))))

(deftest mysql-test-delete-all-without-cascade
  (testing "delete-all without cascade leaves related records"
    (cleanup-customers)
    (cleanup-orders)

    ;; Insert customer with orders
    (let* ((customer (make-record '<customer> :name "Jane" :email "jane@example.com"))
           (inserted-customer (first (insert-all (list customer))))
           (customer-id (ref inserted-customer :id)))

      ;; Insert orders for this customer
      (let* ((orders (list
                      (make-record '<order>
                                  :customer-id customer-id
                                  :product-id 1
                                  :quantity 1
                                  :total-price 1500
                                  :order-date "2024-01-03")))
             (inserted-orders (insert-all orders)))

        ;; Verify data is inserted
        (ok (= (count-customers) 1) "Should have 1 customer before delete")
        (ok (= (count-orders) 1) "Should have 1 order before delete")

        ;; Delete customer without cascade (default)
        (let ((result (delete-all (list inserted-customer))))
          (ok (= (length result) 1) "Should return 1 deleted customer")
          (ok (= (count-customers) 0) "Should have 0 customers after delete")
          (ok (= (count-orders) 1) "Should still have 1 order (not cascaded)"))))))


(deftest mysql-test-delete-all-with-connection
  (testing "delete-all with explicit connection"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (inserted (insert-all (list product)))
           (connection (get-connection)))

      ;; Delete with explicit connection
      (let ((result (delete-all inserted :connection connection)))
        (ok (= (length result) 1) "Should return 1 record")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-all-with-transaction
  (testing "delete-all within transaction"
    (cleanup-table)

    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)))
           (inserted (insert-all products))
           (connection (get-connection)))

      ;; Delete within transaction
      (with-transaction-using-connection connection
        (delete-all inserted :connection connection))

      ;; Verify deletion
      (ok (= (count-records) 0) "Should have 0 records after transaction"))))


;;;; ----------------------------------------
;;;; delete-bulk Tests
;;;; ----------------------------------------

(deftest mysql-test-delete-bulk-single-record
  (testing "delete-bulk with single record"
    (cleanup-table)

    ;; Insert a record
    (let* ((product (make-record '<product>
                                :name "Laptop"
                                :price 1500
                                :stock 10))
           (inserted (insert-all (list product))))

      ;; Verify insert
      (ok (= (count-records) 1) "Should have 1 record before delete")

      ;; Delete the record
      (let ((count (delete-bulk '<product> inserted)))
        (ok (= count 1) "Should return 1 as deleted count")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-bulk-multiple-records
  (testing "delete-bulk with multiple records"
    (cleanup-table)

    ;; Insert multiple records
    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)
                      (make-record '<product> :name "Keyboard" :price 80 :stock 50)))
           (inserted (insert-all products)))

      ;; Verify insert
      (ok (= (count-records) 3) "Should have 3 records before delete")

      ;; Delete all records
      (let ((count (delete-bulk '<product> inserted)))
        (ok (= count 3) "Should return 3 as deleted count")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-bulk-batch-size
  (testing "delete-bulk with custom batch size"
    (cleanup-table)

    ;; Insert 5 records
    (let* ((products (loop for i from 1 to 5
                          collect (make-record '<product>
                                              :name (format nil "Product~A" i)
                                              :price (* i 100)
                                              :stock 10)))
           (inserted (insert-all products)))

      ;; Delete with batch size 2
      (let ((count (delete-bulk '<product> inserted :batch-size 2)))
        (ok (= count 5) "Should delete all 5 records")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-bulk-empty-list
  (testing "delete-bulk with empty list"
    (let ((count (delete-bulk '<product> nil)))
      (ok (= count 0) "Should return 0 for empty list"))))

(deftest mysql-test-delete-bulk-type-mismatch-error
  (testing "delete-bulk with type mismatch should raise error"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-product (first (insert-all (list product)))))

      (ok (signals (delete-bulk '<product> (list inserted-product customer)))
          "Should signal type-mismatch-error"))))

(deftest mysql-test-delete-bulk-type-mismatch-skip
  (testing "delete-bulk with type mismatch :skip option"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-product (first (insert-all (list product)))))

      (let ((count (delete-bulk '<product> (list inserted-product customer)
                                :on-type-mismatch :skip)))
        (ok (= count 1) "Should delete only the matching type (skip customer)")
        (ok (= (count-records) 0) "Should have 0 products after delete")))))

(deftest mysql-test-delete-bulk-with-connection
  (testing "delete-bulk with explicit connection"
    (cleanup-table)

    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)))
           (inserted (insert-all products))
           (connection (get-connection)))

      ;; Delete with explicit connection
      (let ((count (delete-bulk '<product> inserted :connection connection)))
        (ok (= count 2) "Should delete 2 records")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))

(deftest mysql-test-delete-bulk-with-transaction
  (testing "delete-bulk within transaction"
    (cleanup-table)

    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)
                      (make-record '<product> :name "Keyboard" :price 80 :stock 50)))
           (inserted (insert-all products))
           (connection (get-connection)))

      ;; Delete within transaction
      (let ((count nil))
        (with-transaction-using-connection connection
          (setf count (delete-bulk '<product> inserted
                                   :connection connection
                                   :use-transaction nil)))

        (ok (= count 3) "Should delete 3 records")
        (ok (= (count-records) 0) "Should have 0 records after transaction")))))

(deftest mysql-test-delete-bulk-without-transaction
  (testing "delete-bulk without transaction"
    (cleanup-table)

    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)))
           (inserted (insert-all products)))

      ;; Delete without transaction
      (let ((count (delete-bulk '<product> inserted :use-transaction nil)))
        (ok (= count 2) "Should delete 2 records")
        (ok (= (count-records) 0) "Should have 0 records after delete")))))
