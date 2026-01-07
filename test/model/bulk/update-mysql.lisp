(in-package #:cl-user)
(defpackage #:clails-test/model/bulk/update-mysql
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
                #:update-all
                #:update-bulk)
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

(defpackage #:clails-test/model/db/bulk-update-mysql
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/bulk/update-mysql)

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

  ;; Test model definitions
  (defmodel <product> (<base-model>)
    (:table "products"))

  (defmodel <customer> (<base-model>)
    (:table "customers"))

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
;;;; update-all Tests
;;;; ----------------------------------------

(deftest mysql-test-update-all-single-record
  (testing "update-all with single record"
    (cleanup-table)

    ;; Insert a record
    (let* ((product (make-record '<product>
                                :name "Laptop"
                                :price 1500
                                :stock 10))
           (inserted (insert-all (list product)))
           (original-updated-at (ref (first inserted) :updated-at)))

      ;; Wait a moment to ensure timestamp difference
      (sleep 1)

      ;; Update the record
      (setf (ref (first inserted) :name) "Gaming Laptop")
      (setf (ref (first inserted) :price) 2000)

      (let ((result (update-all inserted)))
        (ok (= (length result) 1) "Should return 1 record")
        (ok (string= "Gaming Laptop" (ref (first result) :name)) "Name should be updated")
        (ok (= 2000 (ref (first result) :price)) "Price should be updated")
        (ok (ref (first result) :updated-at) "Should have updated-at")
        (ok (/= original-updated-at (ref (first result) :updated-at)) "updated-at should be changed")))))

(deftest mysql-test-update-all-multiple-records
  (testing "update-all with multiple records"
    (cleanup-table)

    ;; Insert multiple records
    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)
                      (make-record '<product> :name "Keyboard" :price 80 :stock 50)))
           (inserted (insert-all products)))

      ;; Update all records
      (setf (ref (first inserted) :price) 1600)
      (setf (ref (second inserted) :price) 60)
      (setf (ref (third inserted) :price) 90)

      (let ((result (update-all inserted)))
        (ok (= (length result) 3) "Should return 3 records")
        (ok (= 1600 (ref (first result) :price)) "First record price should be updated")
        (ok (= 60 (ref (second result) :price)) "Second record price should be updated")
        (ok (= 90 (ref (third result) :price)) "Third record price should be updated")))))

(deftest mysql-test-update-all-empty-list
  (testing "update-all with empty list"
    (let ((result (update-all nil)))
      (ok (null result) "Should return nil for empty list"))))

(deftest mysql-test-update-all-with-connection
  (testing "update-all with explicit connection"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (inserted (insert-all (list product)))
           (connection (get-connection)))

      (setf (ref (first inserted) :price) 1700)

      (let ((result (update-all inserted :connection connection)))
        (ok (= (length result) 1) "Should return 1 record")
        (ok (= 1700 (ref (first result) :price)) "Price should be updated")))))

(deftest mysql-test-update-all-mixed-models
  (testing "update-all with mixed model instances"
    (cleanup-table)
    ;; Also cleanup customers table
    (let ((connection (get-connection)))
      (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM customers") '()))

    ;; Insert product and customer
    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-product (first (insert-all (list product))))
           (inserted-customer (first (insert-all (list customer)))))

      ;; Update both
      (setf (ref inserted-product :price) 1600)
      (setf (ref inserted-customer :name) "John Doe")

      ;; update-all should update both instances to their respective tables
      (let ((result (update-all (list inserted-product inserted-customer))))
        (ok (= (length result) 2) "Should return 2 records")
        (ok (= 1600 (ref (first result) :price)) "Product price should be updated")
        (ok (string= "John Doe" (ref (second result) :name)) "Customer name should be updated")

        ;; Verify in database
        (let* ((conn (get-connection))
               (product-result (dbi-cp:fetch-all
                                (dbi-cp:execute
                                 (dbi-cp:prepare conn "SELECT * FROM products WHERE id = ?")
                                 (list (ref inserted-product :id)))))
               (customer-result (dbi-cp:fetch-all
                                 (dbi-cp:execute
                                  (dbi-cp:prepare conn "SELECT * FROM customers WHERE id = ?")
                                  (list (ref inserted-customer :id))))))
          (ok (= 1600 (getf (first product-result) :|price|)) "Product price should be updated in DB")
          (ok (string= "John Doe" (getf (first customer-result) :|name|)) "Customer name should be updated in DB"))))))


;;;; ----------------------------------------
;;;; update-bulk Tests
;;;; ----------------------------------------

(deftest mysql-test-update-bulk-single-record
  (testing "update-bulk with single record"
    (cleanup-table)

    ;; Insert a record
    (let* ((product (make-record '<product>
                                :name "Laptop"
                                :price 1500
                                :stock 10))
           (inserted (insert-all (list product))))

      ;; Update the record
      (setf (ref (first inserted) :name) "Gaming Laptop")
      (setf (ref (first inserted) :price) 2000)

      (let ((count (update-bulk '<product> '(:name :price) inserted)))
        (ok (= count 1) "Should return 1 as updated count")

        ;; Verify the update in database
        (let* ((conn (get-connection))
               (result (dbi-cp:fetch-all
                        (dbi-cp:execute
                         (dbi-cp:prepare conn "SELECT * FROM products WHERE id = ?")
                         (list (ref (first inserted) :id))))))
          (ok (string= "Gaming Laptop" (getf (first result) :|name|)) "Name should be updated in DB")
          (ok (= 2000 (getf (first result) :|price|)) "Price should be updated in DB"))))))

(deftest mysql-test-update-bulk-multiple-records
  (testing "update-bulk with multiple records"
    (cleanup-table)

    ;; Insert multiple records
    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)
                      (make-record '<product> :name "Keyboard" :price 80 :stock 50)))
           (inserted (insert-all products)))

      ;; Update all records
      (setf (ref (first inserted) :price) 1600)
      (setf (ref (second inserted) :price) 60)
      (setf (ref (third inserted) :price) 90)

      (let ((count (update-bulk '<product> '(:price) inserted)))
        (ok (= count 3) "Should return 3 as updated count")))))

(deftest mysql-test-update-bulk-column-adjustment
  (testing "update-bulk should remove :id and :created-at, add :updated-at"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (inserted (insert-all (list product)))
           (original-id (ref (first inserted) :id)))

      ;; Try to update with :id and :created-at (should be ignored)
      (setf (ref (first inserted) :name) "New Name")

      (let ((count (update-bulk '<product> '(:id :name :created-at) inserted)))
        (ok (= count 1) "Should update successfully")

        ;; Verify id and created-at are not changed
        (let* ((conn (get-connection))
               (result (dbi-cp:fetch-all
                        (dbi-cp:execute
                         (dbi-cp:prepare conn "SELECT * FROM products WHERE id = ?")
                         (list original-id)))))
          (ok (= original-id (getf (first result) :|id|)) "ID should not change")
          (ok (string= "New Name" (getf (first result) :|name|)) "Name should be updated")
          ;; updated-at should be updated automatically
          (ok (getf (first result) :|updated_at|) "updated_at should exist"))))))

(deftest mysql-test-update-bulk-empty-list
  (testing "update-bulk with empty list"
    (let ((count (update-bulk '<product> '(:name :price) nil)))
      (ok (= count 0) "Should return 0 for empty list"))))

(deftest mysql-test-update-bulk-type-mismatch-error
  (testing "update-bulk with type mismatch should raise error"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-product (first (insert-all (list product)))))

      (ok (signals (update-bulk '<product> '(:name) (list inserted-product customer)))
          "Should signal type-mismatch-error"))))

(deftest mysql-test-update-bulk-type-mismatch-skip
  (testing "update-bulk with type mismatch :skip option"
    (cleanup-table)

    (let* ((product (make-record '<product> :name "Laptop" :price 1500 :stock 10))
           (customer (make-record '<customer> :name "John" :email "john@example.com"))
           (inserted-product (first (insert-all (list product)))))

      (setf (ref inserted-product :name) "Gaming Laptop")

      (let ((count (update-bulk '<product> '(:name) (list inserted-product customer)
                                :on-type-mismatch :skip)))
        (ok (= count 1) "Should update only the matching type (skip customer)")))))

(deftest mysql-test-update-bulk-with-transaction
  (testing "update-bulk with transaction"
    (cleanup-table)

    (let* ((products (list
                      (make-record '<product> :name "Laptop" :price 1500 :stock 10)
                      (make-record '<product> :name "Mouse" :price 50 :stock 100)))
           (inserted (insert-all products)))

      ;; Update with transaction
      (setf (ref (first inserted) :price) 1600)
      (setf (ref (second inserted) :price) 60)

      (let ((count (update-bulk '<product> '(:price) inserted :use-transaction t)))
        (ok (= count 2) "Should update 2 records in transaction")))))

(deftest mysql-test-update-bulk-batch-size
  (testing "update-bulk with batch size"
    (cleanup-table)

    ;; Insert 5 records
    (let* ((products (loop for i from 1 to 5
                          collect (make-record '<product>
                                              :name (format nil "Product ~D" i)
                                              :price (* i 100)
                                              :stock 10)))
           (inserted (insert-all products)))

      ;; Update all with batch size 2
      (loop for i from 0 below 5
            do (setf (ref (nth i inserted) :price) (* (1+ i) 200)))

      (let ((count (update-bulk '<product> '(:price) inserted :batch-size 2)))
        (ok (= count 5) "Should update all 5 records with batch processing")))))
