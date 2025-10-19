(in-package #:cl-user)
(defpackage #:clails-test/model/default-value
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:ref-error
                #:has-error-p
                #:frozen-p
                #:clear-error
                #:validate))

(defpackage #:clails-test/model/db/default-value
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/model/default-value)

(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <product> (<base-model>)
    (:table "product"))

 (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
 (setf clails/environment:*project-environment* :test)
 (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                     :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                     :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                     :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                     :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
 (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0005" "/app/test/data/0005-default-value-test"))
 (uiop:setup-temporary-directory)
 (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
 (setf clails/environment::*project-dir* uiop:*temporary-directory*)
 (clails/model/migration::db-create)
 (clails/model/migration::db-migrate)
 (clails/model/base-model::initialize-table-information)
 (clails/model/connection:startup-connection-pool))


(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(deftest test-insert-with-default-values
  (testing "insert record without setting optional columns should use database default values"
    (let* ((product (make-instance '<product>)))
      ;; Only set required column
      (setf (ref product :name) "Test Product")
      
      ;; Save the record
      (save product)
      
      (ok (ref product :id)
          "Product should be saved with an ID")
      
      ;; Retrieve the record from database to verify default values
      (let* ((query (query <product>
                           :as :product
                           :where (:= (:product :id) :id)))
             (retrieved (car (execute-query query (list :id (ref product :id))))))
        
        (ok retrieved
            "Product should be retrieved from database")
        
        (ok (string= (ref retrieved :name) "Test Product")
            "Name should match the set value")
        
        (ok (string= (ref retrieved :status) "active")
            "Status should have database default value 'active'")
        
        (ok (= (ref retrieved :priority) 0)
            "Priority should have database default value 0")
        
        (ok (eq (ref retrieved :is-published) T)
            "Is-published should have database default value TRUE"))))

  (testing "insert record with some optional columns set should use database defaults for unset columns"
    (let* ((product (make-instance '<product>)))
      ;; Set required column and one optional column
      (setf (ref product :name) "Another Product")
      (setf (ref product :status) "draft")
      
      ;; Save the record
      (save product)
      
      ;; Retrieve the record from database
      (let* ((query (query <product>
                           :as :product
                           :where (:= (:product :id) :id)))
             (retrieved (car (execute-query query (list :id (ref product :id))))))
        
        (ok (string= (ref retrieved :name) "Another Product")
            "Name should match the set value")
        
        (ok (string= (ref retrieved :status) "draft")
            "Status should match the explicitly set value")
        
        (ok (= (ref retrieved :priority) 0)
            "Priority should have database default value 0")
        
        (ok (eq (ref retrieved :is-published) T)
            "Is-published should have database default value TRUE"))))

  (testing "insert record with all columns explicitly set should not use defaults"
    (let* ((product (make-instance '<product>)))
      ;; Set all columns explicitly
      (setf (ref product :name) "Full Product")
      (setf (ref product :status) "archived")
      (setf (ref product :priority) 10)
      (setf (ref product :is-published) NIL)
      
      ;; Save the record
      (save product)
      
      ;; Retrieve the record from database
      (let* ((query (query <product>
                           :as :product
                           :where (:= (:product :id) :id)))
             (retrieved (car (execute-query query (list :id (ref product :id))))))
        
        (ok (string= (ref retrieved :name) "Full Product")
            "Name should match the set value")
        
        (ok (string= (ref retrieved :status) "archived")
            "Status should match the explicitly set value")
        
        (ok (= (ref retrieved :priority) 10)
            "Priority should match the explicitly set value")
        
        (ok (null (ref retrieved :is-published))
            "Is-published should match the explicitly set value (NULL/FALSE)"))))

  (testing "insert record using make-record helper should also respect defaults"
    (let* ((product (make-record '<product> :name "Helper Product" :priority 5)))
      ;; Save the record
      (save product)
      
      ;; Retrieve the record from database
      (let* ((query (query <product>
                           :as :product
                           :where (:= (:product :id) :id)))
             (retrieved (car (execute-query query (list :id (ref product :id))))))
        
        (ok (string= (ref retrieved :name) "Helper Product")
            "Name should match the set value")
        
        (ok (string= (ref retrieved :status) "active")
            "Status should have database default value 'active'")
        
        (ok (= (ref retrieved :priority) 5)
            "Priority should match the explicitly set value")
        
        (ok (eq (ref retrieved :is-published) T)
            "Is-published should have database default value TRUE")))))


(deftest test-ref-with-unset-columns
  (testing "ref should return NIL for valid columns that have not been set"
    (let ((product (make-instance '<product>)))
      
      (ok (null (ref product :name))
          "Unset column 'name' should return NIL")
      
      (ok (null (ref product :status))
          "Unset column 'status' should return NIL")
      
      (ok (null (ref product :priority))
          "Unset column 'priority' should return NIL")))

  (testing "ref should raise error for non-existent columns"
    (let ((product (make-instance '<product>)))
      
      (ok (signals (ref product :non-existent-column))
          "Accessing non-existent column should raise an error"))))
