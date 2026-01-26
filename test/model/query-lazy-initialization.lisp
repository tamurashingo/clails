(in-package #:cl-user)
(defpackage #:clails-test/model/query-lazy-initialization
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))

(in-package #:clails-test/model/query-lazy-initialization)


(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  
  ;; Reset initialization state
  (setf clails/environment:*table-information-initialized* nil)
  (setf clails/environment:*query-initialization-callbacks* nil)
  
  ;; define models
  (defmodel <blog> (<base-model>)
    (:table "blogs"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0020" "/app/test/data/0020-join-query-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/connection:startup-connection-pool))


(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool)
  
  ;; Reset initialization state
  (setf clails/environment:*table-information-initialized* nil)
  (setf clails/environment:*query-initialization-callbacks* nil))


(deftest test-query-before-initialization
  (testing "query macro before initialize-table-information should create placeholder"
    ;; Ensure we're in uninitialized state
    (setf clails/environment:*table-information-initialized* nil)
    (setf clails/environment:*query-initialization-callbacks* nil)
    
    ;; Create query before initialization
    (let ((q (query <blog>
               :as :blog
               :columns ((blog :id :title))
               :where (:> (:blog :star) :min-star))))
      
      ;; Should be a placeholder
      (ok (typep q 'clails/model/query::<query-placeholder>))
      
      ;; Callback should be registered
      (ok (= 1 (length clails/environment:*query-initialization-callbacks*)))
      
      ;; actual-query should be nil
      (ok (null (slot-value q 'clails/model/query::actual-query)))
      
      ;; Attempting to use should raise error
      (ok (signals (execute-query q '(:min-star 5))
                   'error)
          "Should raise error when using uninitialized placeholder"))))


(deftest test-query-after-initialization
  (testing "query macro after initialize-table-information should create actual query"
    ;; Initialize table information
    (clails/model/base-model:initialize-table-information)
    
    ;; Create query after initialization
    (let ((q (query <blog>
               :as :blog
               :columns ((blog :id :title))
               :where (:> (:blog :star) :min-star))))
      
      ;; Should be an actual query
      (ok (typep q 'clails/model/query::<query>))
      
      ;; No new callback should be registered
      (ok (= 0 (length clails/environment:*query-initialization-callbacks*))))))


(deftest test-placeholder-initialization
  (testing "placeholder should be initialized after initialize-table-information"
    ;; Reset initialization state
    (setf clails/environment:*table-information-initialized* nil)
    (setf clails/environment:*query-initialization-callbacks* nil)
    
    ;; Create query before initialization
    (let ((q (query <blog>
               :as :blog
               :columns ((blog :id :title))
               :where (:> (:blog :star) :min-star))))
      
      ;; Should be a placeholder
      (ok (typep q 'clails/model/query::<query-placeholder>))
      
      ;; Initialize table information (this should execute callbacks)
      (clails/model/base-model:initialize-table-information)
      
      ;; actual-query should now be set
      (ok (not (null (slot-value q 'clails/model/query::actual-query))))
      
      ;; actual-query should be a <query> instance
      (ok (typep (slot-value q 'clails/model/query::actual-query) 
                 'clails/model/query::<query>))
      
      ;; Callbacks should be cleared
      (ok (= 0 (length clails/environment:*query-initialization-callbacks*))))))


(deftest test-placeholder-delegation
  (testing "placeholder should delegate method calls to actual query"
    ;; Reset initialization state
    (setf clails/environment:*table-information-initialized* nil)
    (setf clails/environment:*query-initialization-callbacks* nil)
    
    ;; Create query before initialization
    (let ((q (query <blog>
               :as :blog
               :columns ((blog :id :title))
               :where (:> (:blog :star) :min-star))))
      
      ;; Initialize
      (clails/model/base-model:initialize-table-information)
      
      ;; set-where should work through delegation
      (ok (set-where q '(:= (:blog :status) :status)))
      
      ;; set-limit should work through delegation
      (ok (set-limit q 10))
      
      ;; set-offset should work through delegation
      (ok (set-offset q 5))
      
      ;; set-columns should work through delegation
      (ok (set-columns q '((blog :id :title :content))))
      
      ;; The returned value should be the actual query, not the placeholder
      (let ((result (set-where q '(:= (:blog :id) :id))))
        (ok (typep result 'clails/model/query::<query>))))))


(deftest test-multiple-placeholders
  (testing "multiple query placeholders should all be initialized"
    ;; Reset initialization state
    (setf clails/environment:*table-information-initialized* nil)
    (setf clails/environment:*query-initialization-callbacks* nil)
    
    ;; Create multiple queries before initialization
    (let ((q1 (query <blog>
                :as :blog
                :columns ((blog :id :title))
                :where (:> (:blog :star) 5)))
          (q2 (query <blog>
                :as :blog
                :columns ((blog :id :content))
                :where (:= (:blog :status) :status)))
          (q3 (query <blog>
                :as :blog
                :order-by ((:blog :created-at :desc)))))
      
      ;; All should be placeholders
      (ok (typep q1 'clails/model/query::<query-placeholder>))
      (ok (typep q2 'clails/model/query::<query-placeholder>))
      (ok (typep q3 'clails/model/query::<query-placeholder>))
      
      ;; Three callbacks should be registered
      (ok (= 3 (length clails/environment:*query-initialization-callbacks*)))
      
      ;; Initialize
      (clails/model/base-model:initialize-table-information)
      
      ;; All should now have actual queries
      (ok (not (null (slot-value q1 'clails/model/query::actual-query))))
      (ok (not (null (slot-value q2 'clails/model/query::actual-query))))
      (ok (not (null (slot-value q3 'clails/model/query::actual-query))))
      
      ;; All actual queries should be <query> instances
      (ok (typep (slot-value q1 'clails/model/query::actual-query) 'clails/model/query::<query>))
      (ok (typep (slot-value q2 'clails/model/query::actual-query) 'clails/model/query::<query>))
      (ok (typep (slot-value q3 'clails/model/query::actual-query) 'clails/model/query::<query>))
      
      ;; Callbacks should be cleared
      (ok (= 0 (length clails/environment:*query-initialization-callbacks*))))))
