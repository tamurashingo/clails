(in-package #:cl-user)
(defpackage #:clails-test/model/query-cache
  (:use #:cl
        #:rove
        #:clails/model)
  (:import-from #:clails/model/query
                #:query
                #:query-builder
                #:set-columns
                #:set-joins
                #:set-where
                #:set-order-by
                #:set-limit
                #:set-offset
                #:execute-query
                #:generate-query)
  (:import-from #:clails/model/connection
                #:startup-connection-pool
                #:shutdown-connection-pool
                #:get-connection)
  (:import-from #:clails/model/migration
                #:db-create
                #:db-migrate)
  (:import-from #:clails/logger
                #:clear-loggers
                #:register-logger
                #:make-console-appender
                #:<text-formatter>)
  (:import-from #:clails/util
                #:env-or-default))

(defpackage #:clails-test/model/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-index
                #:drop-table))

(in-package #:clails-test/model/query-cache)


(setup
  (clear-loggers)
  (register-logger
   :sql
   :appender (make-console-appender
              :formatter (make-instance '<text-formatter>))
   :level :trace)

  (clrhash clails/model/base-model::*table-information*)
  
  ;; Define test model
  (defmodel <todo> (<base-model>)
    (:table "todo"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0001" "/app/test/data/0001-migration-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (db-create)
  (db-migrate)
  (startup-connection-pool)
  (initialize-table-information)
  
  ;; Insert test data
  (let ((connection (get-connection)))
    ;; Clear existing data first
    (dbi-cp:execute (dbi-cp:prepare connection "DELETE FROM todo") '())
    
    (dbi-cp:execute (dbi-cp:prepare connection "INSERT INTO todo (title, done, done_at, created_at, updated_at) VALUES (?, ?, ?, ?, ?)")
                    (list "First Task" 1 "2024-01-01 00:00:00" "2024-01-01 00:00:00" "2024-01-01 00:00:00"))
    (dbi-cp:execute (dbi-cp:prepare connection "INSERT INTO todo (title, done, done_at, created_at, updated_at) VALUES (?, ?, ?, ?, ?)")
                    (list "Second Task" 0 nil "2024-01-02 00:00:00" "2024-01-02 00:00:00"))
    (dbi-cp:execute (dbi-cp:prepare connection "INSERT INTO todo (title, done, done_at, created_at, updated_at) VALUES (?, ?, ?, ?, ?)")
                    (list "Third Task" 1 "2024-01-03 00:00:00" "2024-01-03 00:00:00" "2024-01-03 00:00:00"))))


(teardown
  (shutdown-connection-pool)
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t))


;;;; ----------------------------------------
;;;; Unit Tests

(deftest query-cache-initial-state
  (testing "Cache is invalid initially"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))

(deftest query-cache-generation
  (testing "Cache is generated on first generate-query call"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; First call - cache should be generated
      (generate-query q '(:done 1))
      
      (ok (not (null (slot-value q 'clails/model/query::query-cache))))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Verify cache structure
      (let ((cache (slot-value q 'clails/model/query::query-cache)))
        (ok (getf cache :sql-template))
        (ok (getf cache :where-params))
        (ok (getf cache :where-columns))
        (ok (getf cache :column-types))))))

(deftest query-cache-usage
  (testing "Cached query is returned on subsequent calls"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; Generate cache
      (generate-query q '(:done 1))
      
      ;; Set a dummy string in cache to verify it's being used
      (setf (slot-value q 'clails/model/query::query-cache)
            (list :sql-template "DUMMY SQL TEMPLATE"
                  :where-params '(:done)
                  :where-columns '((:todo :done))
                  :column-types '(:boolean)
                  :limit-param nil
                  :offset-param nil))
      
      ;; Second call - should use the dummy cache
      (multiple-value-bind (sql params)
          (generate-query q '(:done 0))
        (ok (string= sql "DUMMY SQL TEMPLATE"))
        (ok (equal params '(0)))))))

(deftest query-cache-invalidation-by-set-columns
  (testing "Cache is invalidated when set-columns is called"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; Generate cache
      (generate-query q '(:done 1))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Call set-columns
      (set-columns q '((todo :id :title)))
      
      ;; Cache should be invalidated
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))

(deftest query-cache-invalidation-by-set-where
  (testing "Cache is invalidated when set-where is called"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-where q '(:= (:todo :done) :done))
      
      ;; Generate cache
      (generate-query q '(:done 1))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Call set-where
      (set-where q '(:not-null (:todo :done-at)))
      
      ;; Cache should be invalidated
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))

(deftest query-cache-invalidation-by-set-order-by
  (testing "Cache is invalidated when set-order-by is called"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; Generate cache
      (generate-query q '(:done 1))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Call set-order-by
      (set-order-by q '((:todo :id :desc)))
      
      ;; Cache should be invalidated
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))

(deftest query-cache-invalidation-by-set-limit
  (testing "Cache is invalidated when set-limit is called"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; Generate cache
      (generate-query q '(:done 1))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Call set-limit
      (set-limit q 10)
      
      ;; Cache should be invalidated
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))

(deftest query-cache-invalidation-by-set-offset
  (testing "Cache is invalidated when set-offset is called"
    (let ((q (query <todo> :as :todo :where (:= (:todo :done) :done))))
      ;; Generate cache
      (generate-query q '(:done 1))
      (ok (slot-value q 'clails/model/query::query-cache-valid-p))
      
      ;; Call set-offset
      (set-offset q 20)
      
      ;; Cache should be invalidated
      (ok (null (slot-value q 'clails/model/query::query-cache)))
      (ok (null (slot-value q 'clails/model/query::query-cache-valid-p))))))


;;;; ----------------------------------------
;;;; Integration Tests

(deftest query-cache-integration-first-and-second-execution
  (testing "First and second executions return the same results"
    (let ((q (query <todo>
               :as :todo
               :columns ((todo :id :title :done))
               :where (:= (:todo :done) :done)
               :order-by ((:todo :id)))))
      
      ;; First execution (no cache)
      (let ((results1 (execute-query q '(:done 1))))
        
        ;; Second execution (with cache)
        (let ((results2 (execute-query q '(:done 1))))
          
          ;; Results should be identical
          (ok (= (length results1) (length results2)))
          (ok (= (length results1) 2))
          
          ;; Verify first result
          (ok (string= (ref (first results1) :title) (ref (first results2) :title)))
          (ok (eq (ref (first results1) :done) (ref (first results2) :done))))))))

(deftest query-cache-integration-different-parameters
  (testing "Different parameters return different results with cache"
    (let ((q (query <todo>
               :as :todo
               :columns ((todo :id :title :done))
               :where (:= (:todo :done) :done)
               :order-by ((:todo :id)))))
      
      ;; First execution with done=1
      (let ((results1 (execute-query q '(:done 1))))
        
        ;; Second execution with done=0 (cache should be reused, but parameters are different)
        (let ((results2 (execute-query q '(:done 0))))
          
          ;; Results should be different
          (ok (= (length results1) 2))
          (ok (= (length results2) 1))
          
          (ok (eq (ref (first results1) :done) T))
          (ok (eq (ref (first results2) :done) NIL)))))))

(deftest query-cache-integration-query-builder
  (testing "Query builder with cache works correctly"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title :done)))
      (set-where q '(:= (:todo :done) :done))
      (set-order-by q '((:todo :id)))
      
      ;; First execution (no cache)
      (let ((results1 (execute-query q '(:done 1))))
        
        ;; Second execution (with cache)
        (let ((results2 (execute-query q '(:done 1))))
          
          ;; Results should be identical
          (ok (= (length results1) (length results2)))
          (ok (= (length results1) 2))
          
          ;; All results should have done=T
          (ok (every #'(lambda (r) (eq (ref r :done) T)) results1))
          (ok (every #'(lambda (r) (eq (ref r :done) T)) results2)))))))
