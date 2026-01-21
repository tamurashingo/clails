(in-package #:cl-user)
(defpackage #:clails-test/model/query-builder
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))

(defpackage #:clails-test/model/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))
(in-package #:clails-test/model/query-builder)

(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <todo> (<base-model>)
    (:table "todo"))

  ;; Enable SQL logging
  ;(setf clails/logger:*log-config* '((:sql :level :debug)))
  (clails/logger:register-logger :sql :level :debug :appender (clails/logger:make-console-appender :formatter (make-instance 'clails/logger:<text-formatter>)))
  
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
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    (dbi-cp:do-sql connection "delete from todo")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:00', '2024-01-02 13:00:00', 'create program', true, '2024-01-02 13:00:00')")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:01', '2024-01-02 13:00:00', 'create pull request', false, null)")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:02', '2024-01-02 13:00:01', 'merge pr', false, null)"))
  (clails/model/connection:startup-connection-pool)
  (clails/model/base-model:initialize-table-information))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))

(deftest test-query-builder-creation
  (testing "Create query-builder instance"
    (let ((q (query-builder '<todo> :as :todo)))
      (ok (typep q '<query>)))))

(deftest test-query-builder-requires-as
  (testing "query-builder requires :as parameter"
    (ok (signals (query-builder '<todo>) 'error))))

(deftest test-set-columns
  (testing "Set columns in query-builder"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title :done)))
      (let ((results (execute-query q nil)))
        (ok (= 3 (length results)))
        (ok (ref (first results) :id))
        (ok (ref (first results) :title))
        (ok (ref (first results) :done))))))

(deftest test-set-columns-multiple-aliases
  (testing "Set columns from multiple table aliases"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (let ((results (execute-query q nil)))
        (ok (= 3 (length results)))
        (ok (ref (first results) :id))
        (ok (ref (first results) :title))
        (ng (ref (first results) :done))))))

(deftest test-set-where
  (testing "Set WHERE clause in query-builder"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title :done)))
      (set-where q '(:= (:todo :done) :done))
      (let ((results (execute-query q '(:done 1))))  ; Use 1 for true
        (ok (= 1 (length results)))
        (ok (string= "create program" (ref (first results) :title)))
        (ok (ref (first results) :done))))))

(deftest test-set-where-complex
  (testing "Set complex WHERE clause with AND/OR"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title :done)))
      (set-where q '(:and (:= (:todo :done) :done)
                          (:like (:todo :title) :keyword)))
      (let ((results (execute-query q '(:done 1 :keyword "%program%"))))  ; Use 1 for true
        (ok (= 1 (length results)))
        (ok (string= "create program" (ref (first results) :title)))
        (ok (ref (first results) :done))))))

(deftest test-set-order-by
  (testing "Set ORDER BY clause"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-order-by q '((:todo :title :desc)))
      (let ((results (execute-query q nil)))
        (ok (= 3 (length results)))
        (ok (string= "merge pr" (ref (first results) :title)))
        (ok (string= "create pull request" (ref (second results) :title)))
        (ok (string= "create program" (ref (third results) :title)))))))

(deftest test-set-limit
  (testing "Set LIMIT clause with integer"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-order-by q '((:todo :id :asc)))
      (set-limit q 2)
      (let ((results (execute-query q nil)))
        (ok (= 2 (length results)))
        (ok (string= "create program" (ref (first results) :title)))
        (ok (string= "create pull request" (ref (second results) :title)))))))

(deftest test-set-limit-parameter
  (testing "Set LIMIT clause with parameter keyword"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-order-by q '((:todo :id :asc)))
      (set-limit q :limit-param)
      (let ((results (execute-query q '(:limit-param 1))))
        (ok (= 1 (length results)))
        (ok (string= "create program" (ref (first results) :title)))))))

(deftest test-set-offset
  (testing "Set OFFSET clause with integer"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-order-by q '((:todo :id :asc)))
      (set-limit q 10)  ; MySQL requires LIMIT with OFFSET
      (set-offset q 1)
      (let ((results (execute-query q nil)))
        (ok (= 2 (length results)))
        (ok (string= "create pull request" (ref (first results) :title)))
        (ok (string= "merge pr" (ref (second results) :title)))))))

(deftest test-set-offset-parameter
  (testing "Set OFFSET clause with parameter keyword"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-order-by q '((:todo :id :asc)))
      (set-limit q 10)  ; MySQL requires LIMIT with OFFSET
      (set-offset q :offset-param)
      (let ((results (execute-query q '(:offset-param 2))))
        (ok (= 1 (length results)))
        (ok (string= "merge pr" (ref (first results) :title)))))))

(deftest test-method-chaining
  (testing "Method chaining with setter functions"
    (let* ((q (query-builder '<todo> :as :todo))
           (result (set-limit (set-offset (set-columns q '((todo :id :title))) 1) 1)))
      (ok (eq result q))
      (let ((results (execute-query q nil)))
        (ok (= 1 (length results)))))))

(deftest test-dynamic-column-selection
  (testing "Dynamic column selection at runtime"
    (let ((search-column :title))
      (let ((q (query-builder '<todo> :as :todo)))
        (set-columns q `((todo :id ,search-column)))
        (let ((results (execute-query q nil)))
          (ok (= 3 (length results)))
          (ok (ref (first results) :id))
          (ok (ref (first results) :title))
          (ng (ref (first results) :done)))))))

(deftest test-dynamic-where-construction
  (testing "Dynamic WHERE clause construction"
    (let ((conditions nil))
      (push '(:= (:todo :done) :done) conditions)
      (push '(:like (:todo :title) :keyword) conditions)
      (let ((q (query-builder '<todo> :as :todo)))
        (set-columns q '((todo :id :title :done)))
        (set-where q `(:and ,@(nreverse conditions)))
        (let ((results (execute-query q '(:done 0 :keyword "%pull%"))))  ; Use 0 for false
          (ok (= 1 (length results)))
          (ok (string= "create pull request" (ref (first results) :title)))
          (ng (ref (first results) :done)))))))

(deftest test-replace-columns
  (testing "set-columns replaces existing columns"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-columns q '((todo :id :done)))  ; Include :id to avoid duplicates
      (let ((results (execute-query q nil)))
        (ok (= 3 (length results)))
        (ok (ref (first results) :id))
        (ok (ref (first results) :done))
        ;; title should be nil (not selected)
        (ng (ref (first results) :title))))))

(deftest test-replace-where
  (testing "set-where replaces existing WHERE clause"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-where q '(:= (:todo :done) :done))
      (set-where q '(:like (:todo :title) :keyword))
      (let ((results (execute-query q '(:keyword "%program%"))))
        (ok (= 1 (length results)))
        (ok (string= "create program" (ref (first results) :title)))
        ;; done column should be nil (not selected)
        (ng (ref (first results) :done))))))

(deftest test-clear-where
  (testing "set-where with nil clears WHERE clause"
    (let ((q (query-builder '<todo> :as :todo)))
      (set-columns q '((todo :id :title)))
      (set-where q '(:= (:todo :done) :done))
      (set-where q nil)
      (let ((results (execute-query q nil)))
        (ok (= 3 (length results)))
        ;; done should be nil (not selected)
        (ng (ref (first results) :done))))))
