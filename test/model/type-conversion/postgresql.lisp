(in-package #:cl-user)
(defpackage #:clails-test/model/type-conversion/postgresql
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))
(in-package #:clails-test/model/type-conversion/postgresql)


(setup
  (clrhash clails/model/base-model::*table-information*)
  
  (defmodel <type-conversion-test> (<base-model>)
    (:table "type_conversion_test"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                                                      :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                                                      :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0012" "/app/test/data/0012-type-conversion-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  
  ;; Insert test data
  (clails/model/connection::with-db-connection-direct (connection)
    (dbi-cp:do-sql connection "INSERT INTO type_conversion_test (created_at, updated_at, title, is_active, priority, score, completed_at) VALUES ('2026-01-25 00:00:00', '2026-01-25 00:00:00', 'Active Task 1', true, 10, 95.5, '2026-01-24 10:00:00')")
    (dbi-cp:do-sql connection "INSERT INTO type_conversion_test (created_at, updated_at, title, is_active, priority, score, completed_at) VALUES ('2026-01-25 00:00:01', '2026-01-25 00:00:01', 'Active Task 2', true, 20, 87.3, '2026-01-24 11:00:00')")
    (dbi-cp:do-sql connection "INSERT INTO type_conversion_test (created_at, updated_at, title, is_active, priority, score, completed_at) VALUES ('2026-01-25 00:00:02', '2026-01-25 00:00:02', 'Inactive Task 1', false, 5, 50.0, NULL)")
    (dbi-cp:do-sql connection "INSERT INTO type_conversion_test (created_at, updated_at, title, is_active, priority, score, completed_at) VALUES ('2026-01-25 00:00:03', '2026-01-25 00:00:03', 'Inactive Task 2', false, 15, 72.8, NULL)"))
  (clails/model/connection:startup-connection-pool)
  (clails/model/base-model:initialize-table-information))


(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))


(deftest test-boolean-type-conversion-postgresql
  (testing "Query with boolean true"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:= (:test :is-active) :active)
                         :order-by ((:test :id))))
           (result (execute-query query '(:active t))))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 1" (ref (first result) :title)))
      (ok (string= "Active Task 2" (ref (second result) :title)))
      (ok (ref (first result) :is-active))
      (ok (ref (second result) :is-active))))

  (testing "Query with boolean false"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:= (:test :is-active) :active)
                         :order-by ((:test :id))))
           (result (execute-query query '(:active nil))))
      (ok (= 2 (length result)))
      (ok (string= "Inactive Task 1" (ref (first result) :title)))
      (ok (string= "Inactive Task 2" (ref (second result) :title)))
      (ok (not (ref (first result) :is-active)))
      (ok (not (ref (second result) :is-active)))))

  (testing "Query with boolean true using != operator"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:!= (:test :is-active) :active)
                         :order-by ((:test :id))))
           (result (execute-query query '(:active nil))))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 1" (ref (first result) :title)))
      (ok (ref (first result) :is-active)))))


(deftest test-integer-type-conversion-postgresql
  (testing "Query with integer comparison"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:> (:test :priority) :min-priority)
                         :order-by ((:test :id))))
           (result (execute-query query '(:min-priority 10))))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 2" (ref (first result) :title)))
      (ok (= 20 (ref (first result) :priority)))))

  (testing "Query with integer BETWEEN"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:between (:test :priority) :min :max)
                         :order-by ((:test :id))))
           (result (execute-query query '(:min 10 :max 19))))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 1" (ref (first result) :title)))
      (ok (string= "Inactive Task 2" (ref (second result) :title))))))


(deftest test-datetime-type-conversion-postgresql
  (testing "Query with datetime comparison"
    (let* ((completed-time (encode-universal-time 0 0 10 24 1 2026))
           (query (query <type-conversion-test>
                         :as :test
                         :where (:>= (:test :completed-at) :completed-time)
                         :order-by ((:test :id))))
           (result (execute-query query `(:completed-time ,completed-time))))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 1" (ref (first result) :title)))
      (ok (string= "Active Task 2" (ref (second result) :title))))))


(deftest test-multiple-conditions-with-conversion-postgresql
  (testing "Query with multiple type conversions"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:and (:= (:test :is-active) :active)
                                      (:> (:test :priority) :min-priority))
                         :order-by ((:test :id))))
           (result (execute-query query '(:active t :min-priority 15))))
      (ok (= 1 (length result)))
      (ok (string= "Active Task 2" (ref (first result) :title)))
      (ok (ref (first result) :is-active))
      (ok (= 20 (ref (first result) :priority))))))


(deftest test-convert-types-option-postgresql
  (testing "Query without type conversion using raw values"
    (let* ((query (query <type-conversion-test>
                         :as :test
                         :where (:= (:test :is-active) :active)
                         :order-by ((:test :id))))
           (result (execute-query query '(:active t) :convert-types nil)))
      (ok (= 2 (length result)))
      (ok (string= "Active Task 1" (ref (first result) :title)))
      (ok (ref (first result) :is-active)))))
