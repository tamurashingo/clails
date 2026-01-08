(in-package #:cl-user)
(defpackage #:clails-test/model/bulk
  (:use #:cl
        #:rove)
  (:import-from #:clails/model/bulk
                #:query-type-of
                #:format-sql-value
                #:format-sql-with-params
                #:escape-sql-string
                #:show-query-sql)
  (:import-from #:clails/model/query
                #:make-record)
  (:import-from #:clails/model/base-model
                #:ref))

(in-package #:clails-test/model/bulk)

;;;; ----------------------------------------
;;;; Helper Function Tests
;;;; ----------------------------------------

(deftest test-query-type-of
  (testing "query-type-of with unsupported type"
    (ok (signals (query-type-of "string") 'error)
        "Should raise error for unsupported type"))

  (testing "query-type-of with <query> object"
    ;; Note: This will be tested in database-specific tests
    (pass "Skipping <query> object test (requires database setup)"))

  (testing "query-type-of with <batis-sql> object"
    ;; Note: This will be tested in database-specific tests
    (pass "Skipping <batis-sql> object test (requires database setup)")))


(deftest test-format-sql-value
  (testing "format-sql-value with NULL"
    (ok (string= (format-sql-value nil) "NULL")
        "NIL should be formatted as NULL"))

  (testing "format-sql-value with TRUE"
    (ok (string= (format-sql-value t) "TRUE")
        "T should be formatted as TRUE"))

  (testing "format-sql-value with string"
    (ok (string= (format-sql-value "test") "'test'")
        "String should be quoted"))

  (testing "format-sql-value with number"
    (ok (string= (format-sql-value 42) "42")
        "Number should not be quoted"))

  (testing "format-sql-value with keyword"
    (ok (string= (format-sql-value :active) "'active'")
        "Keyword should be lowercase and quoted")))


(deftest test-escape-sql-string
  (testing "escape-sql-string with single quote"
    (ok (string= (escape-sql-string "test's") "test''s")
        "Single quote should be doubled"))

  (testing "escape-sql-string with multiple quotes"
    (ok (string= (escape-sql-string "it's a test's") "it''s a test''s")
        "Multiple single quotes should be doubled"))

  (testing "escape-sql-string without quotes"
    (ok (string= (escape-sql-string "test") "test")
        "String without quotes should remain unchanged")))


(deftest test-format-sql-with-params
  (testing "format-sql-with-params with single parameter"
    (let ((sql "SELECT * FROM users WHERE name = ?")
          (params '("John")))
      (ok (string= (format-sql-with-params sql params)
                   "SELECT * FROM users WHERE name = 'John'")
          "Parameter should be embedded")))

  (testing "format-sql-with-params with multiple parameters"
    (let ((sql "SELECT * FROM users WHERE name = ? AND age > ?")
          (params '("John" 30)))
      (ok (string= (format-sql-with-params sql params)
                   "SELECT * FROM users WHERE name = 'John' AND age > 30")
          "Multiple parameters should be embedded")))

  (testing "format-sql-with-params with NULL parameter"
    (let ((sql "SELECT * FROM users WHERE name = ?")
          (params '(nil)))
      (ok (string= (format-sql-with-params sql params)
                   "SELECT * FROM users WHERE name = NULL")
          "NULL parameter should be embedded")))

  (testing "format-sql-with-params with no parameters"
    (let ((sql "SELECT * FROM users")
          (params '()))
      (ok (string= (format-sql-with-params sql params)
                   "SELECT * FROM users")
          "SQL without parameters should remain unchanged"))))


(deftest test-show-query-sql
  (testing "show-query-sql with <query> object"
    ;; Note: Tested in database-specific tests (requires database setup)
    (pass "Skipping <query> object test (tested in DB-specific tests)"))

  (testing "show-query-sql with <batis-sql> object"
    ;; Note: Tested in database-specific tests (requires database setup)
    (pass "Skipping <batis-sql> object test (tested in DB-specific tests)"))

  (testing "show-query-sql with string should raise error"
    (ok (signals (show-query-sql "SELECT * FROM users" '()) 'error)
        "Should raise error for string SQL")))


;;;; ----------------------------------------
;;;; Insert Helper Function Tests
;;;; ----------------------------------------

(deftest test-split-into-batches
  (testing "split-into-batches with even division"
    (let ((items '(1 2 3 4 5 6))
          (result (clails/model/bulk::split-into-batches '(1 2 3 4 5 6) 2)))
      (ok (= (length result) 3) "Should have 3 batches")
      (ok (equal (first result) '(1 2)) "First batch should be (1 2)")
      (ok (equal (second result) '(3 4)) "Second batch should be (3 4)")
      (ok (equal (third result) '(5 6)) "Third batch should be (5 6)")))

  (testing "split-into-batches with uneven division"
    (let ((result (clails/model/bulk::split-into-batches '(1 2 3 4 5 6 7) 3)))
      (ok (= (length result) 3) "Should have 3 batches")
      (ok (equal (first result) '(1 2 3)) "First batch should be (1 2 3)")
      (ok (equal (second result) '(4 5 6)) "Second batch should be (4 5 6)")
      (ok (equal (third result) '(7)) "Third batch should be (7)")))

  (testing "split-into-batches with batch size larger than list"
    (let ((result (clails/model/bulk::split-into-batches '(1 2 3) 10)))
      (ok (= (length result) 1) "Should have 1 batch")
      (ok (equal (first result) '(1 2 3)) "Batch should contain all items")))

  (testing "split-into-batches with empty list"
    (let ((result (clails/model/bulk::split-into-batches '() 5)))
      (ok (= (length result) 0) "Should have 0 batches")))

  (testing "split-into-batches with batch size 1"
    (let ((result (clails/model/bulk::split-into-batches '(1 2 3) 1)))
      (ok (= (length result) 3) "Should have 3 batches")
      (ok (every (lambda (batch) (= (length batch) 1)) result)
          "Each batch should have 1 item"))))


(deftest test-make-placeholders
  (testing "make-placeholders with single placeholder"
    (ok (string= (clails/model/bulk::make-placeholders 1) "(?)")
        "Should create single placeholder"))

  (testing "make-placeholders with multiple placeholders"
    (ok (string= (clails/model/bulk::make-placeholders 3) "(?, ?, ?)")
        "Should create 3 placeholders"))

  (testing "make-placeholders with 5 placeholders"
    (ok (string= (clails/model/bulk::make-placeholders 5) "(?, ?, ?, ?, ?)")
        "Should create 5 placeholders"))

  (testing "make-placeholders with zero placeholders"
    (ok (string= (clails/model/bulk::make-placeholders 0) "()")
        "Should create empty placeholder")))


(deftest test-make-multi-row-values
  (testing "make-multi-row-values with single row"
    (ok (string= (clails/model/bulk::make-multi-row-values "(?, ?)" 1) "(?, ?)")
        "Should create single row VALUES"))

  (testing "make-multi-row-values with multiple rows"
    (ok (string= (clails/model/bulk::make-multi-row-values "(?, ?, ?)" 3) "(?, ?, ?), (?, ?, ?), (?, ?, ?)")
        "Should create 3 row VALUES"))

  (testing "make-multi-row-values with zero rows"
    (ok (string= (clails/model/bulk::make-multi-row-values "(?, ?)" 0) "")
        "Should create empty VALUES")))


(deftest test-prepare-timestamps-plist
  (testing "prepare-timestamps-plist sets created-at when not present"
    (let* ((now 3900000000)
           (plist '(:name "Test"))
           (result (clails/model/bulk::prepare-timestamps-plist plist '(:name :created-at :updated-at) now)))
      (ok (= (getf result :created-at) now) "Should set created-at")
      (ok (= (getf result :updated-at) now) "Should set updated-at")
      (ok (string= (getf result :name) "Test") "Should preserve existing values")))

  (testing "prepare-timestamps-plist does not overwrite existing timestamps"
    (let* ((now 3900000000)
           (existing-time 3800000000)
           (plist `(:name "Test" :created-at ,existing-time :updated-at ,existing-time))
           (result (clails/model/bulk::prepare-timestamps-plist plist '(:name :created-at :updated-at) now)))
      (ok (= (getf result :created-at) existing-time) "Should not overwrite existing created-at")
      (ok (= (getf result :updated-at) existing-time) "Should not overwrite existing updated-at")))

  (testing "prepare-timestamps-plist does not add timestamps when not in columns"
    (let* ((now 3900000000)
           (plist '(:name "Test"))
           (result (clails/model/bulk::prepare-timestamps-plist plist '(:name) now)))
      (ok (null (getf result :created-at)) "Should not add created-at")
      (ok (null (getf result :updated-at)) "Should not add updated-at"))))


(deftest test-prepare-timestamps-instance
  (testing "prepare-timestamps-instance sets timestamps when not present"
    ;; Note: This requires a model class definition, which needs database setup
    ;; We'll use a simple test that checks the function doesn't error
    (pass "Skipping instance test (requires model setup)"))

  (testing "prepare-timestamps-instance preserves existing values"
    ;; Note: This requires a model class definition, which needs database setup
    (pass "Skipping instance test (requires model setup)")))
