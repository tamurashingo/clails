(in-package #:cl-user)
(defpackage #:clails-test/model/bulk
  (:use #:cl
        #:rove)
  (:import-from #:clails/model/bulk
                #:query-type-of
                #:format-sql-value
                #:format-sql-with-params
                #:escape-sql-string
                #:show-query-sql))

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
