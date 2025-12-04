(in-package #:cl-user)
(defpackage #:clails-test/model/native-query
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:cl-batis
                #:select
                #:defsql
                #:sql-where
                #:sql-cond
                #:sql-set
                #:gen-sql-and-params))
(in-package #:clails-test/model/native-query)

(cl-syntax:use-syntax :annot)

;;; Tests for native query support using cl-batis
;;; These tests verify query definition and parameter conversion

;; Simple SELECT query with single parameter
(select
 ("select * from users where id = :id")
 (defsql test-find-user (id)))

(deftest test-simple-query-definition
  (testing "Simple SELECT query with single parameter"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-user '(:id 123))
      (ok (string= sql "select * from users where id =   ?"))
      (ok (equal params '(123))))))

;; Query with multiple parameters
(select
 ("select * from posts where user_id = :user_id and status = :status")
 (defsql test-find-posts (user_id status)))

(deftest test-multiple-parameters
  (testing "Query with multiple parameters"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-posts '(:user_id 1 :status "published"))
      (ok (string= sql "select * from posts where user_id =        ? and status =       ?"))
      (ok (equal params '(1 "published"))))))

;; Query with multiple parameters for order testing
(select
 ("select * from users where name = :name and age = :age and email = :email")
 (defsql test-find-by-criteria (name age email)))

(deftest test-parameter-order
  (testing "Parameters are ordered correctly"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-by-criteria '(:name "Alice" :age 30 :email "alice@example.com"))
      (ok (string= sql "select * from users where name =     ? and age =    ? and email =      ?"))
      (ok (equal params '("Alice" 30 "alice@example.com"))))))

;; Same placeholder used multiple times
(select
 ("select * from users where (status = :status or backup_status = :status)")
 (defsql test-find-by-status (status)))

(deftest test-duplicate-placeholders
  (testing "Same placeholder used multiple times"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-by-status '(:status "active"))
      (ok (string= sql "select * from users where (status =       ? or backup_status =       ?)"))
      (ok (equal params '("active" "active"))))))

;; Query without parameters
(select
 ("select * from users")
 (defsql test-all-users ()))

(deftest test-no-parameters
  (testing "Query without parameters"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-all-users '())
      (ok (string= sql "select * from users"))
      (ok (null params)))))

;; Query with dynamic WHERE clause
(select
 ("select * from posts"
  (sql-where
   (sql-cond (not (null user_id))
             " user_id = :user_id ")
   (sql-cond (not (null status))
             " and status = :status "))
  " order by created_at desc")
 (defsql test-dynamic-posts (user_id status)))

(deftest test-dynamic-where-clause
  (testing "sql-where with dynamic conditions - Both parameters provided"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-dynamic-posts '(:user_id 1 :status "published"))
      (ok (string= sql "select * from posts where   user_id =        ?   and status =       ?  order by created_at desc"))
      (ok (equal params '(1 "published")))))

  (testing "sql-where with dynamic conditions - Only user_id provided"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-dynamic-posts '(:user_id 1 :status nil))
      (ok (string= sql "select * from posts where   user_id =        ?  order by created_at desc"))
      (ok (equal params '(1)))))

  (testing "sql-where with dynamic conditions - Only status provided"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-dynamic-posts '(:user_id nil :status "draft"))
      (ok (string= sql "select * from posts where   status =       ?  order by created_at desc"))
      (ok (equal params '("draft")))))

  (testing "sql-where with dynamic conditions - No parameters provided"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-dynamic-posts '(:user_id nil :status nil))
      (ok (string= sql "select * from posts order by created_at desc"))
      (ok (null params)))))

;; Complex JOIN query
(select
 ("select blog.*, comment.* from blog"
  " left join comment on blog.id = comment.blog_id"
  " join calendar on calendar.type = :calendar_type"
  "   and calendar.start_date >= blog.created_at"
  "   and calendar.end_date <= blog.created_at"
  (sql-where " user_name = :user_name "))
 (defsql test-blog-comments (calendar_type user_name)))

(deftest test-complex-join-query
  (testing "Complex JOIN query with multiple conditions"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-blog-comments '(:calendar_type "blog" :user_name "tamura"))
      (ok (string= "select blog.*, comment.* from blog left join comment on blog.id = comment.blog_id join calendar on calendar.type =              ?   and calendar.start_date >= blog.created_at   and calendar.end_date <= blog.created_at where   user_name =          ? " sql))
      (ok (equal params '("blog" "tamura"))))))

;; Aggregate query with GROUP BY
(select
 ("select user_id,"
  "       count(distinct post_id) as post_count,"
  "       count(comment_id) as comment_count"
  " from user_activity"
  (sql-where " created_at >= :start_date"
             " and created_at < :end_date ")
  " group by user_id")
 (defsql test-user-stats (start_date end_date)))

(deftest test-aggregate-query
  (testing "Aggregate query with GROUP BY"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-user-stats '(:start_date "2025-01-01" :end_date "2025-02-01"))
      (ok (string= "select user_id,       count(distinct post_id) as post_count,       count(comment_id) as comment_count from user_activity where   created_at >=           ?  and created_at <         ?  group by user_id" sql))
      (ok (equal params '("2025-01-01" "2025-02-01"))))))

;; Annotation style query - simple
@select
("select * from users where email = :email")
(defsql test-find-by-email (email))

;; Annotation style query - with dynamic WHERE
@select
("select * from articles"
 (sql-where
  (sql-cond (not (null category))
            " category = :category ")
  (sql-cond (not (null author))
            " and author = :author "))
 " order by published_at desc")
(defsql test-find-articles (category author))

(deftest test-annotation-style
  (testing "@select annotation style - simple query"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-by-email '(:email "test@example.com"))
      (ok (string= sql "select * from users where email =      ?"))
      (ok (equal params '("test@example.com")))))

  (testing "@select annotation style - with dynamic where - both parameters"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-articles '(:category "tech" :author "alice"))
      (ok (string= sql "select * from articles where   category =         ?   and author =       ?  order by published_at desc"))
      (ok (equal params '("tech" "alice")))))

  (testing "@select annotation style - with dynamic where - only category"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-articles '(:category "tech" :author nil))
      (ok (string= sql "select * from articles where   category =         ?  order by published_at desc"))
      (ok (equal params '("tech")))))

  (testing "@select annotation style - with dynamic where - no parameters"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-articles '(:category nil :author nil))
      (ok (string= sql "select * from articles order by published_at desc"))
      (ok (null params)))))

;; Snake case parameters
(select
 ("select * from orders where user_id = :user_id and order_date = :order_date")
 (defsql test-find-orders (user_id order_date)))

(deftest test-snake-case-parameters
  (testing "Parameters using snake_case convention"
    (multiple-value-bind (sql params)
        (gen-sql-and-params test-find-orders '(:user_id 100 :order_date "2025-01-15"))
      (ok (string= sql "select * from orders where user_id =        ? and order_date =           ?"))
      (ok (equal params '(100 "2025-01-15"))))))
