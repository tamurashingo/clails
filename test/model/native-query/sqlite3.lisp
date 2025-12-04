(in-package #:cl-user)
(defpackage #:clails-test/model/native-query/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/connection
                #:get-connection)
  (:import-from #:clails/environment
                #:*database-type*
                #:*project-environment*
                #:*database-config*
                #:*migration-base-dir*
                #:<database-type-sqlite3>)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel)
  (:import-from #:cl-batis
                #:select
                #:defsql
                #:sql-where
                #:sql-cond
                #:gen-sql-and-params))

(defpackage #:clails-test/model/native-query-db-sqlite3
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/native-query/sqlite3)

(cl-syntax:use-syntax :annot)

;;; Tests

(in-package #:clails-test/model/native-query/sqlite3)

(setup
  (setf *database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
  (setf *project-environment* :test)
  (setf *database-config*
        `(:test (:database-name ,(format nil "~A/volumes/clails_test.sqlite3" (env-or-default "CLAILS_SQLITE3_DATABASE" "/app")))))
  (setf *migration-base-dir* (uiop:pathname-directory-pathname *load-truename*))

  (clails/model/migration::db-create)

  ;; Create tables directly
  (clails/model/connection::with-db-connection-direct (conn)
    (clails/model/migration:create-table conn :table "users"
                                              :columns '(("name" :type :string)
                                                         ("email" :type :string)))
    (clails/model/migration:create-table conn :table "posts"
                                              :columns '(("user_id" :type :integer)
                                                         ("title" :type :string)
                                                         ("status" :type :string))))

  (clails/model/connection::with-db-connection-direct (conn)
    (dbi:do-sql conn
      "insert into users (name, email, created_at, updated_at) values (?, ?, datetime('now'), datetime('now'))"
      (list "Alice" "alice@example.com"))
    (dbi:do-sql conn
      "insert into users (name, email, created_at, updated_at) values (?, ?, datetime('now'), datetime('now'))"
      (list "Bob" "bob@example.com"))
    (dbi:do-sql conn
      "insert into users (name, email, created_at, updated_at) values (?, ?, datetime('now'), datetime('now'))"
      (list "Charlie" "charlie@example.com"))

    (dbi:do-sql conn
      "insert into posts (user_id, title, status, created_at, updated_at) values (?, ?, ?, datetime('now'), datetime('now'))"
      (list 1 "First Post" "published"))
    (dbi:do-sql conn
      "insert into posts (user_id, title, status, created_at, updated_at) values (?, ?, ?, datetime('now'), datetime('now'))"
      (list 1 "Second Post" "draft"))
    (dbi:do-sql conn
      "insert into posts (user_id, title, status, created_at, updated_at) values (?, ?, ?, datetime('now'), datetime('now'))"
      (list 2 "Bob's Post" "published"))
    (dbi:commit conn))

  (clails/model/connection:startup-connection-pool))

(teardown
  (clails/model/connection:shutdown-connection-pool))


;; Simple query
(select
 ("select * from users where id = :id")
 (defsql find-user-by-id (id)))

(deftest test-execute-query-simple
  (let ((result (execute-query find-user-by-id '(:id 1))))
    (ok (= 1 (length result)))
    (ok (= 1 (getf (first result) :|id|)))
    (ok (string= "Alice" (getf (first result) :|name|)))
    (ok (string= "alice@example.com" (getf (first result) :|email|)))))

;; Query with multiple parameters
(select
 ("select * from users where name = :name and email = :email")
 (defsql find-user-by-name-and-email (name email)))

(deftest test-execute-query-multiple-parameters
  (let ((result (execute-query find-user-by-name-and-email '(:name "Bob" :email "bob@example.com"))))
    (ok (= 1 (length result)))
    (ok (= 2 (getf (first result) :|id|)))
    (ok (string= "Bob" (getf (first result) :|name|)))))

(deftest test-execute-query-no-results
  (let ((result (execute-query find-user-by-id '(:id 999))))
    (ok (null result))))

;; Query with dynamic WHERE
(select
 ("select * from posts"
  (sql-where
   (sql-cond (not (null user_id))
             " user_id = :user_id ")
   (sql-cond (not (null status))
             " and status = :status "))
  " order by id")
 (defsql find-posts (user_id status)))

(deftest test-execute-query-dynamic-where
  (testing "both parameters"
    (let ((result (execute-query find-posts '(:user_id 1 :status "published"))))
      (ok (= 1 (length result)))
      (ok (string= "First Post" (getf (first result) :|title|)))))

  (testing "only user_id"
    (let ((result (execute-query find-posts '(:user_id 1 :status nil))))
      (ok (= 2 (length result)))))

  (testing "only status"
    (let ((result (execute-query find-posts '(:user_id nil :status "published"))))
      (ok (= 2 (length result)))))

  (testing "no parameters"
    (let ((result (execute-query find-posts '(:user_id nil :status nil))))
      (ok (= 3 (length result))))))

;; Annotation style
@select
("select * from users where email = :email")
(defsql find-user-by-email (email))

(deftest test-execute-query-annotation-style
  (let ((result (execute-query find-user-by-email '(:email "charlie@example.com"))))
    (ok (= 1 (length result)))
    (ok (= 3 (getf (first result) :|id|)))
    (ok (string= "Charlie" (getf (first result) :|name|)))))

;; Query returning all records
(select
 ("select * from users order by id")
 (defsql all-users ()))

(deftest test-execute-query-all-records
  (let ((result (execute-query all-users '())))
    (ok (= 3 (length result)))))

(deftest test-execute-query-result-format
  (let ((result (execute-query find-user-by-id '(:id 1))))
    (ok (listp result))
    (ok (listp (first result)))
    (ok (keywordp (first (first result))))))

;; JOIN query
(select
 ("select u.id, u.name, p.title from users u inner join posts p on u.id = p.user_id where u.id = :user_id order by p.id")
 (defsql find-user-posts-join (user_id)))

(deftest test-execute-query-join
  (let ((result (execute-query find-user-posts-join '(:user_id 1))))
    (ok (= 2 (length result)))
    (ok (= 1 (getf (first result) :|id|)))
    (ok (string= "Alice" (getf (first result) :|name|)))
    (ok (string= "First Post" (getf (first result) :|title|)))
    (ok (string= "Second Post" (getf (second result) :|title|)))))

;; Aggregation query with COUNT
(select
 ("select status, count(*) as count from posts where user_id = :user_id group by status order by status")
 (defsql count-posts-by-status (user_id)))

(deftest test-execute-query-aggregation-count
  (let ((result (execute-query count-posts-by-status '(:user_id 1))))
    (ok (= 2 (length result)))
    (ok (string= "draft" (getf (first result) :|status|)))
    (ok (= 1 (getf (first result) :|count|)))
    (ok (string= "published" (getf (second result) :|status|)))
    (ok (= 1 (getf (second result) :|count|)))))

;; Aggregation with GROUP BY and HAVING
(select
 ("select user_id, count(*) as post_count from posts group by user_id having count(*) > :min_count order by user_id")
 (defsql find-active-users (min_count)))

(deftest test-execute-query-aggregation-having
  (let ((result (execute-query find-active-users '(:min_count 1))))
    (ok (= 1 (length result)))
    (ok (= 1 (getf (first result) :|user_id|)))
    (ok (= 2 (getf (first result) :|post_count|)))))

;; Subquery
(select
 ("select * from users where id in (select user_id from posts where status = :status)")
 (defsql find-users-by-post-status (status)))

(deftest test-execute-query-subquery
  (let ((result (execute-query find-users-by-post-status '(:status "published"))))
    (ok (= 2 (length result)))
    (ok (member (getf (first result) :|name|) '("Alice" "Bob") :test #'string=))))

;; Window function (SQLite 3.25.0+)
(select
 ("select u.name, p.title, row_number() over (partition by u.id order by p.id) as row_num from users u inner join posts p on u.id = p.user_id where u.id <= :max_user_id order by u.id, p.id")
 (defsql find-posts-with-row-number (max_user_id)))

(deftest test-execute-query-window-function
  (let ((result (execute-query find-posts-with-row-number '(:max_user_id 2))))
    (ok (= 3 (length result)))
    (ok (= 1 (getf (first result) :|row_num|)))
    (ok (= 2 (getf (second result) :|row_num|)))
    (ok (= 1 (getf (third result) :|row_num|)))))

;; WITH clause (CTE - Common Table Expression)
(select
 ("with user_stats as (select user_id, count(*) as post_count from posts group by user_id) select u.id, u.name, coalesce(us.post_count, 0) as post_count from users u left join user_stats us on u.id = us.user_id where u.id >= :min_user_id order by u.id")
 (defsql find-users-with-post-counts (min_user_id)))

(deftest test-execute-query-with-clause
  (let ((result (execute-query find-users-with-post-counts '(:min_user_id 1))))
    (ok (= 3 (length result)))
    (ok (= 1 (getf (first result) :|id|)))
    (ok (= 2 (getf (first result) :|post_count|)))
    (ok (= 2 (getf (second result) :|id|)))
    (ok (= 1 (getf (second result) :|post_count|)))
    (ok (= 3 (getf (third result) :|id|)))
    (ok (= 0 (getf (third result) :|post_count|)))))

;; Complex WITH clause with multiple CTEs
(select
 ("with published_posts as (select user_id, count(*) as count from posts where status = :published_status group by user_id), draft_posts as (select user_id, count(*) as count from posts where status = :draft_status group by user_id) select u.id, u.name, coalesce(pp.count, 0) as published_count, coalesce(dp.count, 0) as draft_count from users u left join published_posts pp on u.id = pp.user_id left join draft_posts dp on u.id = dp.user_id order by u.id")
 (defsql find-users-with-post-status-counts (published_status draft_status)))

(deftest test-execute-query-complex-with-clause
  (let ((result (execute-query find-users-with-post-status-counts '(:published_status "published" :draft_status "draft"))))
    (ok (= 3 (length result)))
    (ok (= 1 (getf (first result) :|id|)))
    (ok (= 1 (getf (first result) :|published_count|)))
    (ok (= 1 (getf (first result) :|draft_count|)))
    (ok (= 2 (getf (second result) :|id|)))
    (ok (= 1 (getf (second result) :|published_count|)))
    (ok (= 0 (getf (second result) :|draft_count|)))))

;; INSERT query
(cl-batis:update
 ("insert into users (name, email, created_at, updated_at) values (:name, :email, datetime('now'), datetime('now'))")
 (cl-batis:defsql insert-user (name email)))

(deftest test-execute-insert-query
  (let ((rows (execute-query insert-user '(:name "David" :email "david@example.com"))))
    (ok (= 1 rows))))

;; UPDATE query
(cl-batis:update
 ("update posts set title = :title, updated_at = datetime('now') where id = :id")
 (cl-batis:defsql update-post-title (title id)))

(deftest test-execute-update-query
  (let ((rows (execute-query update-post-title '(:title "Updated Title" :id 1))))
    (ok (= 1 rows))))

;; DELETE query
(cl-batis:update
 ("delete from posts where id = :id")
 (cl-batis:defsql delete-post (id)))

(deftest test-execute-delete-query
  (let ((rows (execute-query delete-post '(:id 3))))
    (ok (= 1 rows))))
