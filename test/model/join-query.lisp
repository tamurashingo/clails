(in-package #:cl-user)
(format t "===================================================~%")
(format t "1~%")
(defpackage #:clails-test/model/join-query
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:ref-in))
(format t "2~%")

(defpackage #:clails-test/model/db/join-query
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))
(format t "3~%")

(defpackage #:clails-test-model-joinquery
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:ref
                #:<base-model>))
(in-package #:clails-test-model-joinquery)
(format t "4~%")


(in-package #:clails-test/model/join-query)


(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <account> (<base-model>)
    (:table "account"
    :relations ((:has-many "clails-test/model/join-query::<blog>"
                  :as :blogs
                  :foreign-key :account-id)
                (:has-many "clails-test/model/join-query::<comment>"
                  :as :comments
                  :foreign-key :comment-id)
                (:has-many "clails-test/model/join-query::<comment>"
                  :as :approved-comments
                  :foreign-key :approved-id))))

  (defmodel <blog> (<base-model>)
    (:table "blog"
    :relations ((:belongs-to "clails-test/model/join-query::<account>"
                  :column :account
                  :key :account-id)
                (:has-many "clails-test/model/join-query::<comment>"
                  :as :comments
                  :foreign-key :blog-id))))

  (defmodel <comment> (<base-model>)
    (:table "comment"
    :relations ((:belongs-to "clails-test/model/join-query::<blog>"
                  :column :blog
                  :key :blog-id)
                (:belongs-to "clails-test/model/join-query::<account>"
                  :column :comment-account
                  :key :comment-id)
                (:belongs-to "clails-test/model/join-query::<account>"
                  :column :approved-account
                  :key :approved-id))))

 (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
 (setf clails/environment:*project-environment* :test)
 (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                     :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                     :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                     :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                     :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
 (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0002" "/app/test/data/0002-join-test"))
 (uiop:setup-temporary-directory)
 (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
 (setf clails/environment::*project-dir* uiop:*temporary-directory*)
 (clails/model/migration::db-create)
 (clails/model/migration::db-migrate)
 (clails/model/base-model:initialize-table-information)
 (clails/model/connection::with-db-connection-direct (connection)
   (dbi-cp:do-sql connection "insert into account (created_at, updated_at, username) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'user1')")
   (dbi-cp:do-sql connection "insert into account (created_at, updated_at, username) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'user2')")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'first blog', 'this is my first blog', 1, 0)")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'second blog', 'this is my second blog', 1, 10)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id, comment_id, approved_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'hi', 1, 2, 1)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id, comment_id, approved_id) values ('2024-01-02 00:00:01', '2024-01-02 00:00:01', 'good', 1, 2, 1)"))
 (clails/model/connection:startup-connection-pool))



(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(deftest generate-query-test
  (format t "------------------------------------~%")
  (format t "table-information~%")
  (maphash (lambda (k v)
             (format t "~A => ~A~%" k v))
           clails/model/base-model::*table-information*)
  (format t "------------------------------------~%")
  (let* ((query (query <blog>
                  :as :blog
                  :joins ((:inner-join :account)
                          (:left-join :comments)
                          (:left-join :comment-account :through :comments)
                          (:left-join :approved-account :through :comments))
                  :where (:> (:blog :star) 0)
                  :order-by ((:blog :star :desc)
                            (:blog :id))
                  :limit 20
                  :offset 40))

         (result (clails/model/query::generate-query query))
         (sql (getf result :query))
         (keywords (getf result :keywords)))

    (ok (string-equal "SELECT BLOG.ID as \"BLOG.ID\", BLOG.CREATED_AT as \"BLOG.CREATED_AT\", BLOG.UPDATED_AT as \"BLOG.UPDATED_AT\", BLOG.TITLE as \"BLOG.TITLE\", BLOG.CONTENT as \"BLOG.CONTENT\", BLOG.ACCOUNT_ID as \"BLOG.ACCOUNT_ID\", BLOG.STAR as \"BLOG.STAR\", ACCOUNT.ID as \"ACCOUNT.ID\", ACCOUNT.CREATED_AT as \"ACCOUNT.CREATED_AT\", ACCOUNT.UPDATED_AT as \"ACCOUNT.UPDATED_AT\", ACCOUNT.USERNAME as \"ACCOUNT.USERNAME\", COMMENTS.ID as \"COMMENTS.ID\", COMMENTS.CREATED_AT as \"COMMENTS.CREATED_AT\", COMMENTS.UPDATED_AT as \"COMMENTS.UPDATED_AT\", COMMENTS.COMMENT as \"COMMENTS.COMMENT\", COMMENTS.BLOG_ID as \"COMMENTS.BLOG_ID\", COMMENTS.COMMENT_ID as \"COMMENTS.COMMENT_ID\", COMMENTS.APPROVED_ID as \"COMMENTS.APPROVED_ID\", COMMENT_ACCOUNT.ID as \"COMMENT_ACCOUNT.ID\", COMMENT_ACCOUNT.CREATED_AT as \"COMMENT_ACCOUNT.CREATED_AT\", COMMENT_ACCOUNT.UPDATED_AT as \"COMMENT_ACCOUNT.UPDATED_AT\", COMMENT_ACCOUNT.USERNAME as \"COMMENT_ACCOUNT.USERNAME\", APPROVED_ACCOUNT.ID as \"APPROVED_ACCOUNT.ID\", APPROVED_ACCOUNT.CREATED_AT as \"APPROVED_ACCOUNT.CREATED_AT\", APPROVED_ACCOUNT.UPDATED_AT as \"APPROVED_ACCOUNT.UPDATED_AT\", APPROVED_ACCOUNT.USERNAME as \"APPROVED_ACCOUNT.USERNAME\" FROM blog as BLOG INNER JOIN account as ACCOUNT ON BLOG.ACCOUNT_ID = ACCOUNT.id LEFT JOIN comment as COMMENTS ON BLOG.id = COMMENTS.BLOG_ID LEFT JOIN account as COMMENT_ACCOUNT ON COMMENTS.COMMENT_ID = COMMENT_ACCOUNT.id LEFT JOIN account as APPROVED_ACCOUNT ON COMMENTS.APPROVED_ID = APPROVED_ACCOUNT.id WHERE BLOG.STAR > 0 ORDER BY BLOG.STAR DESC, BLOG.ID LIMIT 20 OFFSET 40"
                       sql))
    (ok (null keywords))))



(deftest generate-query-with-params
  (let* ((query (query <blog>
                       :as :blog
                       :joins ((:inner-join :account)
                               (:inner-join :comments)
                               (:inner-join :approved-account :through :comments))
                       :where (:and (:= (:account :username) :current-user)
                                    (:= (:approved-account :username) :current-user))))

         (result (clails/model/query::generate-query query))
         (sql (getf result :query))
         (keywords (getf result :keywords)))

    (format t "result:~S~%" result)
    (ok (string= "SELECT BLOG.ID as \"BLOG.ID\", BLOG.CREATED_AT as \"BLOG.CREATED_AT\", BLOG.UPDATED_AT as \"BLOG.UPDATED_AT\", BLOG.TITLE as \"BLOG.TITLE\", BLOG.CONTENT as \"BLOG.CONTENT\", BLOG.ACCOUNT_ID as \"BLOG.ACCOUNT_ID\", BLOG.STAR as \"BLOG.STAR\", ACCOUNT.ID as \"ACCOUNT.ID\", ACCOUNT.CREATED_AT as \"ACCOUNT.CREATED_AT\", ACCOUNT.UPDATED_AT as \"ACCOUNT.UPDATED_AT\", ACCOUNT.USERNAME as \"ACCOUNT.USERNAME\", COMMENTS.ID as \"COMMENTS.ID\", COMMENTS.CREATED_AT as \"COMMENTS.CREATED_AT\", COMMENTS.UPDATED_AT as \"COMMENTS.UPDATED_AT\", COMMENTS.COMMENT as \"COMMENTS.COMMENT\", COMMENTS.BLOG_ID as \"COMMENTS.BLOG_ID\", COMMENTS.COMMENT_ID as \"COMMENTS.COMMENT_ID\", COMMENTS.APPROVED_ID as \"COMMENTS.APPROVED_ID\", APPROVED_ACCOUNT.ID as \"APPROVED_ACCOUNT.ID\", APPROVED_ACCOUNT.CREATED_AT as \"APPROVED_ACCOUNT.CREATED_AT\", APPROVED_ACCOUNT.UPDATED_AT as \"APPROVED_ACCOUNT.UPDATED_AT\", APPROVED_ACCOUNT.USERNAME as \"APPROVED_ACCOUNT.USERNAME\" FROM blog as BLOG INNER JOIN account as ACCOUNT ON BLOG.ACCOUNT_ID = ACCOUNT.id INNER JOIN comment as COMMENTS ON BLOG.id = COMMENTS.BLOG_ID INNER JOIN account as APPROVED_ACCOUNT ON COMMENTS.APPROVED_ID = APPROVED_ACCOUNT.id WHERE (ACCOUNT.USERNAME = ? AND APPROVED_ACCOUNT.USERNAME = ?)" sql))
    (ok (equal keywords '(:current-user :current-user)))))

(deftest execute-query-test
  (let* ((query (query <blog>
                       :as :blog
                       :joins ((:inner-join :account)
                               (:left-join :comments)
                               (:left-join :comment-account :through :comments))
                       :where (:= (:account :username) :current-user)
                       :order-by ((:blog :star :desc)
                                 (:blog :id))))
         (result (execute-query query '(:current-user "user1"))))

    (ok (= 2 (length result)))

    ;; 1st record
    (let ((blog (first result)))
      (ok (typep blog '<blog>))
      (ok (= (ref blog :id) 2))
      (ok (string= (ref blog :title) "second blog"))

      (let ((comments (ref blog :comments)))
        (ok (= 0 (length comments)))))

    ;; 2nd record
    (let ((blog (second result)))
      (ok (typep blog '<blog>))
      (ok (= (ref blog :id) 1))
      (ok (string= (ref blog :title) "first blog"))

      (let ((comments (ref blog :comments)))
        (ok (= 2 (length comments)))

        (ok (string= (ref (first comments) :comment) "hi"))
        (ok (string= (ref (second comments) :comment) "good"))))

    ;; ref directly
    (ok (string= (ref-in result 0 :title) "second blog"))
    (ok (string= (ref-in result 1 :title) "first blog"))

    (ok (string= (ref-in result 1 :comments 0 :comment) "hi"))
    (ok (string= (ref-in result 1 :comments 1 :comment) "good"))

    (ok (string= (ref-in result           ; blog[]
                         1                ; blog[1]
                         :comments        ; blog[1].comments[]
                         0                ; blog[1].comments[0]
                         :comment-account ; blog[1].comments[0].comment-account
                         :username)       ; blog[1].comments[0].comment-account.username
                 "user2"))))

