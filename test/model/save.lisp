(in-package #:cl-user)
(defpackage #:clails-test/model/save
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:ref
                #:ref-error
                #:ref-in
                #:has-error-p
                #:clear-error
                #:validate))

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

(defpackage #:clails-test-model-joinquery
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:ref
                #:<base-model>))
(in-package #:clails-test-model-joinquery)
(defmodel <account> (<base-model>)
  (:table "account"
   :relations ((:has-many "clails-test-model-joinquery::<blog>"
                :as :blogs
                :foreign-key :account-id)
               (:has-many "clails-test-model-joinquery::<comment>"
                :as :comments
                :foreign-key :comment-id)
               (:has-many "clails-test-model-joinquery::<comment>"
                :as :approved-comments
                :foreign-key :approved-id))))

(defmodel <blog> (<base-model>)
  (:table "blog"
   :relations ((:belongs-to "clails-test-model-joinquery::<account>"
                :column :account
                :key :account-id)
               (:has-many "clails-test-model-joinquery::<comment>"
                :as :comments
                :foreign-key :blog-id))))

(defmodel <comment> (<base-model>)
  (:table "comment"
   :relations ((:belongs-to "clails-test-model-joinquery::<blog>"
                :column :blog
                :key :blog-id)
               (:belongs-to "clails-test-model-joinquery::<account>"
                :column :comment-account
                :key :comment-id)
               (:belongs-to "clails-test-model-joinquery::<account>"
                :column :approved-account
                :key :approved-id))))

(in-package #:clails-test/model/save)

(setup
 (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
 (setf clails/environment:*project-environment* :test)
 (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                     :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                     :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                     :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                     :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
 (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_JOIN__DIR" "/app/test/join-test"))
 (uiop:setup-temporary-directory)
 (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
 (setf clails/environment::*project-dir* uiop:*temporary-directory*)
 (clails/model/migration::db-create)
 (clails/model/migration::db-migrate)
 (clails/model/base-model::initialize-table-information)
 (clails/model/connection::with-db-connection-direct (connection)
   (dbi-cp:do-sql connection "insert into account (created_at, updated_at, username) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'user1')")
   (dbi-cp:do-sql connection "insert into account (created_at, updated_at, username) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'user2')")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'first blog', 'this is my first blog', 1, 0)")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'second blog', 'this is my second blog', 1, 10)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id, comment_id, approved_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'hi', 1, 2, 1)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id, comment_id, approved_id) values ('2024-01-02 00:00:01', '2024-01-02 00:00:01', 'good', 1, 2, 1)"))
 (clails/model/connection:startup-connection-pool))


(teardown
 (clails/model/connection::with-db-connection-direct (connection)
   (dbi-cp:do-sql connection "truncate table comment")
   (dbi-cp:do-sql connection "truncate table blog")
   (dbi-cp:do-sql connection "truncate table account"))
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(defmethod validate ((inst clails-test-model-joinquery::<blog>))
  (when (or (null (ref inst :title))
            (string= (ref inst :title) ""))
    (setf (ref-error inst :title)
          "title is empty")))


(deftest validation-errors
  (testing "new record"
    (let* ((record (make-record 'clails-test-model-joinquery::<blog>))
           (result (save record)))

      (ok (null result)
          "`save` returns nil when validation error occurs during `save`'")

      (ok (has-error-p record))
      (ok (string= (ref-error record :title)
                   "title is empty"))

      (ok (null (ref record :id))
          "not saved")


      ;; save new record
      (setf (ref record :title) "this is a pen")
      (setf result (save record))

      (ok result
          "`save` returns record instance if save is success")

      (ok (null (has-error-p record)))
      (ok (ref record :id))))

  (testing "update record"
    (let* ((query (query clails-test-model-joinquery::<blog>
                         :as :blog
                         :where (:= (:blog :title) :title)))
           (record (car (execute-query query '(:title "this is a pen"))))
           result)

      (setf (ref record :title) "")
      (setf result (save record))

      (ok (null result))
      (ok (has-error-p record))
      (ok (string= (ref-error record :title)
                   "title is empty"))

      ;; update record
      (setf (ref record :title) "this is new blog")
      (setf result (save record))

      (ok (null (has-error-p record)))

      (setf record (car (execute-query query '(:title "this is new blog"))))

      (ok record))))

