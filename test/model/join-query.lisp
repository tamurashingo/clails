(in-package #:cl-user)
(defpackage #:clails-test/model/join-query
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:ref))

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
   :has-one (("<blog>" :as "blog"
                       :foreign-key :account-id))))

(defmodel <blog> (<base-model>)
  (:table "blog"
   :belongs-to (("<account>" :as "account"
                             :key :account-id))
   :has-many (("<comment>" :as "comments"
                           :foreign-key :blog-id))))

(defmodel <comment> (<base-model>)
  (:table "comment"
   :belongs-to (("<blog>" :as "blog"
                          :key :blog-id))))

(in-package #:clails-test/model/join-query)


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
 (clails/model/connection::with-db-connection-direct (connection)
   (dbi-cp:do-sql connection "insert into account (created_at, updated_at, username) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'user1')")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'first blog', 'this is my first blog', 1, 0)")
   (dbi-cp:do-sql connection "insert into blog (created_at, updated_at, title, content, account_id, star) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'second blog', 'this is my second blog', 1, 10)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'hi', 1)")
   (dbi-cp:do-sql connection "insert into comment (created_at, updated_at, comment, blog_id) values ('2024-01-02 00:00:01', '2024-01-02 00:00:01', 'good', 1)"))
 (clails/model/connection:startup-connection-pool))



(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(deftest join-query-test
  (clails/model/base-model:initialize-table-information)

  (defvar *blog*
         (clails/model/query::query clails-test-model-joinquery::<blog>
            :as blog
            :joins ((:inner-join clails-test-model-joinquery::<account>
                     :as account
                     :on (= (account :id)
                            (blog :account-id)))
                    (:left-join clails-test-model-joinquery::<comment>
                     :as comment
                     :on (= (blog :id)
                            (comment :blog-id))))
            :where (> (blog :star) 0)
            :order-by ((blog :star :desc)
                       (blog :id))
            :offset 40
            :limit 20))
  (format t "query1:~A~%" (clails/model/query::generate-query *blog*))


  (defvar *blog2*
          (clails/model/query::query clails-test-model-joinquery::<blog>
           :joins ((:inner-join clails-test-model-joinquery::<account>
                    :on (:and (= (account :id)
                                 (blog :account-id))
                              (:like (account :username) :login-user))))
           :where (> (blog :star) :hottest)
           :limit :page1
           :offset :page2))

  (format t "qury2:~S~%" (clails/model/query::generate-query *blog2*))


  (defvar *query*
    (clails/model/query::query clails-test-model-joinquery::<blog>
       :as blog
       :joins ((:inner-join clails-test-model-joinquery::<account>
                :as account
                :on (:and (= (account :id)
                             (blog :account-id))))
               (:left-join clails-test-model-joinquery::<comment>
                :as comment
                :on (= (blog :id)
                       (comment :blog-id))))
       :order-by ((blog :id :desc))))

  (format t "result raw:~S~%"
          (clails/model/query::execute-query *query* '()))


  (maphash #'(lambda (k v)
               (format t "key:~A, val:~S~%" k v))
           (clails/model/query::collect-result *query*
                                               (clails/model/query::execute-query *query* '())))




)
