(in-package #:cl-user)
(defpackage #:clails-test/model/query
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
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

(defpackage #:clails-test-model
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:defmodel
                #:ref
                #:<base-model>))
(in-package #:clails-test-model)
(defmodel <todo> (<base-model>)
  (:table "todo"))

(in-package #:clails-test/model/query)


(setup
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test/migration-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:00', '2024-01-02 13:00:00', 'create program', true, '2024-01-02 13:00:00')")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:01', '2024-01-02 13:00:00', 'create pull request', false, null)")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:02', '2024-01-02 13:00:01', 'merge pr', false, null)"))
  (clails/model/connection:startup-connection-pool)
  (clails/model/base-model:initialize-table-information))





(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))


(deftest test-select
  (testing "no condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result))
           (3rd (third result)))
      (ok (= 3 (length result)))

      (ok (string= "create program" (ref 1st :title)))
      (ok (= (encode-universal-time 0 0  0 1 1 2024) (ref 1st :created-at)))
      (ok (= (encode-universal-time 0 0 13 2 1 2024) (ref 1st :updated-at)))
      (ok (eq T (ref 1st :done)))
      (ok (= (encode-universal-time 0 0 13 2 1 2024) (ref 1st :done-at)))

      (ok (string= "create pull request" (ref 2nd :title)))
      (ok (= (encode-universal-time 1 0  0 1 1 2024) (ref 2nd :created-at)))
      (ok (= (encode-universal-time 0 0 13 2 1 2024) (ref 2nd :updated-at)))
      (ok (null (ref 2nd :done)))
      (ok (null (ref 2nd :done-at)))

      (ok (string= "merge pr" (ref 3rd :title)))
      (ok (= (encode-universal-time 2 0  0 1 1 2024) (ref 3rd :created-at)))
      (ok (= (encode-universal-time 1 0 13 2 1 2024) (ref 3rd :updated-at)))
      (ok (null (ref 3rd :done)))
      (ok (null (ref 3rd :done-at)))))

  (testing "single condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:= (:todo :done) 0)
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result)))
      (ok (= 2 (length result)))

      (ok (string= "create pull request" (ref 1st :title)))
      (ok (string= "merge pr" (ref 2nd :title)))))

  (testing "is null condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:null (:todo :done-at))
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result)))
      (ok (= 2 (length result)))

      (ok (string= "create pull request" (ref 1st :title)))
      (ok (string= "merge pr" (ref 2nd :title)))))


  (testing "not null condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:not-null (:todo :done-at))
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result)))
      (ok (= 1 (length result)))

      (ok (string= "create program" (ref 1st :title)))))

  (testing "and condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:and (:= (:todo :done) 0)
                                      (:= (:todo :title) "merge pr"))
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result)))
      (ok (= 1 (length result)))

      (ok (string= "merge pr" (ref 1st :title)))))

  (testing "or condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:or (:= (:todo :done) 1)
                                     (:= (:todo :title) "merge pr"))
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result)))
      (ok (= 2 (length result)))

      (ok (string= "create program" (ref 1st :title)))
      (ok (string= "merge pr" (ref 2nd :title)))))

  (testing "other condition"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:<= (:todo :created-at) "2024-01-01 00:00:00")
                         :order-by ((:todo :id))))
           (result (execute-query query '()))
           (1st (first result)))
      (ok (= 1 (length result)))

      (ok (string= "create program" (ref 1st :title)))))

  (testing "sort order"
    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :order-by ((:todo :id :desc))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result))
           (3rd (third result)))
      (ok (= 3 (length result)))

      (ok (string= "merge pr" (ref 1st :title)))
      (ok (string= "create pull request" (ref 2nd :title)))
      (ok (string= "create program" (ref 3rd :title))))

    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :order-by ((:todo :updated-at :desc)
                                    (:todo :created-at))))
           (result (execute-query query '()))
           (1st (first result))
           (2nd (second result))
           (3rd (third result)))
      (ok (= 3 (length result)))

      (ok (string= "merge pr" (ref 1st :title)))
      (ok (string= "create program" (ref 2nd :title)))
      (ok (string= "create pull request" (ref 3rd :title))))))

(deftest save
  (let ((record (make-record 'clails-test-model::<todo> :title "create new project")))

    ;; make sure id is set
    (ok (null (ref record :id)))
    (ok (null (ref record :created-at)))
    (ok (null (ref record :updated-at)))
    (save record)
    (ok (not (null (ref record :id))))
    (ok (not (null (ref record :created-at))))
    (ok (not (null (ref record :updated-at))))

    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:= (:todo :id) :target-id)))
           (result (execute-query query `(:target-id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "create new project" (ref (first result) :title))))

    ;; debug output
    (clails/model/base-model::show-model-data record)

    ;; change title
    (setf (ref record :title) "create clails project")

    (save record)

    (let* ((query (query clails-test-model::<todo>
                         :as :todo
                         :where (:= (:todo :id) :target-id)))
           (result (execute-query query `(:target-id ,(ref record :id)))))
      (ok (= (length result)))
      (ok (string= "create clails project" (ref (first result) :title))))

    ;; debug output
    (clails/model/base-model::show-model-data record)))

