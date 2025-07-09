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
                                                      :user ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf *migration-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test"))
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate *migration-dir*)
  (clails/model/connection::with-db-connection-direct (connection)
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:00', '2024-01-02 13:00:00', 'create program', true, '2024-01-02 13:00:00')")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:01', '2024-01-02 13:00:00', 'create pull request', false, null)")
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, done, done_at) values ('2024-01-01 00:00:02', '2024-01-02 13:00:01', 'merge pr', false, null)"))
  (clails/model/connection:startup-connection-pool))




(teardown
  (clails/model/connection:shutdown-connection-pool))

(deftest test-select-order-by
  (testing "symbol"
    ;; single symbol
    (ok (equal '(("ID" "ASC"))
                (clails/model/query::parse-order-by '(id))))
    ;; single list
    (ok (equal '(("ID" "ASC"))
                (clails/model/query::parse-order-by '((id :ASC)))))
    ;; multiple symbols
    (ok (equal '(("ID" "ASC") ("CREATED_AT" "ASC"))
                (clails/model/query::parse-order-by '(id created-at))))
    ;; multiple list
    (ok (equal '(("ID" "DESC") ("CREATED_AT" "ASC"))
                (clails/model/query::parse-order-by '((id :DESC) (created-at :ASC))))))
  (testing "string"
    ;; single string
    (ok (equal '(("id" "ASC"))
                (clails/model/query::parse-order-by '("id"))))
    ;; single list
    (ok (equal '(("id" "ASC"))
                (clails/model/query::parse-order-by '(("id" :ASC)))))
    ;; multiple symbols
    (ok (equal '(("ID" "ASC") ("CREATED_AT" "ASC"))
                (clails/model/query::parse-order-by '("ID" "CREATED-AT"))))
    ;; multiple list
    (ok (equal '(("id" "DESC") ("created_at" "ASC"))
                (clails/model/query::parse-order-by '(("id" :DESC) ("created-at" :ASC))))))
  (testing "error"
    ;; not symbol, not string
    (ok (signals (clails/model/query::parse-order-by '(1))))
    (ok (signals (clails/model/query::parse-order-by '((1 :ASC)))))
    ;; not keyword :ASC or :DESC
    (ok (signals (clails/model/query::parse-order-by '((id :ASCENDING)))))))


(deftest test-select
  (testing "no condition"
    (let* ((result (select 'clails-test-model::<todo> :order-by '(id)))
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
    (let* ((result (select 'clails-test-model::<todo> :where '(= done 0) :order-by '(id)))
           (1st (first result))
           (2nd (second result)))
      (ok (= 2 (length result)))

      (ok (string= "create pull request" (ref 1st :title)))
      (ok (string= "merge pr" (ref 2nd :title)))))

  (testing "and condition"
    (let* ((result (select 'clails-test-model::<todo> :where '(and (= done 0)
                                                                   (= title "merge pr"))
                                                      :order-by '(id)))
           (1st (first result)))
      (ok (= 1 (length result)))

      (ok (string= "merge pr" (ref 1st :title)))))

  (testing "or condition"
    (let* ((result (select 'clails-test-model::<todo> :where '(or (= done 1)
                                                                   (= title "merge pr"))
                                                      :order-by '(id)))
           (1st (first result))
           (2nd (second result)))
      (ok (= 2 (length result)))

      (ok (string= "create program" (ref 1st :title)))
      (ok (string= "merge pr" (ref 2nd :title)))))

  (testing "other condition"
    (let* ((result (select 'clails-test-model::<todo> :where `(<= created-at "2024-01-01 00:00:00") :order-by '(id)))
           (1st (first result)))
      (ok (= 1 (length result)))

      (ok (string= "create program" (ref 1st :title)))))

  (testing "sort order"
    (let* ((result (select 'clails-test-model::<todo> :order-by '((id :desc))))
           (1st (first result))
           (2nd (second result))
           (3rd (third result)))
      (ok (= 3 (length result)))

      (ok (string= "merge pr" (ref 1st :title)))
      (ok (string= "create pull request" (ref 2nd :title)))
      (ok (string= "create program" (ref 3rd :title))))

    (let* ((result (select 'clails-test-model::<todo> :order-by '((updated-at :desc) created-at)))
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

    (let ((result (select 'clails-test-model::<todo> :where `(= id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "create new project" (ref (first result) :title))))

    ;; debug output
    (clails/model/base-model::show-model-data record)

    ;; change title
    (setf (ref record :title) "create clails project")

    (save record)

    (let ((result (select 'clails-test-model::<todo> :where `(= id ,(ref record :id)))))
      (ok (= (length result)))
      (ok (string= "create clails project" (ref (first result) :title))))

    ;; debug output
    (clails/model/base-model::show-model-data record)))

