(in-package #:cl-user)
(defpackage #:clails-test/model/query/mysql
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
(defmodel <debug> (<base-model>)
  (:table "debug"))

(in-package #:clails-test/model/query/mysql)

(defparameter *insert-query-mysql*
  (concatenate 'string
    " insert into debug ("
    " created_at, "
    " updated_at, "
    " col_1, " ; :type :string
    " col_2, " ; :type :text
    " col_3, " ; :type :integer
    " col_4, " ; :type :float
    " col_5, " ; :type :decimal
    " col_6, " ; :type :datetime
    " col_7, " ; :type :date
    " col_8, " ; :type :time
    " col_9 " ; :type :boolean
    ") values ("
    " now(), "
    " now(), "
    " 'string'," ; :type :string
    " 'text', " ; :type :text
    " 1, " ; :type :integer
    " 3.1415," ; :type :float
    " 3.14, " ; :type :decimal
    " '2024-10-01 12:34:45', " ; :type :datetime
    " '2024-10-01', " ; :type :date
    " '12:34:45', " ; :type :time
    " true " ; :type :boolean
    ")"))



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
    (dbi:do-sql connection *insert-query-mysql*)
    (dbi:commit connection))
  (clails/model/connection:startup-connection-pool))

(teardown
  (clails/model/connection:shutdown-connection-pool))


(deftest mysql-column-type-check
  (let ((result (car (select 'clails-test-model::<debug>))))
    (ok (string= "string" (ref result :col-1)))
    (ok (string= "text" (ref result :col-2)))
    (ok (eq 1 (ref result :col-3)))
    (ok (= 3.1415d0 (ref result :col-4))) ; double-float
    (ok (= 3.14d0 (ref result :col-5)))
    (ok (= (encode-universal-time 45 34 12 1 10 2024) (ref result :col-6)))
    (ok (= (encode-universal-time 0 0 0 1 10 2024) (ref result :col-7)))
    (ok (= (+ (* 12 60 60) (* 34 60) 45) (ref result :col-8)))
    (ok (ref result :col-9))))


(deftest mysql-insert-check
  (let ((record (make-record 'clails-test-model::<debug> :col-1 "new mysql record")))

    (ok (null (ref record :id)))
    (ok (null (ref record :created-at)))
    (ok (null (ref record :updated-at)))

    (save record)

    (ok (not (null (ref record :id))))
    (ok (not (null (ref record :created-at))))
    (ok (not (null (ref record :updated-at))))

    ; check inserted record
    (let ((result (select 'clails-test-model::<debug> :where `(= id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "new mysql record" (ref (first result) :col-1))))

    ;; change content
    (setf (ref record :col-1) "updated mysql record")

    (save record)

    ;; check updated record
    (let ((result (select 'clails-test-model::<debug> :where `(= id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "updated mysql record" (ref (first result) :col-1))))))
