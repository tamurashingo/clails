(in-package #:cl-user)
(defpackage #:clails-test/model/query/postgresql
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

(in-package #:clails-test/model/query/postgresql)

(defparameter *insert-query-postgresql*
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
  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                                                       :user ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                                                       :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                                                       :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                                                       :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (setf *migration-dir* (env-or-default "CLAILS_MIGRATION_DIR" "/app/test"))
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate *migration-dir*)
  (clails/model/connection::with-db-connection-direct (connection)
    (dbi-cp:do-sql connection *insert-query-postgresql*))
  (clails/model/connection:startup-connection-pool))

(teardown
  (clails/model/connection:shutdown-connection-pool))


(deftest postgresql-column-type-check
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
