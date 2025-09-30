(in-package #:cl-user)
(defpackage #:clails-test/model/query/sqlite3
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
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

(in-package #:clails-test/model/query/sqlite3)

(defparameter *insert-query-sqlite3*
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
    " current_timestamp, "
    " current_timestamp, "
    " 'string'," ; :type :string
    " 'text', " ; :type :text
    " 1, " ; :type :integer
    " 3.1415," ; :type :float
    " 3.14, " ; :type :decimal
    " '2024-10-01 12:34:45', " ; :type :datetime
    " '2024-10-01', " ; :type :date
    " '12:34:45', " ; :type :time
    " 1 " ; :type :boolean
    ")"))


(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <debug> (<base-model>)
    (:table "debug"))

   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-sqlite3>))
   (setf clails/environment:*project-environment* :test)
   (setf clails/environment:*database-config* `(:test (:database-name ,(format NIL "~A/volumes/clails_test.sqlite3" (env-or-default "CLAILS_SQLITE3_DATABASE" "/app")))))
   (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0001" "/app/test/data/0001-migration-test"))
   (uiop:setup-temporary-directory)
   (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
   (setf clails/environment::*project-dir* uiop:*temporary-directory*)
   (let ((db-file (getf (getf clails/environment:*database-config* :test) :database-name)))
    (when (probe-file db-file)
      (delete-file db-file)))
   (clails/model/migration::db-create)
   (clails/model/migration::db-migrate)
   (clails/model/connection::with-db-connection-direct (connection)
     (dbi:do-sql connection *insert-query-sqlite3*)
     (dbi:commit connection))
   (clails/model/connection:startup-connection-pool)
   (clails/model/base-model:initialize-table-information))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))


(deftest sqlite3-column-type-check
  (let* ((query (query <debug>
                       :as :debug))
         (result (car (execute-query query '()))))
    (ok (string= "string" (ref result :col-1)))
    (ok (string= "text" (ref result :col-2)))
    (ok (eq 1 (ref result :col-3)))
    (ok (= 3.1415d0 (ref result :col-4))) ; double-float
    (ok (= 3.14d0 (ref result :col-5)))
    (ok (= (encode-universal-time 45 34 12 1 10 2024) (ref result :col-6)))
    (ok (= (encode-universal-time 0 0 0 1 10 2024) (ref result :col-7)))
    (ok (= (+ (* 12 60 60) (* 34 60) 45) (ref result :col-8)))
    (ok (ref result :col-9))))


(deftest sqlite3-insert-check
  (let ((record (make-record '<debug> :col-1 "new sqlite3 record")))

    (ok (null (ref record :id)))
    (ok (null (ref record :created-at)))
    (ok (null (ref record :updated-at)))

    (save record)

    (ok (not (null (ref record :id))))
    (ok (not (null (ref record :created-at))))
    (ok (not (null (ref record :updated-at))))

    ; check inserted record
    (let* ((query (query <debug>
                         :as :debug
                         :where (:= (:debug :id) :target-id)))
           (result (execute-query query `(:target-id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "new sqlite3 record" (ref (first result) :col-1))))

    ;; change content
    (setf (ref record :col-1) "updated sqlite3 record")

    (save record)

    ;; check updated record
    (let* ((query (query <debug>
                         :as :debug
                         :where (:= (:debug :id) :target-id)))
           (result (execute-query query `(:target-id ,(ref record :id)))))
      (ok (= (length result) 1))
      (ok (string= "updated sqlite3 record" (ref (first result) :col-1))))))


(deftest sqlite3-null-check
    (let ((record (make-record '<debug>)))
      (save record)

      (let* ((query (query <debug>
                           :as :debug
                           :where (:= (:debug :id) :target-id)))
             (result (execute-query query `(:target-id ,(ref record :id)))))
        (ok (= (length result) 1))
        (defvar r (car result))

        (ok (null (ref r :col-1)))
        (ok (null (ref r :col-2)))
        (ok (null (ref r :col-3)))
        (ok (null (ref r :col-4)))
        (ok (null (ref r :col-5)))
        (ok (null (ref r :col-6)))
        (ok (null (ref r :col-7)))
        (ok (null (ref r :col-8)))
        (ok (null (ref r :col-9))))))

