(in-package #:cl-user)
(defpackage #:clails-test/model/impl/postgresql
  (:use #:cl
        #:rove
        #:clails/model/impl/postgresql))
(in-package #:clails-test/model/impl/postgresql)

(setup
   (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
   (setf clails/environment:*database-config* '(:database "clails_test"
                                                :user "clails"
                                                :password "password"
                                                :host "host.docker.internal"
                                                :port "5432")))


(deftest create-database
  (clails/model/migration::db-create)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check database exists
    (let* ((query (dbi:prepare connection "select datname from pg_database where datname = ?"))
           (result (dbi:execute query (list "clails_test"))))
      (ok (string= "clails_test" (getf (dbi:fetch result) :|datname|))))
    ;; check migration table exists
    (let* ((query (dbi:prepare connection "select tablename from pg_tables where tablename = ?"))
           (result (dbi:execute query (list "migration"))))
      (ok (string= "migration" (getf (dbi:fetch result) :|tablename|))))))

(deftest migration
  (clails/model/migration::db-migrate "/app/test/")
  (clails/model/connection::with-db-connection-direct (connection)
    ;; check schema
    (let* ((result (dbi:fetch-all (dbi:execute (dbi:prepare connection
      "select column_name, data_type, is_nullable, column_default from information_schema.columns where table_name = ? order by ordinal_position")
                                               (list "todo"))))
           (id (first result))
           (created-at (second result))
           (updated-at (third result))
           (title (fourth result))
           (done (fifth result))
           (done-at (sixth result)))
      ;; id
      (ok (string= "id" (getf id :|column_name|)))
      (ok (string= "integer" (getf id :|data_type|)))
      (ok (string= "NO" (getf id :|is_nullable|)))
      (ok (string= "nextval('todo_id_seq'::regclass)" (getf id :|column_default|)))
      ;; created_at
      (ok (string= "created_at" (getf created-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf created-at :|data_type|)))
      (ok (string= "NO" (getf created-at :|is_nullable|)))
      (ok (eq :NULL (getf created-at :|column_default|)))
      ;; updated_at
      (ok (string= "updated_at" (getf updated-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf updated-at :|data_type|)))
      (ok (string= "NO" (getf updated-at :|is_nullable|)))
      (ok (eq :NULL (getf updated-at :|column_default|)))
      ;; title
      (ok (string= "title" (getf title :|column_name|)))
      (ok (string= "character varying" (getf title :|data_type|)))
      (ok (string= "NO" (getf title :|is_nullable|)))
      (ok (eq :NULL (getf title :|column_default|)))
      ;; done
      (ok (string= "done" (getf done :|column_name|)))
      (ok (string= "boolean" (getf done :|data_type|)))
      (ok (string= "YES" (getf done :|is_nullable|)))
      (ok (eq :NULL (getf done :|column_default|)))
      ;; done_at
      (ok (string= "done_at" (getf done-at :|column_name|)))
      (ok (string= "timestamp without time zone" (getf done-at :|data_type|)))
      (ok (string= "YES" (getf done-at :|is_nullable|)))
      (ok (eq :NULL (getf done-at :|column_default|))))))



                                                                                                            



