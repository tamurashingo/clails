(in-package #:cl-user)
(defpackage #:clails-test/model/save
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:ref-error
                #:ref-in
                #:has-error-p
                #:clear-error
                #:validate))

(defpackage #:clails-test/model/db/save
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))

(in-package #:clails-test/model/save)

(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  (defmodel <company> (<base-model>)
    (:table "company"
    :relations ((:has-many "clails-test/model/save::<department>"
                  :as :departments
                  :foreign-key :company-id))))

  (defmethod validate ((inst <company>))
    (when (or (null (ref inst :name))
              (string= (ref inst :name) ""))
      (setf (ref-error inst :name)
            "name is empty")))

  (defmodel <department> (<base-model>)
    (:table "department"
    :relations ((:belongs-to "clails-test/model/save::<company>"
                  :column :company
                  :key :company-id)
                (:has-many "clails-test/model/save::<employee>"
                  :as :employees
                  :foreign-key :department-id))))

  (defmodel <employee> (<base-model>)
    (:table "employee"
    :relations ((:belongs-to "clails-test/model/save::<department>"
                  :column :department
                  :key :department-id))))

 (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
 (setf clails/environment:*project-environment* :test)
 (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                     :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                     :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                     :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                     :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
 (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0003" "/app/test/data/0003-save-test"))
 (uiop:setup-temporary-directory)
 (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
 (setf clails/environment::*project-dir* uiop:*temporary-directory*)
 (clails/model/migration::db-create)
 (clails/model/migration::db-migrate)
 (clails/model/base-model::initialize-table-information)
 (clails/model/connection::with-db-connection-direct (connection)
   (dbi-cp:do-sql connection "insert into company (created_at, updated_at, name) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'company_a')")
   (dbi-cp:do-sql connection "insert into company (created_at, updated_at, name) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'company_b')")
   (dbi-cp:do-sql connection "insert into department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'sales dept', 1)")
   (dbi-cp:do-sql connection "insert into department (created_at, updated_at, name, company_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'develop dept', 1)")
   (dbi-cp:do-sql connection "insert into employee (created_at, updated_at, name, department_id, employee_number) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'sales1', 1, 0)")
   (dbi-cp:do-sql connection "insert into employee (created_at, updated_at, name, department_id, employee_number) values ('2024-01-02 00:00:01', '2024-01-02 00:00:01', 'sales2', 1, 2)")
   (dbi-cp:do-sql connection "insert into employee (created_at, updated_at, name, department_id, employee_number) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'developer1', 2, 1)")
   (dbi-cp:do-sql connection "insert into employee (created_at, updated_at, name, department_id, employee_number) values ('2024-01-02 00:00:01', '2024-01-02 00:00:01', 'developer2', 2, 3)"))
 (clails/model/connection:startup-connection-pool))


(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(deftest validation-errors
  (testing "new record"
    (let* ((record (make-record '<company>))
           (result (save record)))

      (ok (null result)
          "`save` returns nil when validation error occurs during `save`'")

      (ok (has-error-p record))
      (ok (string= (ref-error record :name)
                   "name is empty"))

      (ok (null (ref record :id))
          "not saved")


      ;; save new record
      (setf (ref record :name) "big company")
      (setf result (save record))

      (ok result
          "`save` returns record instance if save is success")

      (ok (null (has-error-p record)))
      (ok (ref record :id))))

  (testing "update record"
    (let* ((query (query <company>
                         :as :company
                         :where (:= (:company :name) :name)))
           (record (car (execute-query query '(:name "big company"))))
           result)

      (setf (ref record :name) "")
      (setf result (save record))

      (ok (null result))
      (ok (has-error-p record))
      (ok (string= (ref-error record :name)
                   "name is empty"))

      ;; update record
      (setf (ref record :name) "small company")
      (setf result (save record))

      (ok (null (has-error-p record)))

      (setf record (car (execute-query query '(:name "small company"))))

      (ok record))))

