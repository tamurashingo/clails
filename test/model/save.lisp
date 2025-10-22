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
                #:frozen-p
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


(deftest destroy-one-record
  (let* ((new-record (make-record '<employee> :name "to be deleted" :employee-number 99 :department-id 1))
         result)

    (save new-record)

    (ok (execute-query
         (query <employee>
                :as :employee
                :where (:= (:employee :name) :name))
         '(:name "to be deleted"))
        "record exists before `destroy`")

    ;; destroy record
    (ok (= (destroy new-record) 1)
        "`destroy` returns number of destroyed records")
    
    (ok (frozen-p new-record)
        "record is frozen after `destroy`")

    (ok (null (execute-query (query <employee> :as :employee :where (:= (:employee :name) :name))
                             '(:name "to be deleted")))
        "record does not exist after `destroy`")

    ;; destroy frozen record
    (ok (= (destroy new-record) 0)
        "`destroy` returns 0 when trying to destroy frozen record")))

(deftest destroy-multiple-record
  (save (make-record '<employee> :name "to be deleted1" :employee-number 100 :department-id 1))
  (save (make-record '<employee> :name "to be deleted2" :employee-number 101 :department-id 1))
  (save (make-record '<employee> :name "to be deleted3" :employee-number 102 :department-id 1))

  (let* ((query (query <employee>
                       :as :employee
                       :where (:like (:employee :name) :employee-name)))
         (result (execute-query query '(:employee-name "to be deleted%")))
         count
         destroyed-count)

    (ok (= (length result) 3)
        "3 records exist before `destroy`")

    ;; destroy records
    (ok (= (destroy result) 3)
        "`destroy` returns number of destroyed records")))


(deftest destroy-multiple-record-with-frozen
  (let* ((e1 (make-record '<employee> :name "to be deleted4" :employee-number 110 :department-id 1))
         (e2 (make-record '<employee> :name "to be deleted5" :employee-number 111 :department-id 1))
         (e3 (make-record '<employee> :name "to be deleted6" :employee-number 112 :department-id 1))
         count
         destroyed-count)

    (save e1)
    (save e2)
    (save e3)

    (ok (= (length (execute-query
                    (query <employee>
                           :as :employee
                           :where (:like (:employee :name) :employee-name))
                    '(:employee-name "to be deleted%")))
           3)
        "3 records exist before `destroy`")

    ;; freeze e2
    (setf (slot-value e2 'clails/model/base-model::frozen-p) T)

    ;; destroy records
    (ok (= (destroy (list e1 e2 e3)) 2)
        "`destroy` returns number of destroyed records")
    
    (ok (frozen-p e1)
        "update frozen-p of destroyed record")
    (ok (frozen-p e3)
        "update frozen-p of destroyed record")

    (ok (= (length (execute-query
                    (query <employee>
                           :as :employee
                           :where (:like (:employee :name) :employee-name))
                    '(:employee-name "to be deleted%")))
           1)
        "only frozen record remains after `destroy`")))


(deftest destroy-invalid-instance
  (ok (signals
       (let ((invalid-instance (list 1 2 3)))
         (destroy invalid-instance)))
     "type-error is signaled"))

(deftest destroy-one-record-with-cascade
  (let* ((new-company (make-record '<company> :name "company-c"))
         (new-dept1 (make-record '<department> :name "dept1" :company new-company))
         (new-dept2 (make-record '<department> :name "dept2" :company new-company))
         (new-emp1 (make-record '<employee> :name "emp1" :employee-number 200 :department new-dept1))
         (new-emp2 (make-record '<employee> :name "emp2" :employee-number 201 :department new-dept1))
         (new-emp3 (make-record '<employee> :name "emp3" :employee-number 202 :department new-dept2))
         result)

    (save new-company)
    (setf (ref new-dept1 :company-id) (ref new-company :id))
    (setf (ref new-dept2 :company-id) (ref new-company :id))
    (save new-dept1)
    (save new-dept2)
    (setf (ref new-emp1 :department-id) (ref new-dept1 :id))
    (setf (ref new-emp2 :department-id) (ref new-dept1 :id))
    (setf (ref new-emp3 :department-id) (ref new-dept2 :id))
    (save new-emp1)
    (save new-emp2)
    (save new-emp3)
    
    ;; set relations
    (setf (ref new-company :departments) (list new-dept1 new-dept2))
    (setf (ref new-dept1 :employees) (list new-emp1 new-emp2))
    (setf (ref new-dept2 :employees) (list new-emp3))

    
    ;; destroy company with cascade
    (ok (= (destroy new-company :cascade T) 1)
        "`destroy` returns number of destroyed records")
    
    (let ((company-query (query <company>
                                :as :company
                                :where (:= (:company :id) :company-id)))
          (dept-query (query <department>
                             :as :department
                             :where (:= (:department :id) :department-id)))
          (emp-query (query <employee>
                            :as :employee
                            :where (:= (:employee :id) :employee-id))))
      
      (ok (= (length (execute-query company-query `(:company-id ,(ref new-company :id))))
             0)
          "company record does not exist after `destroy`")

      (ok (= (length (execute-query dept-query `(:department-id ,(ref new-dept1 :id))))
             0)
          "department1 record does not exist after `destroy`")

      (ok (= (length (execute-query dept-query `(:department-id ,(ref new-dept2 :id))))
             0)
          "department2 record does not exist after `destroy`")

      (ok (= (length (execute-query emp-query `(:employee-id ,(ref new-emp1 :id))))
             0)
          "employee1 record does not exist after `destroy`")

      (ok (= (length (execute-query emp-query `(:employee-id ,(ref new-emp2 :id))))
             0)
          "employee2 record does not exist after `destroy`")

      (ok (= (length (execute-query emp-query `(:employee-id ,(ref new-emp3 :id))))
             0)
          "employee3 record does not exist after `destroy`"))

    ;; frozen-p is set to T for all destroyed records    
    (ok (frozen-p new-company)
        "company record is frozen after `destroy`")
    (ok (frozen-p new-dept1)
        "department1 record is frozen after `destroy`")
    (ok (frozen-p new-dept2)
        "department2 record is frozen after `destroy`")
    (ok (frozen-p new-emp1)
        "employee1 record is frozen after `destroy`")
    (ok (frozen-p new-emp2)
        "employee2 record is frozen after `destroy`")
    (ok (frozen-p new-emp3)
        "employee3 record is frozen after `destroy`")))


(deftest destroy-many-record-with-cascade
  (let ((company (execute-query (query <company>
                                       :as :company
                                       :joins ((:left-join :departments)
                                               (:left-join :employees :through :departments))
                                       :where (:like (:company :name) "company_%") ; expect: company_a, company_b
                                       :order-by ((:company :id)))
                                '())))
    (ok (= (length company) 2)
        "2 company records exist before `destroy`")
    
    (ok (= (destroy company :cascade T) 2)
        "`destroy` returns number of destroyed records")

    (let ((company-query (query <company>
                                :as :company
                                :where (:= (:company :id) :company-id)))
          (dept-query (query <department>
                             :as :department
                             :where (:= (:department :id) :department-id)))
          (emp-query (query <employee>
                            :as :employee
                            :where (:= (:employee :id) :employee-id))))
      
      (ok (null (execute-query company-query `(:company-id ,(ref-in company 0 :id))))
          "company_a record does not exist after `destroy`")
      (ok (null (execute-query company-query `(:company-id ,(ref-in company 1 :id))))
          "company_b record does not exist after `destroy`")
      
      (ok (null (execute-query dept-query `(:department-id ,(ref-in company 0 :departments 0 :id))))
          "company_a -> sales dept record does not exist after `destroy`")
      (ok (null (execute-query dept-query `(:department-id ,(ref-in company 0 :departments 1 :id))))
          "company_a -> develop dept record does not exist after `destroy`")

      (ok (null (execute-query emp-query `(:employee-id ,(ref-in company 0 :departments 0 :employees 0 :id))))
          "comany_a -> sales dept -> sales1 record does not exist after `destroy`")
      (ok (null (execute-query emp-query `(:employee-id ,(ref-in company 0 :departments 0 :employees 1 :id))))
          "comany_a -> sales dept -> sales2 record does not exist after `destroy`")
      (ok (null (execute-query emp-query `(:employee-id ,(ref-in company 0 :departments 1 :employees 0 :id))))
          "comany_a -> develop dept -> developer1 record does not exist after `destroy`")
      (ok (null (execute-query emp-query `(:employee-id ,(ref-in company 0 :departments 1 :employees 1 :id))))
          "comany_a -> develop dept -> developer2 record does not exist after `destroy`")
      
      
      (ok (frozen-p (ref-in company 0)))
      (ok (frozen-p (ref-in company 1)))
      (ok (frozen-p (ref-in company 0 :departments 0)))
      (ok (frozen-p (ref-in company 0 :departments 1)))
      (ok (frozen-p (ref-in company 0 :departments 0 :employees 0)))
      (ok (frozen-p (ref-in company 0 :departments 0 :employees 1)))
      (ok (frozen-p (ref-in company 0 :departments 1 :employees 0)))
      (ok (frozen-p (ref-in company 0 :departments 1 :employees 1))))))

