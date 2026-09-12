(in-package #:cl-user)
(defpackage #:clails-test/model/eager-loading
  (:use #:cl
        #:rove
        #:clails/model/query
        #:clails/model/query/crud)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))

(defpackage #:clails-test/model/db/eager-loading
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:add-column
                #:add-index
                #:drop-table
                #:drop-column
                #:drop-index))
(in-package #:clails-test/model/eager-loading)

;;;; ----------------------------------------
;;;; Test instrumentation: count how many times execute-query actually issues
;;;; a query for a <query> instance (i.e. one DB round-trip). This is used to
;;;; prove :includes really collapses N+1 queries into a small constant
;;;; number, rather than merely asserting the returned data looks right.

(defvar *query-exec-count* 0
  "Number of times execute-query has run against a <query> instance since it
   was last reset. Incremented by the :around method below.")

(defmethod execute-query :around ((query clails/model/query:<query>) named-values &rest args &key &allow-other-keys)
  (declare (ignore named-values))
  (incf *query-exec-count*)
  (call-next-method))

(defun reset-query-count ()
  (setf *query-exec-count* 0))


(setup
  ;; clear table-information
  (clrhash clails/model/base-model::*table-information*)
  ;; define models
  ;; Table names are prefixed with eager_ to avoid colliding with the
  ;; "company"/"department"/"office" tables used by other test fixtures that
  ;; share the same test database (e.g. clails-test/model/save).
  (defmodel <company> (<base-model>)
    (:table "eager_company"
    :relations ((:has-many "clails-test/model/eager-loading::<department>"
                  :as :departments
                  :foreign-key :company-id)
                (:has-many "clails-test/model/eager-loading::<office>"
                  :as :offices
                  :foreign-key :company-id))))

  (defmodel <department> (<base-model>)
    (:table "eager_department"
    :relations ((:belongs-to "clails-test/model/eager-loading::<company>"
                  :column :company
                  :key :company-id))))

  (defmodel <office> (<base-model>)
    (:table "eager_office"
    :relations ((:belongs-to "clails-test/model/eager-loading::<company>"
                  :column :company
                  :key :company-id))))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0014" "/app/test/data/0014-eager-loading-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/base-model:initialize-table-information)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; company 1: Acme -- 2 departments, 1 office
    (dbi-cp:do-sql connection "insert into eager_company (created_at, updated_at, name) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Acme')")
    ;; company 2: Globex -- 0 departments, 2 offices
    (dbi-cp:do-sql connection "insert into eager_company (created_at, updated_at, name) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Globex')")
    ;; company 3: Initech -- 1 department, 0 offices
    (dbi-cp:do-sql connection "insert into eager_company (created_at, updated_at, name) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Initech')")
    ;; company 4: Umbrella -- 3 departments, 0 offices
    (dbi-cp:do-sql connection "insert into eager_company (created_at, updated_at, name) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Umbrella')")

    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Engineering', 1)")
    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Sales', 1)")
    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Support', 3)")
    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'R&D', 4)")
    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'QA', 4)")
    (dbi-cp:do-sql connection "insert into eager_department (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Ops', 4)")

    (dbi-cp:do-sql connection "insert into eager_office (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'HQ', 1)")
    (dbi-cp:do-sql connection "insert into eager_office (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Branch1', 2)")
    (dbi-cp:do-sql connection "insert into eager_office (created_at, updated_at, name, company_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Branch2', 2)"))
  (clails/model/connection:startup-connection-pool))

(teardown
 (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
 (clails/model/connection:shutdown-connection-pool))


(defun department-names (company)
  (sort (mapcar #'(lambda (d) (ref d :name)) (ref company :departments)) #'string<))

(defun office-names (company)
  (sort (mapcar #'(lambda (o) (ref o :name)) (ref company :offices)) #'string<))

(defun lazily-fetch-department-names (company-id)
  "Reference implementation: fetch a company's departments the way a caller
   would have to before :includes existed -- one query per parent."
  (let ((q (query-builder '<department> :as :department)))
    (set-where q '(:= (:department :company-id) :company-id))
    (sort (mapcar #'(lambda (d) (ref d :name))
                  (execute-query q (list :company-id company-id)))
          #'string<)))


(deftest test-includes-has-many-reduces-query-count
  (testing "loading N companies with :includes (:departments) issues a constant number of queries, not one per company"
    (reset-query-count)
    (let* ((q (query <company> :as :company :includes (:departments)))
           (results (execute-query q nil)))
      ;; 4 parent companies loaded
      (ok (= 4 (length results)))
      ;; exactly 2 queries total: 1 for companies, 1 batched query for all departments
      ;; -- NOT 1 (companies) + 4 (one per company), which is what naive N+1 code would issue.
      (ok (= 2 *query-exec-count*)))))

(deftest test-includes-has-many-correctness
  (testing "eager-loaded has-many data matches what per-company (lazy) fetching would return"
    (reset-query-count)
    (let* ((q (query <company> :as :company :includes (:departments) :order-by ((:company :id))))
           (results (execute-query q nil)))
      (let ((acme (first results))
            (globex (second results))
            (initech (third results))
            (umbrella (fourth results)))
        (ok (string= "Acme" (ref acme :name)))
        (ok (equal '("Engineering" "Sales") (department-names acme)))
        (ok (equal (lazily-fetch-department-names (ref acme :id)) (department-names acme)))

        (ok (string= "Globex" (ref globex :name)))
        (ok (null (ref globex :departments)))
        (ok (equal (lazily-fetch-department-names (ref globex :id)) (department-names globex)))

        (ok (string= "Initech" (ref initech :name)))
        (ok (equal '("Support") (department-names initech)))
        (ok (equal (lazily-fetch-department-names (ref initech :id)) (department-names initech)))

        (ok (string= "Umbrella" (ref umbrella :name)))
        (ok (equal '("Ops" "QA" "R&D") (department-names umbrella)))
        (ok (equal (lazily-fetch-department-names (ref umbrella :id)) (department-names umbrella)))))))

(deftest test-includes-belongs-to-reduces-query-count-and-is-correct
  (testing ":includes on a :belongs-to relation is also batched into one query"
    (reset-query-count)
    (let* ((q (query <department> :as :department :includes (:company) :order-by ((:department :id))))
           (results (execute-query q nil)))
      (ok (= 6 (length results)))
      ;; 1 query for departments + 1 batched query for all their companies
      (ok (= 2 *query-exec-count*))

      (ok (string= "Engineering" (ref (first results) :name)))
      (ok (string= "Acme" (ref (ref (first results) :company) :name)))

      (ok (string= "Support" (ref (third results) :name)))
      (ok (string= "Initech" (ref (ref (third results) :company) :name))))))

(deftest test-includes-multiple-relations-one-query-each
  (testing ":includes with several relations issues one batched query per relation"
    (reset-query-count)
    (let* ((q (query <company> :as :company :includes (:departments :offices) :order-by ((:company :id))))
           (results (execute-query q nil)))
      (ok (= 4 (length results)))
      ;; 1 (companies) + 1 (departments) + 1 (offices) = 3, never one query per company/relation pair
      (ok (= 3 *query-exec-count*))

      (let ((acme (first results))
            (globex (second results)))
        (ok (equal '("Engineering" "Sales") (department-names acme)))
        (ok (equal '("HQ") (office-names acme)))

        (ok (null (ref globex :departments)))
        (ok (equal '("Branch1" "Branch2") (office-names globex)))))))

(deftest test-includes-skips-batch-query-when-no-parents-match
  (testing "no wasted second query when the primary query returns zero rows"
    (reset-query-count)
    (let* ((q (query <company>
                     :as :company
                     :includes (:departments)
                     :where (:= (:company :name) :name)))
           (results (execute-query q '(:name "does-not-exist"))))
      (ok (= 0 (length results)))
      ;; only the (empty) primary query ran -- the batched department query was skipped entirely
      (ok (= 1 *query-exec-count*)))))

(deftest test-includes-unknown-relation-signals-error
  (testing ":includes referencing an undeclared relation is an error"
    (let ((q (query-builder '<company> :as :company)))
      (set-includes q '(:no-such-relation))
      (ok (signals (execute-query q nil) 'error)))))
