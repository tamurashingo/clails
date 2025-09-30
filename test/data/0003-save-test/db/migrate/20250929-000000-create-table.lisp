(in-package #:clails-test/model/db/save)

(defmigration "20250929-000000-create-table"
 (:up #'(lambda (conn)
          (create-table conn :table "company"
                              :columns '(("name" :type :string
                                                  :not-null T)))
          (create-table conn :table "department"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("company-id" :type :integer
                                                       :not-null T)))
          (create-table conn :table "employee"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("department-id" :type :integer
                                                       :not-null T)
                                        ("employee-number" :type :integer
                                                           :not-null T))))
  :down #'(lambda (conn)
           (drop-table conn :table "employee")
           (drop-table conn :table "department")
           (drop-table conn :table "company"))))
