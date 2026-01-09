(in-package #:clails-test/model/db/bulk)

(defmigration "20260106-000000-create-users-table"
 (:up #'(lambda (conn)
          (create-table conn :table "users2"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("age" :type :integer
                                               :default-value 0)
                                        ("status" :type :string
                                                  :default-value "inactive"))))
  :down #'(lambda (conn)
            (drop-table conn :table "users2"))))
