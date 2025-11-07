(in-package #:clails-test/model/db/migration)

(defmigration "20250101-000000-create-users-table"
 (:up #'(lambda (conn)
          (create-table conn :table "users"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("email" :type :string
                                                 :not-null T))))
  :down #'(lambda (conn)
            (drop-table conn :table "users"))))
