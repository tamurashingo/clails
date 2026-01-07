(in-package #:clails-test/model/db/bulk-insert)

(defmigration "20260107-000000-create-products-table"
 (:up #'(lambda (conn)
          (create-table conn :table "products"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("price" :type :integer
                                                 :not-null T)
                                        ("stock" :type :integer
                                                 :default-value 0)
                                        ("description" :type :string)))
          (create-table conn :table "customers"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("email" :type :string
                                                 :not-null T)
                                        ("phone" :type :string))))
  :down #'(lambda (conn)
            (drop-table conn :table "products")
            (drop-table conn :table "customers"))))
