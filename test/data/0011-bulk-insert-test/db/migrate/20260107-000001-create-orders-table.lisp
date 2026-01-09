(in-package #:clails-test/model/db/bulk-insert)

(defmigration "20260107-000001-create-orders-table"
  (:up #'(lambda (conn)
           (create-table conn :table "orders"
                              :columns '(("customer-id" :type :integer
                                                        :not-null T)
                                         ("product-id" :type :integer
                                                       :not-null T)
                                         ("quantity" :type :integer
                                                     :not-null T
                                                     :default-value 1)
                                         ("total-price" :type :integer
                                                        :not-null T)
                                         ("order-date" :type :string))))
   :down #'(lambda (conn)
             (drop-table conn :table "orders"))))
