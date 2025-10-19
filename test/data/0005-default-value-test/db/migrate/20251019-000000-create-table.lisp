(in-package #:clails-test/model/db/default-value)

(defmigration "20251019-000000-create-table"
 (:up #'(lambda (conn)
          (create-table conn :table "product"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("status" :type :string
                                                  :default-value "active")
                                        ("priority" :type :integer
                                                    :default-value 0)
                                        ("is-published" :type :boolean
                                                        :default-value T))))
  :down #'(lambda (conn)
           (drop-table conn :table "product"))))
