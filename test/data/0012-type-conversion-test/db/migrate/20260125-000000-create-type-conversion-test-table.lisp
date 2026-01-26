(in-package #:clails-test/model/type-conversion/db)

(defmigration "20260125-000000-create-type-conversion-test-table"
 (:up #'(lambda (conn)
         (create-table conn :table "type_conversion_test"
                            :columns '(("title" :type :string
                                                :not-null T)
                                       ("is_active" :type :boolean
                                                    :default-value NIL)
                                       ("priority" :type :integer
                                                   :default-value 0)
                                       ("score" :type :float
                                                :default-value 0.0)
                                       ("completed_at" :type :datetime)))
         (add-index conn :table "type_conversion_test"
                         :index "idx-is_active"
                         :columns '("is_active")))
 :down #'(lambda (conn)
           (drop-table conn :table "type_conversion_test"))))
