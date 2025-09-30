(in-package #:clails-test/model/db)

(defmigration "20240103-000000-create-debug-table"
 (:up #'(lambda (conn)
            ;; Create a table using all types
          (create-table conn :table "debug"
                             :columns '(("col-1" :type :string)
                                        ("col-2" :type :text)
                                        ("col-3" :type :integer)
                                        ("col-4" :type :float :precision 16 :scale 4)
                                        ("col-5" :type :decimal :precision 10 :scale 2)
                                        ("col-6" :type :datetime)
                                        ("col-7" :type :date)
                                        ("col-8" :type :time)
                                        ("col-9" :type :boolean))))
  :down #'(lambda (conn)
            (drop-table conn :table "debug"))))

