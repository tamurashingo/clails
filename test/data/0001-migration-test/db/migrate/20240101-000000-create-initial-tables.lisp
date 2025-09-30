(in-package #:clails-test/model/db)

(defmigration "20240101-000000-create-initial-tables"
 (:up #'(lambda (conn)
          (create-table conn :table "todo"
                             :columns '(("title" :type :string
                                                 :not-null T)
                                        ("done" :type :boolean
                                                :default-value NIL)))
          (add-index conn :table "todo"
                          :index "idx-title"
                          :columns '("title")))
  :down #'(lambda (conn)
            (drop-table conn :table "todo"))))

