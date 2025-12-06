(in-package #:clails-test/model/db/pessimistic-lock)

(defmigration "20251206-000000-create-lockbooks-table"
    (:up #'(lambda (conn)
             (create-table conn :table "pessimistic-lock-books"
                                :columns '(("title" :type :string
                                                    :not-null T)
                                           ("price" :type :integer
                                                    :not-null T))))
     :down #'(lambda (conn)
               (drop-table conn :table "pessimistic-lock-books"))))

