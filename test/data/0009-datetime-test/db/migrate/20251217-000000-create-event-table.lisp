(in-package #:clails-test/model/db)

(defmigration "20251217-000000-create-event-table"
 (:up #'(lambda (conn)
          (create-table conn :table "event"
                             :columns '(("title" :type :string
                                                 :not-null T)
                                        ("start-datetime" :type :datetime)
                                        ("end-datetime" :type :datetime))))
  :down #'(lambda (conn)
           (drop-table conn :table "event"))))
