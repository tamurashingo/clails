(in-package #:todoapp-db)

(defmigration "todo"
  (:up #'(lambda (conn)
           (create-table conn :table "todo"
                              :columns '(("title" :type :string
                                                  :not-null T)
                                         ("done" :type :boolean
                                                 :not-null T
                                                 :default 0)
                                         ("done_at" :type :datetime))))
   :down #'(lambda (conn)
             (drop-table conn :table "todo"))))
