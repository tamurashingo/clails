(in-package #:clails-test/model/db)

(defmigration "20240102-000000-add-done-at-to-todo"
  (:up #'(lambda (conn)
           (add-column conn :table "todo"
                            :columns '(("done-at" :type :datetime
                                                  :not-null nil))))
  :down #'(lambda (conn)
            (drop-column conn :table "todo"
                              :column "done-at"))))

