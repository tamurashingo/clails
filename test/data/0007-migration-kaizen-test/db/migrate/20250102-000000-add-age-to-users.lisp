(in-package #:clails-test/model/db/migration)

(defmigration "20250102-000000-add-age-to-users"
 (:up #'(lambda (conn)
          (add-column conn :table "users"
                           :columns '(("age" :type :integer
                                             :default-value 0))))
  :down #'(lambda (conn)
            (drop-column conn :table "users"
                              :column "age"))))
