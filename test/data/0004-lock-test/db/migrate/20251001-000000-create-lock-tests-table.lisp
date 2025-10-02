(in-package #:clails-test/model/db/lock)


(defmigration "20251001-000000-create-lock-tests-table"
  (:up #'(lambda (conn)
           (create-table conn :table "lock_tests"
                              :columns '(("name" :type :string)
                                         ("lock-version" :type :integer
                                                         :not-null T
                                                         :default-value 0))))
   :down #'(lambda (conn)
             (drop-table conn :table "lock_tests"))))
