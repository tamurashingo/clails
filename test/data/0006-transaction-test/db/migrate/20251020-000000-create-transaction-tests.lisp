(in-package #:clails-test/model/db/transaction)

(defmigration "20251020-000000-create-transaction-tests"
  (:up #'(lambda (conn)
           (create-table conn :table "transaction_tests"
                              :columns '(("name" :type :string)
                                         ("value" :type :integer))))
   :down #'(lambda (conn)
            (drop-table conn :table "transaction_tests"))))
