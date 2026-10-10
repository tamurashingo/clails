(in-package #:clails-test/model/db/migration-filename-validation)

(defmigration "20250101-000000-create-widgets-table"
  (:up #'(lambda (conn)
           (create-table conn :table "widgets"
                              :columns '(("name" :type :string
                                                 :not-null T))))
   :down #'(lambda (conn)
             (drop-table conn :table "widgets"))))
