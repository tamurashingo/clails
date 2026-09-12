(in-package #:clails-test/model/db/eager-loading)

;;; Table names are prefixed with eager_ to avoid colliding with the
;;; "company"/"department"/"office" tables used by other test fixtures that
;;; share the same test database (e.g. test/data/0003-save-test).

(defmigration "20250912-000000-create-table"
 (:up #'(lambda (conn)
          (create-table conn :table "eager_company"
                             :columns '(("name" :type :string
                                               :not-null T)))
          (create-table conn :table "eager_department"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("company-id" :type :integer)))
          (create-table conn :table "eager_office"
                             :columns '(("name" :type :string
                                                :not-null T)
                                        ("company-id" :type :integer))))
  :down #'(lambda (conn)
            (drop-table conn :table "eager_office")
            (drop-table conn :table "eager_department")
            (drop-table conn :table "eager_company"))))
