(in-package #:clails-test/model/db/migration)

(defmigration "20250105-000000-create-comments-table"
 (:up #'(lambda (conn)
          (create-table conn :table "comments"
                             :columns '(("post_id" :type :integer
                                                   :not-null T)
                                        ("comment" :type :text
                                                   :not-null T))))
  :down #'(lambda (conn)
            (drop-table conn :table "comments"))))
