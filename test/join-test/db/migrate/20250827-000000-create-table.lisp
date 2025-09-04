(in-package #:clails-test/model/db/join-query)

(defmigration "20250827-000000-create-table"
 (:up #'(lambda (conn)
          (create-table conn :table "account"
                             :columns '(("username" :type :string
                                                    :not-null T)))
          (create-table conn :table "blog"
                             :columns '(("title" :type :string)
                                        ("content" :type :string)
                                        ("account-id" :type :integer)
                                        ("star" :type :integer)))
          (create-table conn :table "comment"
                             :columns '(("comment" :type :string)
                                        ("blog-id" :type :integer))))
  :down #'(lambda (conn)
            (drop-table conn :table "comment")
            (drop-table conn :table "blog")
            (drop-table conn :table "account"))))

