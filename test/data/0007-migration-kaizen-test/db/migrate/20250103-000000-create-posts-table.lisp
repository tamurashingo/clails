(in-package #:clails-test/model/db/migration)

(defmigration "20250103-000000-create-posts-table"
 (:up #'(lambda (conn)
          (create-table conn :table "posts"
                             :columns '(("title" :type :string
                                                :not-null T)
                                        ("content" :type :text)
                                        ("user_id" :type :integer
                                                  :not-null T)))
          (add-index conn :table "posts"
                          :index "idx-user-id"
                          :columns '("user_id")))
  :down #'(lambda (conn)
            (drop-table conn :table "posts"))))
