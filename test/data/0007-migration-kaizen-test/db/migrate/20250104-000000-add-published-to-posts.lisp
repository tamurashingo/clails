(in-package #:clails-test/model/db/migration)

(defmigration "20250104-000000-add-published-to-posts"
 (:up #'(lambda (conn)
          (add-column conn :table "posts"
                           :columns '(("published" :type :boolean
                                                   :default-value NIL))))
  :down #'(lambda (conn)
            (drop-column conn :table "posts"
                              :column "published"))))
