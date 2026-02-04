(in-package #:clails-test/model/db/todo-tag-junction-query)

(defmigration "20260204-000000-create-todo-tag-tables"
 (:up #'(lambda (conn)
          (create-table conn :table "todo"
                             :columns '(("title" :type :string
                                                :not-null t)
                                       ("owner-id" :type :string
                                                  :not-null t)))
          (create-table conn :table "tag"
                             :columns '(("name" :type :string
                                                :not-null t)
                                       ("owner-id" :type :string
                                                  :not-null t)))
          (create-table conn :table "todo-tags"
                             :columns '(("todo-id" :type :integer
                                                   :not-null t)
                                       ("tag-id" :type :integer
                                                 :not-null t)
                                       ("owner-id" :type :string
                                                   :not-null t))))
  :down #'(lambda (conn)
            (drop-table conn :table "todo-tags")
            (drop-table conn :table "tag")
            (drop-table conn :table "todo"))))
