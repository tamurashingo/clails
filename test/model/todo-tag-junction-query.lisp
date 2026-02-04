(in-package #:cl-user)
(defpackage #:clails-test/model/todo-tag-junction-query
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:ref-in))

(defpackage #:clails-test/model/db/todo-tag-junction-query
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))

(in-package #:clails-test/model/todo-tag-junction-query)


(setup
  (clrhash clails/model/base-model::*table-information*)
  
  (defmodel <todo> (<base-model>)
    (:table "todo"
     :relations ((:has-many "clails-test/model/todo-tag-junction-query::<todo-tag>"
                   :as :todo-tags
                   :foreign-key :todo-id))))

  (defmodel <tag> (<base-model>)
    (:table "tag"
     :relations ((:has-many "clails-test/model/todo-tag-junction-query::<todo-tag>"
                   :as :todo-tags
                   :foreign-key :tag-id))))

  (defmodel <todo-tag> (<base-model>)
    (:table "todo_tags"
     :relations ((:belongs-to "clails-test/model/todo-tag-junction-query::<todo>"
                   :column :todo
                   :key :todo-id)
                 (:belongs-to "clails-test/model/todo-tag-junction-query::<tag>"
                   :column :tag
                   :key :tag-id))))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0013" "/app/test/data/0013-todo-tag-junction-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/connection::with-db-connection-direct (connection)
    ;; todo: A
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, owner_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Todo A', 'owner1')")
    ;; todo: B
    (dbi-cp:do-sql connection "insert into todo (created_at, updated_at, title, owner_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'Todo B', 'owner1')")
    ;; tag: X
    (dbi-cp:do-sql connection "insert into tag (created_at, updated_at, name, owner_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 'Tag X', 'owner1')")
    ;; tag: Y
    (dbi-cp:do-sql connection "insert into tag (created_at, updated_at, name, owner_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 'Tag Y', 'owner1')")
    ;; todo-tags: A(id=1), X(id=1)
    (dbi-cp:do-sql connection "insert into todo_tags (created_at, updated_at, todo_id, tag_id, owner_id) values ('2024-01-01 00:00:00', '2024-01-01 00:00:00', 1, 1, 'owner1')")
    ;; todo-tags: B(id=2), Y(id=2)
    (dbi-cp:do-sql connection "insert into todo_tags (created_at, updated_at, todo_id, tag_id, owner_id) values ('2024-01-02 00:00:00', '2024-01-02 00:00:00', 2, 2, 'owner1')"))
  (clails/model/connection:startup-connection-pool)
  (clails/model/base-model:initialize-table-information))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))

(deftest test-simple-parameter-order-bug
  (testing "Minimal reproduction: single table with :in clause"
    (let ((simple-query (query <todo>
                              :as :todos
                              :where (:and (:= (:todos :owner-id) :owner-id)
                                           (:= (:todos :title) :title)
                                           (:in (:todos :id) :ids)))))
      (multiple-value-bind (sql params)
          (generate-query simple-query
                         '(:owner-id "owner1" :title "Todo A" :ids (1 2 3)))
        (format t "~%=== Simple Query Test ===~%")
        (format t "Generated SQL: ~A~%" sql)
        (format t "Parameters: ~A~%" params)
        (format t "Expected params order: (~S ~S ~A ~A ~A)~%" "owner1" "Todo A" 1 2 3)
        (format t "Actual params order:   ~A~%" params)
        (ok (equal params '("owner1" "Todo A" 1 2 3)) 
            "Parameters should be in WHERE clause order")))))

(deftest test-find-todos-by-tag-ids
  (let ((find-query (query <todo-tag>
                          :as :todo-tags
                          :joins ((:inner-join :todo))
                          :where (:and (:= (:todo :owner-id) :owner-id)
                                       (:= (:todo-tags :owner-id) :owner-id)
                                       (:in (:todo-tags :tag-id) :tag-ids))
                          :order-by ((:todo :created-at :desc)))))
    (testing "Find todo A by tag X (tag-id=1)"
      (let ((results (execute-query find-query
                                    '(:owner-id "owner1" :tag-ids (1)))))
        (format t "~%Results: ~S~%" results)
        (ok (= 1 (length results)))
        (ok (string= "Todo A" (ref (first results) :title)))))
    
    (testing "Verify parameter order bug with JOIN query"
      (multiple-value-bind (sql params)
          (generate-query find-query
                         '(:owner-id "owner1" :tag-ids (1 2)))
        (format t "~%=== JOIN Query Test ===~%")
        (format t "Generated SQL: ~A~%" sql)
        (format t "Parameters: ~A~%" params)
        (format t "Expected params order: (~S ~S ~A ~A)~%" "owner1" "owner1" 1 2)
        (format t "Actual params order:   ~A~%" params)
        (ok (equal params '("owner1" "owner1" 1 2))
            "Parameters should be in WHERE clause order")))))

