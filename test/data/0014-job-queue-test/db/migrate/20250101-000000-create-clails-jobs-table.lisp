(in-package #:clails-test/job/db)

(defmigration "20250101-000000-create-clails-jobs-table"
 (:up #'(lambda (connection)
          (create-table connection :table "clails_jobs"
                                   :columns '(("job-name" :type :string
                                                         :not-null T)
                                              ("arguments" :type :text
                                                          :not-null T)
                                              ("status" :type :string
                                                       :not-null T
                                                       :default-value "pending")
                                              ("attempts" :type :integer
                                                         :not-null T
                                                         :default-value 0)
                                              ("max-attempts" :type :integer
                                                             :not-null T
                                                             :default-value 25)
                                              ("available-at" :type :datetime
                                                             :not-null T)
                                              ("last-error" :type :text
                                                           :not-null NIL)))
          (add-index connection :table "clails_jobs"
                                :index "idx-clails-jobs-status-available-at"
                                :columns '("status" "available-at")))
  :down #'(lambda (connection)
            (drop-table connection :table "clails_jobs"))))
