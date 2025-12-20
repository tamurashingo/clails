(in-package #:clails-test/model/db)

(defmigration "20251218-000000-add-date-time-columns"
 (:up #'(lambda (conn)
          (add-column conn :table "event"
                           :columns '(("event-date" :type :date)))
          (add-column conn :table "event"
                           :columns '(("event-time" :type :time))))
  :down #'(lambda (conn)
           (drop-column conn :table "event"
                             :column "event-date")
           (drop-column conn :table "event"
                             :column "event-time"))))
