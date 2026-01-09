(in-package #:cl-user)
(defpackage #:clails-test/model/datetime/datetime-mysql
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/util
                #:env-or-default)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref))

(defpackage #:clails-test/model/db
  (:use #:cl)
  (:import-from #:clails/model/migration
                #:defmigration
                #:create-table
                #:drop-table))
(in-package #:clails-test/model/datetime/datetime-mysql)

(setup
  (clrhash clails/model/base-model::*table-information*)
  (defmodel <event> (<base-model>)
    (:table "event"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-mysql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
                                                      :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
                                                      :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0009" "/app/test/data/0009-datetime-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/base-model::initialize-table-information)
  (clails/model/connection:startup-connection-pool))


(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))


(deftest datetime-column-returns-universal-time-mysql
  (testing "datetime column returns universal-time after save and fetch"
    (let* ((start-datetime (encode-universal-time 0 0 10 17 12 2025))
           (end-datetime (encode-universal-time 0 0 18 17 12 2025))
           (event (make-record '<event>
                               :title "Full Day Event"
                               :start-datetime start-datetime
                               :end-datetime end-datetime)))
      (ok (save event)
          "Event with both datetime values saved successfully")

      (let* ((query (query <event> :as :e :where (:= (:e :id) :id)))
             (result (car (execute-query query (list :id (ref event :id))))))

        (ok result "Record fetched from database")

        (ok (integerp (ref result :start-datetime))
            "start-datetime should be an integer (universal-time)")
        (ok (= start-datetime (ref result :start-datetime))
            "start-datetime should match saved value")

        (ok (integerp (ref result :end-datetime))
            "end-datetime should be an integer (universal-time)")

        (ok (= end-datetime (ref result :end-datetime))
            "end-datetime should match saved value")))))



(deftest date-column-returns-universal-time-mysql
  (testing "date column returns universal-time after save and fetch"
    (let* ((test-date (encode-universal-time 0 0 0 17 12 2025))
           (event (make-record '<event>
                               :title "Date Test Event"
                               :event-date test-date)))
      (ok (save event)
          "Event with date saved successfully")

      (let* ((query (query <event> :as :e :where (:= (:e :id) :id)))
             (result (car (execute-query query (list :id (ref event :id))))))

        (ok result "Record fetched from database")

        (ok (integerp (ref result :event-date))
            "event-date should be an integer (universal-time)")

        (ok (= test-date (ref result :event-date))
            (format nil "event-date should equal the saved value: expected ~A, got ~A"
                    test-date (ref result :event-date)))))))


(deftest time-column-returns-seconds-mysql
  (testing "time column returns seconds from midnight after save and fetch"
    (let* ((test-time-seconds (+ (* 14 3600) (* 30 60)))
           (event (make-record '<event>
                               :title "Time Test Event"
                               :event-time test-time-seconds)))
      (ok (save event)
          "Event with time saved successfully")

      (let* ((query (query <event> :as :e :where (:= (:e :id) :id)))
             (result (car (execute-query query (list :id (ref event :id))))))

        (ok result "Record fetched from database")

        (ok (numberp (ref result :event-time))
            "event-time should be a number (seconds from midnight)")

        (ok (= test-time-seconds (ref result :event-time))
            (format nil "event-time should equal the saved value: expected ~A, got ~A"
                    test-time-seconds (ref result :event-time)))))))

