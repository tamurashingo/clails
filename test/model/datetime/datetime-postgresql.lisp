(in-package #:cl-user)
(defpackage #:clails-test/model/datetime/datetime-postgresql
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
(in-package #:clails-test/model/datetime/datetime-postgresql)

(setup
  (clrhash clails/model/base-model::*table-information*)
  (defmodel <event> (<base-model>)
    (:table "event"))

  (setf clails/environment:*database-type* (make-instance 'clails/environment::<database-type-postgresql>))
  (setf clails/environment:*project-environment* :test)
  (setf clails/environment:*database-config* `(:test (:database-name ,(env-or-default "CLAILS_POSTGRESQL_DATABASE" "clails_test")
                                                      :username ,(env-or-default "CLAILS_POSTGRESQL_USERNAME" "clails")
                                                      :password ,(env-or-default "CLAILS_POSTGRESQL_PASSWORD" "password")
                                                      :host ,(env-or-default "CLAILS_POSTGRESQL_HOST" "postgresql-test")
                                                      :port ,(env-or-default "CLAILS_POSTGRESQL_PORT" "5432"))))
  (setf clails/environment:*migration-base-dir* (env-or-default "CLAILS_MIGRATION_DIR_0009" "/app/test/data/0009-datetime-test"))
  (uiop:setup-temporary-directory)
  (ensure-directories-exist (merge-pathnames "db/" uiop:*temporary-directory*))
  (setf clails/environment::*project-dir* uiop:*temporary-directory*)
  (clails/model/migration::db-create)
  (clails/model/migration::db-migrate)
  (clails/model/base-model:initialize-table-information)
  (clails/model/connection:startup-connection-pool))

(teardown
  (uiop:delete-directory-tree uiop:*temporary-directory* :if-does-not-exist :ignore :validate t)
  (clails/model/connection:shutdown-connection-pool))

(deftest datetime-column-returns-universal-time-postgresql
  (testing "datetime column returns universal-time after save and fetch"
    (let* ((test-datetime (encode-universal-time 0 30 14 17 12 2025))
           (event (make-record '<event>
                               :title "Test Event"
                               :start-datetime test-datetime
                               :end-datetime nil)))
      (ok (save event)
          "Event saved successfully")

      (let* ((query (query <event> :as :event :where (:= (:event :id) :id)))
             (result (car (execute-query query (list :id (ref event :id))))))

        (ok (integerp (ref result :start-datetime))
            "start-datetime should be an integer (universal-time)")

        (ok (= test-datetime (ref result :start-datetime))
            (format nil "start-datetime should equal the saved value: expected ~A, got ~A"
                    test-datetime (ref result :start-datetime)))

        (ok (null (ref result :end-datetime))
            "end-datetime should be nil when NULL in database")))))


(deftest date-column-returns-universal-time-postgresql
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


(deftest time-column-returns-seconds-postgresql
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
