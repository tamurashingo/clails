;;;; Test for Task Registry

(defpackage #:clails-test/task/registry
  (:use #:cl
        #:rove
        #:clails/task))

(in-package #:clails-test/task/registry)

(deftest test-register-and-find-task
  (testing "Register a task and find it"
    (clear-registry)

    (let ((key (register-task :test-task
                              :description "Test task"
                              :function (lambda () (print "test")))))
      (ok (stringp key))
      (ok (string= "TEST-TASK" key))

      (let ((task (find-task :test-task)))
        (ok task)
        (ok (eq :test-task (task-info-name task)))
        (ok (null (task-info-namespace task)))
        (ok (string= "Test task" (task-info-description task)))))))

(deftest test-register-task-with-namespace
  (testing "Register a task with namespace"
    (clear-registry)

    (let ((key (register-task :migrate
                              :namespace :db
                              :description "Run migrations"
                              :function (lambda () (print "migrate")))))
      (ok (string= "DB:MIGRATE" key))

      (let ((task (find-task :migrate :db)))
        (ok task)
        (ok (eq :migrate (task-info-name task)))
        (ok (eq :db (task-info-namespace task)))))))

(deftest test-task-exists-p
  (testing "Check if task exists"
    (clear-registry)

    (register-task :foo :function (lambda () nil))
    (register-task :bar :namespace :baz :function (lambda () nil))

    (ok (task-exists-p :foo))
    (ok (task-exists-p :bar :baz))
    (ok (not (task-exists-p :nonexistent)))
    (ok (not (task-exists-p :bar :wrong-namespace)))))

(deftest test-list-tasks
  (testing "List all tasks"
    (clear-registry)

    (register-task :task1 :function (lambda () nil))
    (register-task :task2 :namespace :ns1 :function (lambda () nil))
    (register-task :task3 :namespace :ns1 :function (lambda () nil))
    (register-task :task4 :namespace :ns2 :function (lambda () nil))

    (let ((all-tasks (list-tasks)))
      (ok (= 4 (length all-tasks))))

    (let ((ns1-tasks (list-tasks :ns1)))
      (ok (= 2 (length ns1-tasks))))))

(deftest test-list-namespaces
  (testing "List all namespaces"
    (clear-registry)

    (register-task :task1 :function (lambda () nil))
    (register-task :task2 :namespace :db :function (lambda () nil))
    (register-task :task3 :namespace :maintenance :function (lambda () nil))
    (register-task :task4 :namespace :db :function (lambda () nil))

    (let ((namespaces (list-namespaces)))
      (ok (= 2 (length namespaces)))
      (ok (member :db namespaces :test #'eq))
      (ok (member :maintenance namespaces :test #'eq)))))

(deftest test-remove-task
  (testing "Remove a task"
    (clear-registry)

    (register-task :to-remove :function (lambda () nil))
    (ok (task-exists-p :to-remove))

    (remove-task :to-remove)
    (ok (not (task-exists-p :to-remove)))))

(deftest test-clear-registry
  (testing "Clear all tasks"
    (clear-registry)

    (register-task :task1 :function (lambda () nil))
    (register-task :task2 :function (lambda () nil))
    (ok (= 2 (length (list-tasks))))

    (clear-registry)
    (ok (= 0 (length (list-tasks))))))
