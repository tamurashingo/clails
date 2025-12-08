;;;; Test for Task Core (DSL)

(defpackage #:clails-test/task/core
  (:use #:cl
        #:rove
        #:clails/task))

(in-package #:clails-test/task/core)

;;; Test: Define a simple task with deftask

(defun setup-deftask-simple ()
  (clear-registry))

(deftest test-deftask-simple
  (testing "Define a simple task with deftask"
    (setup-deftask-simple)

    (let ((executed nil))
      (deftask :test-task
        :description "Test task"
        :function (lambda ()
                    (setf executed t)))

      (ok (task-exists-p :test-task))
      (run-task :test-task)
      (ok executed))))

;;; Test: Define a task with namespace

(defun setup-deftask-namespace ()
  (clear-registry))

(deftest test-deftask-with-namespace
  (testing "Define a task with namespace"
    (setup-deftask-namespace)

    (let ((executed nil))
      (deftask :migrate
        :namespace :db
        :description "Run migrations"
        :function (lambda ()
                    (setf executed t)))

      (ok (task-exists-p :migrate :db))
      (run-task :migrate :db)
      (ok executed))))

;;; Test: Define tasks with defnamespace

(defun setup-defnamespace ()
  (clear-registry))

(deftest test-defnamespace
  (testing "Define multiple tasks with defnamespace"
    (setup-defnamespace)

    (let ((task1-executed nil)
          (task2-executed nil))
      (defnamespace :test
        (deftask :task1
          :description "Task 1"
          :function (lambda ()
                      (setf task1-executed t)))

        (deftask :task2
          :description "Task 2"
          :function (lambda ()
                      (setf task2-executed t))))

      (ok (task-exists-p :task1 :test))
      (ok (task-exists-p :task2 :test))

      (run-task :task1 :test)
      (ok task1-executed)
      (ok (not task2-executed))

      (run-task :task2 :test)
      (ok task2-executed))))

;;; Test: Define task with dependencies

(defvar *dep-execution-order* nil)

(defun setup-deftask-dependencies ()
  (clear-registry)
  (setf *dep-execution-order* '()))

(deftest test-deftask-with-dependencies
  (testing "Define task with dependencies"
    (setup-deftask-dependencies)

    (deftask :base
      :function (lambda ()
                  (push :base *dep-execution-order*)))

    (deftask :derived
      :depends-on (:base)
      :function (lambda ()
                  (push :derived *dep-execution-order*)))

    (run-task :derived)

    (ok (= 2 (length *dep-execution-order*)))
    (ok (eq :derived (first *dep-execution-order*)))
    (ok (eq :base (second *dep-execution-order*)))))

;;; Test: Define task with arguments

(defvar *task-arg-result* nil)

(defun setup-deftask-with-args ()
  (clear-registry)
  (setf *task-arg-result* nil))

(deftest test-deftask-with-arguments
  (testing "Define task with arguments"
    (setup-deftask-with-args)

    (deftask :greet
      :description "Greet someone"
      :args (&key name (greeting "Hello"))
      :function (lambda (&key name (greeting "Hello"))
                  (setf *task-arg-result*
                        (format nil "~A, ~A!" greeting name))))

    (run-task :greet :name "Alice")
    (ok (string= "Hello, Alice!" *task-arg-result*))

    (run-task :greet :name "Bob" :greeting "Hi")
    (ok (string= "Hi, Bob!" *task-arg-result*))))

;;; Test: Task with required and optional arguments

(defvar *rollback-steps* nil)

(defun setup-task-rollback ()
  (clear-registry)
  (setf *rollback-steps* nil))

(deftest test-task-with-required-and-optional-args
  (testing "Task with required and optional arguments"
    (setup-task-rollback)

    (deftask :rollback
      :namespace :db
      :description "Rollback migrations"
      :args (&key (steps 1))
      :function (lambda (&key (steps 1))
                  (setf *rollback-steps* steps)))

    (run-task :rollback :db)
    (ok (= 1 *rollback-steps*) "Default value should be 1")

    (run-task :rollback :db :steps 5)
    (ok (= 5 *rollback-steps*) "Should use provided value")))

;;; Test: Task with multiple arguments

(defvar *import-result* nil)

(defun setup-import-task ()
  (clear-registry)
  (setf *import-result* nil))

(deftest test-task-with-multiple-arguments
  (testing "Task with multiple arguments"
    (setup-import-task)

    (deftask :import
      :namespace :data
      :description "Import data from file"
      :args (&key file format (skip-errors nil))
      :function (lambda (&key file format (skip-errors nil))
                  (setf *import-result*
                        (list :file file
                              :format format
                              :skip-errors skip-errors))))

    (run-task :import :data
              :file "data.csv"
              :format :csv)
    (ok (equal '(:file "data.csv" :format :csv :skip-errors nil)
               *import-result*))

    (run-task :import :data
              :file "data.json"
              :format :json
              :skip-errors t)
    (ok (equal '(:file "data.json" :format :json :skip-errors t)
               *import-result*))))

