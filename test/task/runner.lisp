;;;; Test for Task Runner

(defpackage #:clails-test/task/runner
  (:use #:cl
        #:rove
        #:clails/task))

(in-package #:clails-test/task/runner)

;;; Test: Run a simple task

(defvar *simple-executed* nil)

(defun setup-simple-task ()
  (clear-registry)
  (setf *simple-executed* nil)
  (register-task :simple
                 :function (lambda ()
                             (setf *simple-executed* t))))

(deftest test-run-simple-task
  (testing "Run a simple task"
    (setup-simple-task)
    (run-task :simple)
    (ok *simple-executed*)))

;;; Test: Run a task with namespace

(defvar *namespace-executed* nil)

(defun setup-namespace-task ()
  (clear-registry)
  (setf *namespace-executed* nil)
  (register-task :migrate
                 :namespace :db
                 :function (lambda ()
                             (setf *namespace-executed* t))))

(deftest test-run-task-with-namespace
  (testing "Run a task with namespace"
    (setup-namespace-task)
    (run-task :migrate :db)
    (ok *namespace-executed*)))

;;; Test: Run a task with dependencies

(defvar *execution-order* nil)

(defun setup-dependency-tasks ()
  (clear-registry)
  (setf *execution-order* '())

  (register-task :task1
                 :function (lambda ()
                             (push :task1 *execution-order*)))

  (register-task :task2
                 :depends-on '(:task1)
                 :function (lambda ()
                             (push :task2 *execution-order*)))

  (register-task :task3
                 :depends-on '(:task1 :task2)
                 :function (lambda ()
                             (push :task3 *execution-order*))))

(deftest test-run-task-with-dependencies
  (testing "Run a task with dependencies"
    (setup-dependency-tasks)
    (run-task :task3)

    (ok (= 3 (length *execution-order*)))
    (ok (eq :task3 (first *execution-order*)))
    (ok (eq :task2 (second *execution-order*)))
    (ok (eq :task1 (third *execution-order*)))))

;;; Test: Idempotency

(defvar *counter* 0)

(defun setup-idempotency-tasks ()
  (clear-registry)
  (setf *counter* 0)

  (register-task :base
                 :function (lambda ()
                             (incf *counter*)))

  (register-task :dep1
                 :depends-on '(:base)
                 :function (lambda () nil))

  (register-task :dep2
                 :depends-on '(:base)
                 :function (lambda () nil))

  (register-task :final
                 :depends-on '(:dep1 :dep2)
                 :function (lambda () nil)))

(deftest test-idempotency
  (testing "Task is executed only once even with multiple dependencies"
    (setup-idempotency-tasks)
    (run-task :final)
    (ok (= 1 *counter*) "Base task should be executed only once")))

;;; Test: Task not found error

(deftest test-task-not-found-error
  (testing "Error when task not found"
    (clear-registry)
    (ok (signals (run-task :nonexistent) 'error))))

;;; Test: Dependency not found error

(defun setup-bad-dependency-task ()
  (clear-registry)
  (register-task :task-with-bad-dep
                 :depends-on '(:nonexistent)
                 :function (lambda () nil)))

(deftest test-dependency-not-found-error
  (testing "Error when dependency task not found"
    (setup-bad-dependency-task)
    (ok (signals (run-task :task-with-bad-dep) 'error))))

;;; Test: Run task with arguments

(defvar *arg-task-result* nil)

(defun setup-task-with-args ()
  (clear-registry)
  (setf *arg-task-result* nil)
  (register-task :process
                 :function (lambda (&key input output)
                             (setf *arg-task-result*
                                   (list :input input :output output)))))

(deftest test-run-task-with-arguments
  (testing "Run task with arguments"
    (setup-task-with-args)

    (run-task :process :input "test.txt" :output "result.txt")
    (ok (equal '(:input "test.txt" :output "result.txt")
               *arg-task-result*))))

;;; Test: Run task with default argument values

(defvar *default-arg-result* nil)

(defun setup-task-with-default-args ()
  (clear-registry)
  (setf *default-arg-result* nil)
  (register-task :backup
                 :function (lambda (&key (destination "/backup") (compress t))
                             (setf *default-arg-result*
                                   (list :destination destination
                                         :compress compress)))))

(deftest test-run-task-with-default-arguments
  (testing "Run task with default argument values"
    (setup-task-with-default-args)

    ;; Use all defaults
    (run-task :backup)
    (ok (equal '(:destination "/backup" :compress t)
               *default-arg-result*))

    ;; Override some defaults
    (run-task :backup :destination "/tmp/backup")
    (ok (equal '(:destination "/tmp/backup" :compress t)
               *default-arg-result*))

    ;; Override all
    (run-task :backup :destination "/home/backup" :compress nil)
    (ok (equal '(:destination "/home/backup" :compress nil)
               *default-arg-result*))))

