(defpackage #:todoapp-test/models/todo
  (:use #:cl
        #:rove
        #:clails/test
        #:todoapp/models/todo)
  (:import-from #:clails/model
                #:ref
                #:destroy))

(in-package #:todoapp-test/models/todo)

(deftest-suite :model test-create-todo
  "create-todo creates a new todo"
  (let ((todo (create-todo "Test task")))
    (ok (typep todo '<todo>))
    (ok (string= "Test task" (ref todo :title)))
    (ok (not (ref todo :done)))
    (ok (ref todo :id))
    (destroy todo)))

(deftest-suite :model test-find-all
  "find-all returns all todos"
  (let ((todo1 (create-todo "Task 1"))
        (todo2 (create-todo "Task 2")))
    (let ((todos (find-all)))
      (ok (>= (length todos) 2)))
    (destroy todo1)
    (destroy todo2)))

(deftest-suite :model test-find-by-id
  "find-by-id returns todo by id"
  (let* ((todo (create-todo "Task with ID"))
         (id (ref todo :id))
         (found (find-by-id id)))
    (ok found)
    (ok (= id (ref found :id)))
    (ok (string= "Task with ID" (ref found :title)))
    (destroy todo)))

(deftest-suite :model test-mark-as-done
  "mark-as-done updates todo status"
  (let ((todo (create-todo "Task to complete")))
    (ok (not (ref todo :done)))
    (mark-as-done todo)
    (ok (ref todo :done))
    (ok (ref todo :done-at))
    (destroy todo)))
