(defpackage #:todoapp-test/controllers/todo-controller
  (:use #:cl
        #:rove
        #:clails/test
        #:todoapp/controllers/todo-controller)
  (:import-from #:clails/controller/base-controller
                #:code
                #:response))

(in-package #:todoapp-test/controllers/todo-controller)

(deftest-suite :controller test-controller-creation
  "todo-controller can be created"
  (let ((controller (make-instance '<todo-controller>)))
    (ok (typep controller '<todo-controller>))
    (ok (= 200 (code controller)))))
