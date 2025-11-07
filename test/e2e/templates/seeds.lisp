(in-package #:cl-user)
(defpackage #:todoapp-db
  (:use #:cl)
  (:import-from #:clails/model
                #:save
                #:make-record)
  (:import-from #:todoapp/models/todo
                #:<todo>))

(in-package #:todoapp-db)

(defun run ()
  "Create seed data for todo table."
  (let ((todo1 (make-record '<todo> :title "Buy milk" :done nil))
        (todo2 (make-record '<todo> :title "Read a book" :done nil))
        (todo3 (make-record '<todo> :title "Write code" :done nil)))
    (save todo1)
    (save todo2)
    (save todo3)
    (format t "Created 3 todo items~%")))
