(in-package #:todoapp-db)

(defun run ()
  "Create seed data for todo table."
  (let ((todo1 (make-record 'todoapp/models/todo:<todo> :title "Buy milk" :done nil))
        (todo2 (make-record 'todoapp/models/todo:<todo> :title "Read a book" :done nil))
        (todo3 (make-record 'todoapp/models/todo:<todo> :title "Write code" :done nil)))
    (save todo1)
    (save todo2)
    (save todo3)
    (format t "Created 3 todo items~%")))

(run)
