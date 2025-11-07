(in-package #:cl-user)
(defpackage #:todoapp/controllers/todo-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:clails/controller/base-controller
                #:param)
  (:import-from #:todoapp/models/todo
                #:find-all
                #:create-todo
                #:find-by-id
                #:mark-as-done)
  (:export #:<todo-controller>))

(in-package #:todoapp/controllers/todo-controller)

(defclass <todo-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <todo-controller>))
  "Get all todo items and display them."
  (let ((todos (find-all)))
    (set-view controller "todo/list.html" (list :todos todos))))

(defmethod do-post ((controller <todo-controller>))
  "Create a new todo item."
  (let ((title (param controller "title")))
    (create-todo title)
    (set-redirect controller "/todo")))

(defmethod do-put ((controller <todo-controller>))
  "Update todo item to mark as done."
  (let* ((id-str (param controller "id"))
         (id (parse-integer id-str))
         (todo (find-by-id id)))
    (when todo
      (mark-as-done todo))
    (set-redirect controller "/todo")))
