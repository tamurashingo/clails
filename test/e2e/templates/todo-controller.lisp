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
  "Update todo item to mark as done.

   Reached by the form in todo/list.html, which POSTs to the MEMBER path
   /todo/<id> with a hidden \"_method=put\" field: the request-method override
   stage rewrites the method before the route is matched, and the route
   /todo/:id -- appended to app/config/environment.lisp from
   test/e2e/templates/todo-routes.lisp, since generate:scaffold writes only the
   collection route -- is what puts \"id\" into the parameters.  Without BOTH
   halves this method is unreachable: the override alone leaves a PUT at
   /todo/<id> with no matching route (a 404), and the route alone leaves the
   request a POST (do-post)."
  (let* ((id-str (param controller "id"))
         (id (parse-integer id-str))
         (todo (find-by-id id)))
    (when todo
      (mark-as-done todo))
    (set-redirect controller "/todo")))
