# QuickStart - Building a TODO Application

This guide walks you through creating a simple TODO management application using clails.
You'll learn how to create a database, implement models, views, and controllers, and start a web application step by step.

## Prerequisites

- Roswell must be installed
- qlot must be installed
- clails must be installed

For installation instructions, please refer to [README.md](../README.md).

---

## 1. Create a Project

First, create a new project using the `clails new` command.

```bash
clails new todoapp
cd todoapp
```

This creates the basic structure for a clails application.

---

## 2. Create the Database

Create the database using the `clails db:create` command.

```bash
clails db:create
```

By default, a SQLite3 database is created.
To use MySQL or PostgreSQL, specify the `--database` option when creating the project.

---

## 3. Generate Scaffold

Generate Model, View, and Controller files at once using the `clails generate:scaffold` command.

```bash
clails generate:scaffold todo
```

This command generates the following files:

- `app/models/todo.lisp` - Model file
- `app/views/todo/list.html` - View file
- `app/controllers/todo-controller.lisp` - Controller file
- `db/migrate/YYYYMMDD-HHMMSS-todo.lisp` - Migration file
- `test/models/todo.lisp` - Model test file
- `test/controllers/todo-controller.lisp` - Controller test file

---

## 4. Modify the Migration File

Edit the generated migration file to define the TODO table structure.

Open `db/migrate/YYYYMMDD-HHMMSS-todo.lisp` and modify it as follows:

```lisp
(in-package #:todoapp-db)

(defmigration "todo"
  (:up #'(lambda (conn)
           (create-table conn :table "todo"
                              :columns '(("title" :type :string
                                                  :not-null T)
                                         ("done" :type :boolean
                                                 :not-null T
                                                 :default 0)
                                         ("done_at" :type :datetime))))
   :down #'(lambda (conn)
             (drop-table conn :table "todo"))))
```

This table definition includes:
- `title` - TODO title (required)
- `done` - Completion flag (default: false)
- `done_at` - Completion timestamp

---

## 5. Run the Migration

After modifying the migration file, create the table using the `clails db:migrate` command.

```bash
clails db:migrate
```

This creates the TODO table in the database.

---

## 6. Modify the Model

Open `app/models/todo.lisp` and add the necessary functionality for the TODO application.

```lisp
(in-package #:cl-user)
(defpackage #:todoapp/models/todo
  (:use #:cl)
  (:import-from #:clails/model
                #:defmodel
                #:<base-model>
                #:query
                #:execute-query
                #:make-record
                #:save
                #:ref)
  (:import-from #:local-time
                #:now)
  (:export #:<todo>
           #:find-all
           #:create-todo
           #:find-by-id
           #:mark-as-done))

(in-package #:todoapp/models/todo)

(defmodel <todo> (<base-model>) (:table "todo"))

(defun find-all ()
  "Find all todo items.

   @return [list] List of todo records
   "
  (let ((q (query <todo> :as :todo)))
    (execute-query q nil)))

(defun create-todo (title)
  "Create a new todo item with the given title.

   @param title [string] Todo title
   @return [<todo>] Created todo record
   "
  (let ((todo (make-record '<todo> :title title :done nil)))
    (save todo)
    todo))

(defun find-by-id (id)
  "Find a todo item by ID.

   @param id [integer] Todo ID
   @return [<todo>] Todo record
   @return [nil] NIL if not found
   "
  (let* ((q (query <todo> :as :todo :where (:= (:todo :id) :id-param)))
         (results (execute-query q (list :id-param id))))
    (car results)))

(defun mark-as-done (todo)
  "Mark a todo item as done.

   @param todo [<todo>] Todo record to mark as done
   @return [<todo>] Updated todo record
   "
  (setf (ref todo :done) t)
  (setf (ref todo :done-at) (now))
  (save todo)
  todo)
```

This code implements the following functionality:
- `find-all` - Retrieve all TODO items
- `create-todo` - Create a new TODO item
- `find-by-id` - Find a TODO by ID
- `mark-as-done` - Mark a TODO as completed

---

## 7. Modify the View

Open `app/views/todo/list.html` and modify it to display and manipulate TODO items.

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Todo List</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; margin-top: 20px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .done { text-decoration: line-through; color: #999; }
        form { margin-top: 20px; }
        input[type="text"] { padding: 5px; width: 300px; }
        button { padding: 5px 15px; background-color: #4CAF50; color: white; border: none; cursor: pointer; }
        button:hover { background-color: #45a049; }
    </style>
</head>
<body>
    <h1>Todo List</h1>
    
    <h2>Add New Todo</h2>
    <form action="/todo" method="POST">
        <input type="text" name="title" placeholder="Enter todo title" required>
        <button type="submit">Add</button>
    </form>
    
    <h2>Todo Items</h2>
    <table>
        <thead>
            <tr>
                <th>ID</th>
                <th>Title</th>
                <th>Status</th>
                <th>Done At</th>
                <th>Action</th>
            </tr>
        </thead>
        <tbody>
            <cl:loop for="todo" in="(view :todos)">
            <tr>
                <td><%= (clails/model:ref todo :id) %></td>
                <td class="<%= (if (clails/model:ref todo :done) "done" "") %>">
                    <%= (clails/model:ref todo :title) %>
                </td>
                <td><%= (if (clails/model:ref todo :done) "Done" "Pending") %></td>
                <td><%= (or (clails/model:ref todo :done-at) "-") %></td>
                <td>
                    <cl:unless test="(clails/model:ref todo :done)">
                    <form action="/todo/<%= (clails/model:ref todo :id) %>" method="POST" style="display:inline;">
                        <input type="hidden" name="_method" value="PUT">
                        <button type="submit">Mark as Done</button>
                    </form>
                    </cl:unless>
                </td>
            </tr>
            </cl:loop>
        </tbody>
    </table>
    
    <p style="margin-top: 20px; color: #666;">
        Total: <%= (length (view :todos)) %> items
    </p>
</body>
</html>
```

This view implements:
- TODO addition form
- TODO list display (table format)
- Button to mark TODO as completed
- Strikethrough styling for completed TODO items

**How to Implement PUT in HTML Forms:**

HTML `<form>` tags only support `GET` and `POST` methods natively.
To send PUT or DELETE requests, use the following approach:

```html
<form action="/todo/123" method="POST">
    <input type="hidden" name="_method" value="PUT">
    <button type="submit">Mark as Done</button>
</form>
```

clails checks for the `_method` parameter in POST requests. When the value is `"PUT"`, it routes to the `do-put` method, and when it's `"DELETE"`, it routes to the `do-delete` method. This allows HTML forms to send PUT and DELETE requests.

---

## 8. Modify the Controller

Open `app/controllers/todo-controller.lisp` and modify it to handle requests.

```lisp
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
```

This controller implements:
- `do-get` - Display TODO list
- `do-post` - Create a new TODO
- `do-put` - Mark TODO as completed

---

## 9. Start the Server

Once all implementation is complete, start the server.

```bash
clails server
```

By default, the server starts at `http://localhost:5000`.

---

## 10. Use the TODO Application

Access `http://localhost:5000/todo` in your browser.

You can perform the following operations:

1. **Add TODO**: Enter a title in the "Add New Todo" form and click the "Add" button
2. **View TODO list**: All registered TODOs are displayed
3. **Complete TODO**: Click the "Mark as Done" button to mark a TODO as completed

---

## (Optional) Add Seed Data

If you want to add initial data, create `db/seeds.lisp`.

```lisp
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
```

To load the seed data:

```bash
clails db:seed
```

---

## Summary

In this QuickStart, you learned how to create a TODO application using clails.

Main steps:
1. Create a project (`clails new`)
2. Create a database (`clails db:create`)
3. Generate scaffold (`clails generate:scaffold`)
4. Edit migration file
5. Run migration (`clails db:migrate`)
6. Implement Model
7. Implement View
8. Implement Controller
9. Start server (`clails server`)

By applying these steps, you can create more complex web applications.

## Next Steps

- [Model Guide](model.md) - Database operations and query details
- [View Guide](view.md) - Template engine details
- [Controller Guide](controller.md) - Request handling and routing details
- [Testing Guide](testing.md) - How to write tests
