# QuickStart - Building a TODO Application

This guide walks you through creating a simple TODO management application using clails.
You'll learn how to create a database, implement models, views, and controllers, and start a web application step by step.

## Prerequisites

### For Local Development

- Roswell must be installed
- clails must be installed

For installation instructions, please refer to [README.md](../README.md).

### For Docker Development (Recommended)

- Docker must be installed
- Docker Compose must be installed

---

## 1. Create a Project

First, create a new project using the `clails new` command.

```bash
clails new todoapp
cd todoapp
```

This creates the basic structure for a clails application.

The following files are generated during project creation:
- `Makefile` - A collection of commands to simplify development with Docker
- `docker/Dockerfile.dev` - Docker image definition for development
- `docker/docker-compose.dev.yml` - Docker Compose configuration
- `docker/dev.env` - Environment variable settings for development

---

## 2. Development Environment Setup

### Using Docker Environment (Recommended)

#### 2.1. Build Docker Image

```bash
make build
```

To specify a clails branch or tag, use the `CLAILS_BRANCH` variable (defaults to develop if not specified).

```bash
# branch
CLAILS_BRANCH=release/0.0.2 make build

# tag
CLAILS_BRANCH=v0.0.1 make build
```

Once the image build is complete, the development environment is ready.

#### 2.2. Start Docker Container

```bash
make up
```

The container starts in the background.

#### 2.3. Create the Database

In Docker environment, use `make` commands to perform database operations.

```bash
make db.create
```

By default, a SQLite3 database is created.
To use MySQL or PostgreSQL, specify the `--database` option when creating the project.

**Available Make Commands:**
- `make build` - Build Docker image
- `make rebuild` - Rebuild Docker image without cache
- `make up` - Start containers
- `make down` - Stop containers
- `make console` - Start shell inside container
- `make logs` - Show application logs
- `make db.create` - Create database
- `make db.migrate` - Run migrations
- `make db.rollback` - Rollback migrations
- `make db.seed` - Seed database

### Using Local Environment

Create the database using the `clails db:create` command.

```bash
clails db:create
```

By default, a SQLite3 database is created.
To use MySQL or PostgreSQL, specify the `--database` option when creating the project.

---

## 3. Generate Scaffold

Generate Model, View, and Controller files at once using the `clails generate:scaffold` command.

### Using Docker Environment

Start a shell inside the container and run the command:

```bash
make console
# Inside the container
clails generate:scaffold todo
exit
```

### Using Local Environment

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
  (:up #'(lambda (connection)
           (create-table connection :table "todo"
                                    :columns '(("title" :type :string
                                                        :not-null T)
                                               ("done" :type :boolean
                                                       :default NIL)
                                               ("done-at" :type :datetime))))
   :down #'(lambda (connection)
             (drop-table connection :table "todo"))))
```

This table definition includes:
- `title` - TODO title (required)
- `done` - Completion flag (default: false)
- `done_at` - Completion timestamp

---

## 5. Run the Migration

After modifying the migration file, create the table using the `clails db:migrate` command.

### Using Docker Environment

```bash
make db.migrate
```

### Using Local Environment

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
  (:use #:cl
        #:clails/model)
  (:import-from #:clails/datetime
                #:from-universal-time
                #:format-datetime)
  (:import-from #:local-time
                #:now)
  (:export #:<todo>
           #:find-all
           #:create-todo
           #:find-by-id
           #:mark-as-done
           #:format-done-at))

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


(defmethod format-done-at ((todo <todo>))
  "Format the done-at timestamp of a todo item.

   @param todo [<todo>] Todo record
   @return [string] Formatted timestamp in MySQL format (yyyy-mm-dd hh:mm:ss)
   @return [nil] NIL if done-at is not set
   "
  (let ((done-at (ref todo :done-at)))
    (when done-at
      (format-datetime (from-universal-time done-at)
                       :format :mysql))))
```

This code implements the following functionality:
- `find-all` - Retrieve all TODO items
- `create-todo` - Create a new TODO item
- `find-by-id` - Find a TODO by ID
- `mark-as-done` - Mark a TODO as completed
- `format-done-at` - Format the TODO's DONE-AT timestamp in MySQL format (yyyy-mm-dd hh:mm:ss) if set

---

## 7. Modify the View

Open `app/views/todo/package.lisp` and declare the methods used in the View.

```lisp
(in-package #:cl-user)
(defpackage #:todoapp/views/todo/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:todoapp/models/todo
                #:format-done-at))

(in-package #:todoapp/views/todo/package)
```

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
                <td><%= (or (format-done-at todo) "-") %></td>
                <td>
                    <cl:unless test="(clails/model:ref todo :done)">
                    <form action="/todo" method="POST" style="display:inline;">
                        <input type="hidden" name="_method" value="PUT">
                        <input type="hidden" name="id" value="<%= (clails/model:ref todo :id) %>">
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

### Using Docker Environment

In Docker environment, the server automatically starts when you run `make up`.

```bash
make up
```

To check server logs:

```bash
make logs
```

To stop the container:

```bash
make down
```

### Using Local Environment

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

### Using Docker Environment

```bash
make db.seed
```

### Using Local Environment

```bash
clails db:seed
```

---

## Summary

In this QuickStart, you learned how to create a TODO application using clails.

### Main steps using Docker environment:
1. Create a project (`clails new`)
2. Build Docker image (`make build`)
3. Start Docker container (`make up`)
4. Create database (`make db.create`)
5. Generate scaffold (inside container: `clails generate:scaffold`)
6. Edit migration file
7. Run migration (`make db.migrate`)
8. Implement Model
9. Implement View
10. Implement Controller
11. Start server (automatically started with `make up`)

### Main steps using local environment:
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

## Benefits of Using Docker Environment

- Easy setup (just need Docker)
- Entire team can share the same environment
- Database (MySQL or PostgreSQL) is automatically set up
- Doesn't pollute the host environment
- `Makefile` makes commonly used commands easy to execute

## Next Steps

- [Model Guide](model.md) - Database operations and query details
- [View Guide](view.md) - Template engine details
- [Controller Guide](controller.md) - Request handling and routing details
- [Testing Guide](testing.md) - How to write tests
