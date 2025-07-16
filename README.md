# clails
framework?

## usage (repl)

### create project

```lisp
(clails:create-project "todoapp")
```


### load project

```lisp
(ql:quickload :todoapp)
```


### create database


```lisp
(clails:db/create)
```

### create model

```lisp
(clails:generate/model "todo")
```


edit migration file: db/migrate/yyyymmddhhmmss_todo.lisp

```lisp
(in-package #:todoapp/model/db)

(defmigration "yyyymmddhhmmss_todo"
  (:up #'(lambda (connection)
           (create-table connection :table "todo"
                                    :columns '(("title" :type :string
                                                        :not-null T)
                                               ("done" :type :boolean
                                                       :default-value nil)))
           (add-index connection :table "todo"
                                 :index "idx-title"
                                 :columns '("title")))
   :down #'(lambda (connection)
             (drop-table connection :table "todo"))))
```

### apply migration


```lisp
(clails:db/migrate)
```


## usage (cli)

Environment variables must be set at runtime.

Run the following command in the `clails` directory.

```bash
export CL_SOURCE_REGISTRY=$PWD
```


### create project

```bash
clails.ros new todoapp --path /cl-projects
```

### create database

in project directory,

```bash
clails.ros db create
```

### create model

```bash
clails.ros generate model todo
```

edit migration file: db/migrate/yyyymmddhhmmss_todo.lisp

```lisp
(in-package #:todoapp/model/db)

(defmigration "yyyymmddhhmmss_todo"
  (:up #'(lambda (connection)
           (create-table connection :table "todo"
                                    :columns '(("title" :type :string
                                                        :not-null T)
                                               ("done" :type :boolean
                                                       :default-value nil)))
           (add-index connection :table "todo"
                                 :index "idx-title"
                                 :columns '("title")))
   :down #'(lambda (connection)
             (drop-table connection :table "todo"))))
```


### apply migration

```bash
clails.ros db migrate up
```


## how to develop

Run setup only once.

```sh
make setup
```

### develop clails

Startup components.

```sh
make dev.up
```


Connect slime.
(with Emacs)

```
M-x slime-connect 127.0.0.1 4005
```


Set up Quicklisp

```lisp
(push #P"/app" ql:*local-project-directories*)
```


Load `clails`

```lisp
(ql:quickload :clails)
```


When it's finished, shutdown components.


```sh
make dev.down
```


### test clails


Run test.

```sh
make test
```


Shutdown components.

```sh
make test.down
```

If you want to connect to the database inside the container after the test is finished, type this

```sh
make console.test
```

and then run script

```sh
./script/conn-mysql.sh

# or

./script/conn-postgresql.sh
```

password is `password`


---
Copyright 2024 tamura shingo
