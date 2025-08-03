# clails

web framework inspired by Ruby on Rails

# requirement

- roswell
- qlot
- sbcl 

# install

clails is alpha version. so you will need to manually configure various settings.


**clone clails**

```bash
git clone https://github.com/tamurashingo/clails.git
```

**install dependencies**

```bash
cd clails
qlot install
```

**add environment variables**

path

```bash
export PATH=$PATH:$PWD/roswell
```

asdf's source path

```bash
export CL_SOURCE_REGISTRY=$PWD
```

# usage

## create project

```bash
clails.ros new project-name
```

### options

- `-p` <em>pathname</em> | `--path` <em>pathname</em>
    - set root directory when creating a project

- `-d` <em>database-type</em> | `--database` <em>database-type</em>
    - set database type
    - database types are
         - `sqlite3`
         - `mysql`
         - `postgresql`

## generate scaffold

generate model, view, controller and migration file.

```bash
clails.ros generate scaffold todo
```

generates

- `app/models/todo.lisp`
- `app/views/todo/show.html`
- `app/views/todo/new.html`
- `app/views/todo/edit.html`
- `app/views/todo/delete.html`
- `app/controllers/todo-controller.lisp`
- `db/migrate/yyyymmddhhmmss_todo.lisp`

## generate model, view, controller and migration

```bash
clails.ros generate model todo
clails.ros generate view todo
clails.ros generate controller todo
clails.ros generate migration todo
```

here is the command to generate all of the above files at once.

```bash
clails.ros generate scaffold todo
```

### options

- `--no-overwrite`
    - if file already exists, stop generateing.


- `-n` | `--no-migration`
    - when generating model, generate no migration files.


## create new database

execute `create database` command

```bash
clails.ros db create
```


## migrate database

```bash
clails.ros db migrate up
```

## startup server

```bash
clails.ros server
```

visit `http://localhost:5000/`

you'll see

![clails initial page](document/img/startup.png)

# example

see: https://github.com/tamurashingo/clails/wiki/create-todo-application

---
Copyright 2024-2025 tamura shingo
