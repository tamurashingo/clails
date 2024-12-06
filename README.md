# clails
framework?

## usage

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

### apply migration


```lisp
(clails:db/migrate)
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
