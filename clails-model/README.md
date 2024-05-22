# clails-model


## create model class and migration file

in REPL

```lisp
(clails-model:generate-model todo
  ((title :type string
          :not-null T
          :primary-key T
          :auto-increment T)
   (done :type boolean)))
```

generate model,

```lisp
(defclass todo (clails-model:<base-model>))
```


and generate migration file.

```lisp
(in-package #:cl-user)
(defpackage

(create-table todo
  ((title :type string
          :not-null T
          :primary-key T
          :auto-increment T)
   (done :type boolean)))
```



in REPL

```lisp
(clails-model:mod-model todo :type :add-column
  ((done-at :type datetime)))
```

generate migration file.

```lisp
(add-column todo
  ((done-at :type datetime)))
```


## query

```lisp
(select 'todo')
=> "SELECT ID, CREATED_AT, UPDATED_AT, TITLE, DONE_AT FROM TODO"
#()

(select 'todo :where '(= id 1))
=> "SELECT ID, CREATED_AT, UPDATED_AT, TITLE, DONE_AT FROM TODO WHERE (ID = ?)"
#(1)

(setq begin "2024/04/01 00:00:00")
(setq end "2024/04/30 23:59:59")
(select 'todo :where `(and (= done true)
                           (>= done-at ,begin)
                           (<= done-at ,end)))
=> "SELECT ID, CREATED_AT, UPDATED_AT, TITLE, DONE_AT FROM TODO WHERE (DONE = TRUE AND DONE_AT >= ? AND DONE_AT <= ?)"
#("2024/04/01 00:00:00" "2024/04/30 23:59:59")
```

## update

...

## transaction

...

