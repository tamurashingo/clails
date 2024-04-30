# clails-entity


## create entity class and migration file

in REPL

```lisp
(clails-entity:generate-entity todo
  ((title :type string
          :not-null T
          :primary-key T
          :auto-increment T)
   (done :type boolean)))
```

generate entity,

```lisp
(defclass todo (clails-entity:base-entity))
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
(clails-entity:mod-entity todo :type :add-column
  ((created-at :type datetime)
   (updated-at :type datetime)))
```

generate migration file.

```lisp
(add-column todo
  ((created-at :type datetime)
   (updated-at :type datetime)))
```

