# clails-cli

## create project

`create-project` creates project name's package and some commands.


### Syntax

```lisp
(clails-cli:create-project name &key database project-path)
```

### Arguments and Values

- name -- string
- database -- `sqlite3` , `mysql` , `postgresql`. default is `sqlite3`
- paroject-path -- where to create the project. default is current directory

### Examples

```common-lisp
(clails-cli:craete-project "todoapp" :database :mysql :project-path #P"/project")
```


## create database

### Syntax

```common-lisp
('project-name'-cli:db/create &key env)
```

### Arguments and Values

- env -- `:development`, `:test`, `:production`. default is `:development` and `:test`


### Examples

```common-lisp
(todoapp-cli:db/create)
```


## create model

### Syntax

```lisp
('project-name'-cli:generate/model name body &key no-migration)
```

### Arguments and Values

- name -- string
- body -- list
- no-migration -- boolean. if T generate model only

### Examples

```lisp
(todoapp-cli:generate/model "todo" '((title :type :string
                                            :not-null T)
                                      (done :type :boolean
                                            :not-null T)))
```

## create migration file

### Syntax

```lisp
('project-name'-cli:generate/migration name &key type body)
```

### Arguments and Values

- name -- string
- type -- `:create` or `:add-column`
- body -- list


Examples

```lisp
(todoapp-cli:generate-migration "todo" :type :add-column
                                       :body '((done-at :type :datetime)))
```


