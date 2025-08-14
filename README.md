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

## create new database

execute `create database` command

```bash
clails.ros db:create
```
## migrate database

```bash
clails.ros db:migrate:up
```
## startup server

```bash
clails.ros server
```

visit `http://localhost:5000/`

you'll see

![clails initial page](document/img/startup.png)

# example

It contains instructions on how to create an application using commands:\
https://github.com/tamurashingo/clails/wiki/create-todo-application

Implementation examples for various databases, Docker, and REST:\
https://github.com/tamurashingo/clails-sample-apps

# feature
**model**
- [x] select from single table
- [x] update
- [x] insert
- [ ] delete
- [ ] select from many tables

**view**
- [x] template-based HTML rendering
- [x] JSON rendering

**controller**
- [x] accept query parameter
- [x] accept path parameter
- [x] rest controller


---
Copyright 2024-2025 tamura shingo
