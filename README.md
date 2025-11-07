# clails

web framework inspired by Ruby on Rails

# requirement

- roswell
- qlot
- sbcl 

# install

clails is alpha version. so you will need to manually configure various settings.

```bash
git clone https://github.com/tamurashingo/clails.git
cd clails
qlot install
qlot exec ros install ./roswell/clails.ros
export PATH=$PWD/.qlot/bin:$PATH
```

# configuration

clails uses qlot to manage dependencies, including libraries not registered in Quicklisp and to reference the latest code.

At runtime, clails creates a `qlfile` in the directory specified by `CLAILS_CONF_DIR` if one does not exist.
If `CLAILS_CONF_DIR` is not specified, it uses `$HOME/.clails`.

This qlfile contains the necessary dependencies for clails to function properly.

## development with local clails

When developing clails locally and testing your changes, you can configure clails to use your local version by modifying `$CLAILS_CONF_DIR/qlfile`.

Update the qlfile to specify the local path to your clails repository:

```text
github fukamachi/cl-dbi
github tamurashingo/cl-dbi-connection-pool
github tamurashingo/getcmd
local clails /path/to/clails
```

Additionally, set the `CL_SOURCE_REGISTRY` environment variable to ensure the local clails is loaded:

```bash
export CL_SOURCE_REGISTRY=/path/to/clails
```

This allows you to test local modifications to clails without reinstalling.

# usage

## create project

```bash
clails new project-name
```

## create new database

execute `create database` command

```bash
clails db:create
```
## migrate database

```bash
clails db:migrate:up
```
## startup server

```bash
clails server
```

visit `http://localhost:5000/`

you'll see

![clails initial page](document/img/startup.png)

# documentation

Comprehensive guides for developing with clails:

- **[Model Guide](document/model.md)** ([日本語](document/model_ja.md)) - Database models, queries, migrations, and ORM features
- **[View Guide](document/view.md)** ([日本語](document/view_ja.md)) - Template engine, data binding, and HTML rendering
- **[Controller Guide](document/controller.md)** ([日本語](document/controller_ja.md)) - Request handling, routing, and REST controllers
- **[Command Guide](document/command.md)** ([日本語](document/command_ja.md)) - CLI commands for scaffolding and project management
- **[Environment Guide](document/environment.md)** ([日本語](document/environment_ja.md)) - Configuration, environment variables, and setup
- **[Testing Guide](document/testing.md)** ([日本語](document/testing_ja.md)) - Testing framework and best practices

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
- [x] delete
- [x] select from many tables

**view**
- [x] template-based HTML rendering
- [x] JSON rendering

**controller**
- [x] accept query parameter
- [x] accept path parameter
- [x] rest controller


---
Copyright 2024-2025 tamura shingo
