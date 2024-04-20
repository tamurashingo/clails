# clails
framework?

## how to develop

create docker image.

```sh
make setup
```

run swank server on docker.

```sh
make dev.up
```

connect swank server from emacs.

```emacs
M-x slime-connect
Host: 127.0.0.1
Port: 4005
```


stop swank server

```sh
make dev.down
```

