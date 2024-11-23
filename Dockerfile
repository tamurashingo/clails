FROM fukamachi/sbcl

RUN apt-get update && apt-get install -y \
  default-libmysqlclient-dev \
  libpq-dev \
  libsqlite3-dev \
  default-mysql-client \
  postgresql-client

RUN ros run -e "(ql-dist:install-dist \"http://dist.shirakumo.org/shirakumo.txt\" :prompt nil)"
RUN ros install qlot rove

ENV PATH=${PATH}:/root/.roswell/bin

RUN mkdir /app
WORKDIR /app
COPY . /app

ENTRYPOINT ros run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :dont-close t :style :spawn)"
