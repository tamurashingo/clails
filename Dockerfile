FROM fukamachi/sbcl

RUN apt-get update && apt-get install -y \
  default-libmysqlclient-dev \
  libpq-dev \
  libsqlite3-dev \
  default-mysql-client \
  postgresql-client \
  sqlite3

RUN ros run -e "(ql-dist:install-dist \"http://dist.shirakumo.org/shirakumo.txt\" :prompt nil)"
RUN ros install qlot rove

ENV PATH=${PATH}:/root/.roswell/bin

RUN mkdir /app

RUN mkdir -p /qlot/volumes
WORKDIR /qlot
COPY ./qlfile /qlot
RUN echo 'local clails /app' >> /qlot/qlfile

ENTRYPOINT /bin/bash
