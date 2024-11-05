FROM fukamachi/sbcl

RUN apt-get update && apt-get install -y \
  default-libmysqlclient-dev \
  libpq-dev \
  libsqlite3-dev \
  default-mysql-client \
  postgresql-client

RUN ros install qlot rove

ENV PATH=${PATH}:/root/.roswell/bin

RUN mkdir /app
WORKDIR /app
COPY . /app

