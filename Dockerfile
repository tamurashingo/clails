FROM fukamachi/sbcl:2.5.10

RUN apt-get update && apt-get install -y \
  default-libmysqlclient-dev \
  libpq-dev \
  libsqlite3-dev \
  default-mysql-client \
  postgresql-client \
  sqlite3 \
  && rm -rf /var/lib/apt/lists/*

RUN ros run -e "(ql-dist:install-dist \"http://dist.shirakumo.org/shirakumo.txt\" :prompt nil)"
RUN ros install rove
RUN ros install fukamachi/cl-dbi
RUN ros install tamurashingo/cl-dbi-connection-pool
RUN ros install tamurashingo/cl-batis
RUN ros install tamurashingo/getcmd


ENV PATH=${PATH}:/root/.roswell/bin
ENV CL_SOURCE_REGISTRY=/app

RUN mkdir /app

RUN mkdir /volumes

ENTRYPOINT ros run -e "(ql:quickload :swank)" -e "(setf swank::*loopback-interface* \"0.0.0.0\")" -e "(swank:create-server :port 4005 :dont-close t :style :spawn)"
