FROM fukamachi/sbcl

RUN apt-get update

RUN mkdir /app
WORKDIR /app
COPY . /app

COPY ./script/entrypoint.dev.sh /usr/bin/
RUN chmod +x /usr/bin/entrypoint.dev.sh
ENTRYPOINT ["entrypoint.dev.sh"]

EXPOSE 8000
EXPOSE 4005

