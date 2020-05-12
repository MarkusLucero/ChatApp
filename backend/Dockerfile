FROM postgres:12-alpine

# Install up-to-date version of psqlodbc
RUN apk add make unixodbc unixodbc-dev build-base curl tar postgresql-dev

RUN set -e \
&& mkdir /psqlodbc \
&& cd /psqlodbc \
&& curl https://ftp.postgresql.org/pub/odbc/versions/src/psqlodbc-12.01.0000.tar.gz > psqlodbc-12.01.0000.tar.gz \
&& tar -xf psqlodbc-12.01.0000.tar.gz \
&& cd psqlodbc-12.01.0000 \
&& ./configure --prefix=/usr \
&& make \
&& make install


# Install erlang application and dependencies
RUN apk add erlang git erlang-dev erlang-odbc erlang-eunit erlang-ssl erlang-sasl erlang-crypto erlang-runtime-tools

COPY ./database_api /database_api
COPY ./web_api /web_api

RUN set -e \
&& cd /web_api \
&& make clean \
&& make distclean \
&& make

COPY ./docker_files/start_app /bin/start_app
RUN chmod +x /bin/start_app

EXPOSE 8080


# Configure database for erlang application
COPY ./docker_files/Setup.sql /docker-entrypoint-initdb.d
COPY ./docker_files/odbc.ini /etc/odbc.ini
COPY ./docker_files/odbcinst.ini /etc/odbcinst.ini

ENV POSTGRES_USER adrenaline
ENV POSTGRES_PASSWORD 1234
ENV POSTGRES_DB adrenaline_db


# Start application
# NOTE: `postgres` cannot run as pid=1 in the same container as erlang without crashing.
#       (no idea why... it just is that way)
#       `tini` is a very simple init-system used to work around this.
RUN apk add tini
ENTRYPOINT ["tini", "--", "docker-entrypoint.sh"]
CMD ["postgres"]
