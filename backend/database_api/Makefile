PROJECT = adrenaline_db
PROJECT_DESCRIPTION = A database api for Chat Up!
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = odbc eunit

include erlang.mk

database_rebuild:
	psql -d adrenaline_db -a -f ./../docker_files/Setup.sql
