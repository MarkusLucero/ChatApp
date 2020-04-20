PROJECT = web_api
PROJECT_DESCRIPTION = A web api for Chat Up!
PROJECT_VERSION = 0.1.0
ERLC_OPTS = -Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard -I ../database_api/ebin

LOCAL_DEPS = odbc
DEPS = cowboy
dep_cowboy_commit = 2.6.3

DEP_PLUGINS = cowboy

include erlang.mk

## Everything below are temporary until we find a better solution to build the database_api alongside with the web_api
SOURCES=$(wildcard ./../database_api/src/*.erl)
HEADERS=$(wildcard ./../database_api/src/*.hrl)
OBJECTS=$(SOURCES: ./../database_api/src/%.erl=ebin/%.beam)

.PHONY: database

ebin/%.beam : ./../database_api/src/%.erl $(HEADERS) Makefile
	erlc -o ebin/ $<

database: $(OBJECTS)
	cp ./../database_api/src/* ./src/
