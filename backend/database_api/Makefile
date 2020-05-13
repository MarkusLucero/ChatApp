ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(OBJECTS)

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<
clean:
	-rm $(OBJECTS)
test:	all
	erl -noshell -pa ebin -eval 'eunit:test("ebin/database_api.beam",[verbose])' -s init stop	

.PHONY: doc

doc:
	erl -noshell -pa ebin -eval 'edoc:files(["src/database_api.erl", "src/database_master.erl", "src/database_workers.erl"], [{dir, doc}])' -s init stop

rebuild_database:
	psql -d adrenaline_db -a -f postgres/Setup.sql

# test_workers:
# 	erl -noshell -pa ebin -eval 'database_master:start(1000, 10)' -s init stop
# 	make clean
