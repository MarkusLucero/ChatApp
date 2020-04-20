ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(OBJECTS) test

ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<
clean:
	-rm $(OBJECTS)
test:
	erl -noshell -pa ebin -eval 'eunit:test("ebin/database_api.beam",[verbose])' -s init stop

.PHONY: doc

doc:
	erl -noshell -pa ebin -eval 'edoc:files(["src/database_api.erl"], [{dir, doc}])' -s init stop

test_workers:
	erl -noshell -pa ebin -eval 'database_master:start(1, 10)' -s init stop
