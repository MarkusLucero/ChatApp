ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)
all: $(OBJECTS)
ebin/%.beam : src/%.erl $(HEADERS) Makefile
	erlc $(ERLC_FLAGS) -o ebin/ $<
clean:
	-rm $(OBJECTS)

test_database_api:
	erl -pa ebin -eval 'eunit:test("ebin/database_api.beam",[verbose])' -s init stop

test_workers:
	erl -noshell -pa ebin -eval 'database_master:start(1, 10)' -s init stop
