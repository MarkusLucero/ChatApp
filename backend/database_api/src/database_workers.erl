-module(database_workers).

-export([start/2]).

-spec start(Master, NumberInserts) -> Worker when
      Master :: pid(),
      NumberInserts :: number(),
      Worker :: pid().
%% @doc Creates a worker that inserts messages into a database.
%% @param Master The PID to the caller.
%% @param NumInserts The number of insert messages the worker will perform to the database.
%% @returns The PID of the created worker.
start(Master, NumInserts) ->
    database_api:insert_user("testuser1", "2020-10-19 01:00:00"),
    Worker = spawn(fun() -> loop(NumInserts, "testuser1", Master) end),
    
    Worker.



stop() ->
    ok.

loop(InsertsLeft, Username, Master) ->
    database_api:insert_chat(Username, "chat_id_1", {"2020-10-19 02:00:00", "Hejsan svejsan"}, 1),
    NewInsertsLeft = InsertsLeft - 1,
    Master ! {insert, NewInsertsLeft, self()},
    receive
	continue ->
	    loop(NewInsertsLeft, Username, Master);
	kill ->
	    stop()
    end.
