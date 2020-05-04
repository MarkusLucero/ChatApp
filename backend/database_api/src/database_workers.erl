%% @doc This module is used together with database_master to stress test the database_api module.
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
    X = rand:uniform(1000000),
    Username = "testuser" ++ integer_to_list(X),
    Chat_Name = "chat" ++ integer_to_list(X),
    database_api:insert_user(Username, "12345", "2020-10-19 01:00:00"),
    Chat_ID = database_api:create_chat(Chat_Name, Username, [Username]),
    Worker = spawn(fun() -> loop(NumInserts, Username, Chat_ID, Master) end),
    Worker.



stop() ->
    ok.

loop(InsertsLeft, Username, Chat_ID, Master) ->
    database_api:insert_chat(Username, Chat_ID, {"2020-10-19 02:00:00", "Hejsan svejsan"}, 1),
    NewInsertsLeft = InsertsLeft - 1,
    Master ! {insert, NewInsertsLeft, self()},
    receive
	continue ->
	    loop(NewInsertsLeft, Username, Chat_ID, Master);
	kill ->
	    stop()
    end.
