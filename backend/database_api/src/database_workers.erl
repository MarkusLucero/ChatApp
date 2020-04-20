-module(database_workers).

-export([start/3]).

-spec start(Master, NumberInserts, ID) -> Worker when
      Master :: pid(),
      NumberInserts :: number(),
      ID :: number(),
      Worker :: pid().

start(Master, NumberInserts, ID) ->
    database_api:insert_user("testuser1", "2020-20-19 01:00:00"),
    Worker = spawn(fun() -> loop(NumberInserts, "testuser1", Master) end),
    
    Worker.



stop() ->
    ok.
loop(InsertsLeft, Username, Master) ->
    database_api:insert_chat(Username, "chat_id_1", {"2020-20-19 02:00:00", "Hejsan svejsan"}, 1),
    NewInsertsLeft = InsertsLeft - 1,
    Master ! {insert, NewInsertsLeft, self()},
    receive
	continue ->
	    loop(NewInsertsLeft, Username, Master);
	kill ->
	    stop()
    end.
