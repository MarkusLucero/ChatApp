-module(database_master).

-export([start/0]).

-define(NUM_OF_WORKERS, 1).
-define(NUM_OF_INSERTS, 10).

init() ->
    maps:new().

-spec start() -> Master when
      Master :: pid().

start() ->
    database_api:start(),
    Master = spawn(fun() -> loop(?NUM_OF_WORKERS, 0) end),
    
    [database_workers:start(Master, ?NUM_OF_INSERTS, ID) || ID <- lists:seq(1, ?NUM_OF_WORKERS)],
    
    Master.


loop(0, Inserts) ->    
    database_api:stop(),
    io:format("DONE ~w~n", [Inserts]);
loop(CountDown, Inserts) ->
    receive
	{insert, InsertsLeft, PID} ->
	    case InsertsLeft of
		0 ->
		    PID ! kill,
		    loop(CountDown-1, Inserts+1);
		_ ->
		    
		    PID ! continue,
		    loop(CountDown, Inserts+1)
	    end
    end.
		    
