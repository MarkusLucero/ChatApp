-module(database_master).

-export([start/2]).

init() ->
    maps:new().

-spec start(NumWorkers, NumInserts) -> Master when
      NumWorkers :: number(),
      NumInserts :: number(),
      Master :: pid().

start(NumWorkers, NumInserts) ->
    database_api:start(),
    Master = spawn(fun() -> loop(NumWorkers, 0) end),
    
    [database_workers:start(Master, NumInserts, ID) || ID <- lists:seq(1, NumWorkers)],
    
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
		    
