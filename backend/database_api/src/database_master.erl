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
    Master = spawn_link(fun() -> loop(NumWorkers, 0) end),
    [database_workers:start(Master, NumInserts, ID) || ID <- lists:seq(1, NumWorkers)],
    receive
	    {'EXIT', Master, _} ->
	    timer:sleep(NumWorkers*200),
	    database_api:stop()
    end.


loop(0, Inserts) ->
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
		    
