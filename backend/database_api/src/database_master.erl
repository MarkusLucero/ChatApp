%% @doc This module is used together with database_workers to stress test the database_api module.
-module(database_master).

-export([start/2]).


-spec start(NumWorkers, NumInserts) -> Master when
      NumWorkers :: number(),
      NumInserts :: number(),
      Master :: pid().
%% @doc Creates a Master process and a number of workers that will insert messages into a database.
%% @param NumWorkers The number of workers that will be created.
%% @param NumInserts The number of inserts each worker will perform to the database.
%% @returns The PID of the created Master.
start(NumWorkers, NumInserts) ->
    database_api:start(),
    Master = spawn_link(fun() -> loop(NumWorkers, 0) end),
    [database_workers:start(Master, NumInserts) || _ <- lists:seq(1, NumWorkers)],
    receive
	    {'EXIT', Master, _} ->
%%	    timer:sleep(NumWorkers*200),
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
		    
