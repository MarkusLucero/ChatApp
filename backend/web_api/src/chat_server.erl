-module(chat_server).
-export([start/0, stop/0, add/1, get/0]).

start() ->
    PID = spawn(fun() -> loop([]) end),
    case whereis(chat) of
        undefined -> register(chat, PID);
        _ -> unregister(chat), register(chat, PID)
    end.

stop() ->
    chat ! stop.

add(Text) ->
    chat ! {add, Text}.

get() ->
    chat ! {get, self()},
    receive
        List ->
            List
    end.

loop(List) ->
    io:format("~p", [List]),
    receive
        {add, String} ->
            loop([String | List]);
        {get, From} ->
            From ! List,
            loop(List);
        stop ->
            ok
    end.
