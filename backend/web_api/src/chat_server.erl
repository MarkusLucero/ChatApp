-module(chat_server).
-export([start/0, stop/0, add/1, get/0, add_user/0]).

start() ->
    PID = spawn(fun() -> loop([], maps:new()) end),
    case whereis(chat) of
        undefined -> register(chat, PID);
        _ -> unregister(chat), register(chat, PID)
    end.

stop() ->
    chat ! stop.

add(Text) ->
    chat ! {add, Text}.

add_user() ->
    chat ! {adduser, self()}.

get() ->
    chat ! {get, self()},
    receive
        List ->
            List
    end.

loop(List, UserMap) ->
    io:format("~p | ~p\n", [UserMap, List]),
    receive
        {adduser, PID} ->
            NewMap = maps:put(PID, 0, UserMap),
            loop(List, NewMap);
        {add, String} ->
            maps:map(fun(PID, _) -> PID ! {text, String} end, UserMap),
            loop([String | List], UserMap);
        {get, From} ->
            From ! List,
            loop(List, UserMap);
        stop ->
            ok
    end.
