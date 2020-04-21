-module(chat_server).
-export([new_connection/1, start/0, register_user/4, send_message/5, get_unread_messages/3, login_user/3]).


new_connection(_PID) ->
    ok.

register_user(Username, _Password, _, PID) ->
    database_api:insert_user(Username, _Password, "2020-05-05 16:00:00"),
    chat_server ! {login_user, Username, PID}.

login_user(Username, _Password, PID) ->
    %%TODO: Add actual password check
    chat_server ! {login_user, Username, PID}.

send_message(From_Username, Chat_ID, Message, Timestamp, PID) ->
    %%TODO: Check if we can actually deliver
    chat_members(Chat_ID),
    %user_status("TODO: Check with real users"),
    %%TODO: Check if we can actually deliver
    database_api:insert_chat(From_Username, Chat_ID, delivered),
    chat_server ! {send_message, From_Username, Chat_ID, Message, Timestamp, PID}.


get_unread_messages(_Username, _Chat_ID, _PID) ->
    tbi.

chat_members(_Chat_ID) ->
    tbi.

%user_status(Username) ->
%    chat_server ! {user_status_request, Username, self()},
%    receive
%        {user_status_response, online} ->
%            delivered;
%        {user_status_response, offline} ->
%            undelivered;
%                _ ->
%            erlang:error('user does not exist')
%    end.

start() ->
    database_api:start(),
    case whereis(chat_server) of
        undefined -> 
            ok;
        _PID -> 
            unregister(chat_server)
    end,
    register(chat_server, spawn(fun() -> loop(maps:new()) end)).


loop(Connection_map) ->
    io:format("Connections: ~p~n", [Connection_map]),
    receive
        {login_user, Username, PID} ->
            loop(maps:put(Username, PID, Connection_map));
        {send_message, From_Username, Chat_ID, Message, Timestamp, PID} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "send_message"},
                                      {"chat_id", Chat_ID},
                                      {"user_id", From_Username},
                                      {"message", Message},
                                      {"timestamp", Timestamp}]}),
            Send = 
                fun(_Username, Connected_PID) -> 
                        case Connected_PID of 
                            PID -> 
                                ok;
                            _ ->
                                io:format("Sending to: ~p~n", [Connected_PID]),
                                Connected_PID ! {text, JSON_Message},
                                ok
                        end
                end,
            maps:map(Send, Connection_map),
            loop(Connection_map);
        {user_status_request, Username, From} ->
            case maps:is_key(Username) of
                true -> From ! {user_status_response, online};
                _ -> From ! {user_status_response, offline}
            end
    end.
