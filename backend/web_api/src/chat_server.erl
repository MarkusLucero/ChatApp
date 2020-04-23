-module(chat_server).
-export([new_connection/1, start/0, register_user/4, send_message/5, get_unread_messages/3, login_user/3]).

-spec new_connection(PID) -> ok when
      PID :: pid().
%% @doc Registers a new websocket connection to the chat server
%% @param PID The PID for the websocket handler
%% @returns ok For every connection.
new_connection(_PID) ->
    ok.

-spec register_user(Username, Password, Timestamp, PID) -> ok when
      Username :: list(Integer),
      Password :: list(Integer),
      Timestamp :: list(Integer),
      PID :: pid().
%% @doc Registers a new user to the whole system
%% @param Username The username for the new user
%% @param Password The password for the new user
%% @param Timestamp A timestamp for when the registration happened
%% @param PID The PID for the websocket handler
%% @returns ok For every registration.
register_user(Username, _Password, _Timestamp, PID) ->
%    database_api:insert_user(Username, Timestamp),
    chat_server ! {login_user, Username, PID},
    ok.

-spec login_user(Username, Password, PID) -> ok when
      Username :: list(Integer),
      Password :: list(Integer),
      PID :: pid().
%% @doc Logs a user in to the system on the current API node
%% @param Username The username for the user
%% @param Magic_token The magic token given by the authentication handler
%% @param PID The PID for the websocket handler
%% @returns ok For every login.
login_user(Username, _Magic_token, PID) ->
    %%TODO: Add actual password check
    chat_server ! {login_user, Username, PID},
    ok.

-spec send_message(Username, Chat_ID, Message, Timestamp, PID) -> ok when
      Username :: list(Integer),
      Chat_ID :: list(Integer),
      Message :: list(Integer),
      Timestamp :: list(Integer),
      PID :: pid().
%% @doc Handles an already parsed message sent by a user
%% @param From_Username The username for the user that sent the message
%% @param Chat_ID The ID for the destination chat
%% @param Message The message to be sent
%% @param Timestamp The timestamp for when the message was sent
%% @param PID The PID for the websocket handler
%% @returns ok For every message.
send_message(From_Username, Chat_ID, Message, Timestamp, PID) ->
    %%TODO: Check if we can actually deliver
    chat_members(Chat_ID),
    %user_status("TODO: Check with real users"),
    %%TODO: Check if we can actually deliver
%    database_api:insert_chat(From_Username, Chat_ID, delivered),
    chat_server ! {send_message, From_Username, Chat_ID, Message, Timestamp, PID},
    ok.

-spec get_unread_messages(Username, Chat_ID, PID) -> ok when
      Username :: list(Integer),
      Chat_ID :: list(Integer),
      PID :: pid().
%% @doc Gets undread messages from a given chat for a given user
%% @param Username The username for the user
%% @param Chat_ID The chat ID for the requested chat
%% @param PID The PID for the websocket handler
%% @returns tbi.
%% @TODO Implement this
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

-spec start() -> ok.
%% @doc Starts a Web API node and registers the central process to the name chat_server
%% @returns ok For every start.
start() ->
    case whereis(chat_server) of
        undefined -> 
            ok;
        _PID -> 
            unregister(chat_server)
    end,
    register(chat_server, spawn(fun() -> loop(maps:new()) end)),
    ok.


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
