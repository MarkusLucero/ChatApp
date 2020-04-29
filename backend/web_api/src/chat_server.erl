-module(chat_server).
-export([new_connection/1, start/0, register_user/4, send_message/5, get_unread_messages/3, login_user/3, send_friend_request/2, send_chat/3]).

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

register_user(Username, Password, _, _PID) ->
    Hashed_Password =  password_utils:hash_password(Password),
    database_api:insert_user(Username, Hashed_Password, "2020-05-05 16:00:00"),
    %chat_server ! {register_user, Username, PID},
    ok.

-spec login_user(Username, Magic_token, PID) -> ok when
      Username :: list(Integer),
      Magic_token :: list(Integer),
      PID :: pid().
%% @doc Logs a user in to the system on the current API node
%% @param Username The username for the user
%% @param Magic_token The magic token given by the authentication handler
%% @param PID The PID for the websocket handler
%% @returns ok For every login.
login_user(Username, Magic_token, PID) ->
    DMs = database_api:fetch_DMs(Username),
    FriendList = database_api:fetch_friends(Username),
    chat_server ! {login_user, Username, Magic_token, DMs, FriendList, PID},
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
    %database_api:insert_chat(From_Username, Chat_ID, {Timestamp, Message}, 1),
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

-spec send_friend_request(Username, Friendname) -> ok when
      Username :: list(Integer),
      Friendname :: list(Integer).
%% @doc Sends a friend request to a given user
%% @param Username The username of the user that sends the friend request
%% @param Friendname The name of the user that receives the friend request
%% @returns ok.
send_friend_request(Username, Friendname) ->
    database_api:insert_friend(Username, Friendname),
    database_api:insert_friend(Friendname, Username),
    chat_server ! {friend_request, Username, Friendname},
    ok.

%% @doc Generates a unique chat id
%% @returns A chat id
%% TODO: Better rand function?
%% TODO: Make sure that chat id is unique
%% create_chat_id() ->
%%     rand:uniform(1000000).

-spec send_chat(Chat_Name, Creator, Members) -> ok when
      Chat_Name :: list(Integer),
      Creator :: list(Integer),
      Members :: list(Integer).
%% @doc Creates a new chat for a given number of users
%% @param Chat_Name The name of the chat
%% @param Creator The creator of the chat
%% @param Members The members of the chat
%% @returns ok.
%% TODO: Save the chat id in database
send_chat(Chat_Name, Creator, Members) ->
    Chat = database_api:create_chat(Chat_Name, Creator, Members),
    case Chat of
	{error, Reason} ->
	    %% TODO: Fix error handeling
	    {error, Reason};
	{ok, Chat_ID} ->
	    %%[database_api:insert_chat_id(Username) || Username <- Members],
	    chat_server ! {chat_request, Chat_Name, Chat_ID, Creator, Members},
	    ok
    end.
    

-spec start() -> ok.
%% @doc Starts a Web API node and registers the central process to the name chat_server
%% @returns ok For every start.
start() ->
    crypto:rand_seed_s(),
    database_api:start(),
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
        {login_user, Username, Magic_Token, DMs, FriendList, PID} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "init_login"},
                                      {"user_id", Username},
                                      {"list_of_dms", DMs},
                                      {"list_of_friends", FriendList}]}),
            PID ! {text, JSON_Message},
            loop(maps:put(Username, {PID, Magic_Token}, Connection_map));
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
            end;
        {friend_request, Username, Friendname} ->
            JSON_Message = mochijson:encode(
                             {struct, [{"action", "friend_request"},
                                       {"status", "ok"},
                                       {"friend", Username}]}),
            {ok, Friend_PID} = maps:find(Friendname, Connection_map),
            Friend_PID ! {text, JSON_Message},
            loop(Connection_map);
        {chat_request, Chat_Name, Chat_ID, Creator, Members} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "chat_request"},
                                      {"status", "ok"},
                                      {"chat_name", Chat_Name},
                                      {"chat_id", Chat_ID},
                                      {"members", Members},
                                      {"creator", Creator}]}),
            Member_PIDs = [maps:find(Username, Connection_map) || Username <- Members],
            [PID ! {text, JSON_Message} || {ok, PID} <- Member_PIDs],
            loop(Connection_map)
            
    end.
