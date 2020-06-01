-module(chat_server).
-export([new_connection/1, start/0, register_user/4, send_message/5, get_unread_messages/3, login_user/3, send_friend_request/2, send_chat/3, logout_user/2, create_thread/4, insert_comment/5, upvote/2, downvote/2]).

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

register_user(Username, Password, Timestamp, _PID) ->
   %% Hashed_Password =  password_utils:hash_password(Password),
    database_api:insert_user(Username, Password, Timestamp),
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
    case database_api:fetch_all_chats(Username) of
        {error, _} ->
            %erlang:error('Error fetching chats');
            DMs = [],
            case database_api:fetch_friendlist(Username) of 
                {error, _} ->
                    %erlang:error('Error fetching friends'),
                    chat_server ! {login_user, Username, Magic_token, DMs, [], PID};
                Friends ->
                    FriendList = [Friendname || {Friendname} <- Friends],
                    chat_server ! {login_user, Username, Magic_token, DMs, FriendList, PID}
            end;
        DMs ->
            ok,
            case database_api:fetch_friendlist(Username) of 
                {error, _} ->
                    %erlang:error('Error fetching friends'),
                    chat_server ! {login_user, Username, Magic_token, DMs, [], PID};
                Friends ->
                    FriendList = [Friendname || {Friendname} <- Friends],
                    chat_server ! {login_user, Username, Magic_token, DMs, FriendList, PID}
            end
    end,
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
    chat_members(Chat_ID),
    io:format("Timestamp on message: ~s~n", [Timestamp]),
    chat_server ! {send_message, From_Username, Chat_ID, Message, Timestamp, PID},
    database_api:insert_chat(From_Username, Chat_ID, {Timestamp, Message}, 1),
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
    %database_api:insert_friend(Username, Friendname),
    %database_api:insert_friend(Friendname, Username),
    chat_server ! {friend_request, Username, Friendname},
    ok.


-spec logout_user(Username, Token) -> ok when
      Username :: list(Integer),
      Token :: list(Integer).
%% @doc Logs a user out if Username and Token are correct
%% @param Username The username of the user that gets logged out
%% @param Token The magic token corresponding to the user's session
%% @returns ok.
logout_user(Username, Token) ->
    chat_server ! {logout_user, Username, Token},
    ok.

-spec send_chat(Chat_Name, Creator, Members) -> ok when
      Chat_Name :: list(Integer),
      Creator :: list(Integer),
      Members :: list(Integer).
%% @doc Creates a new chat for a given number of users
%% @param Chat_Name The name of the chat
%% @param Creator The creator of the chat
%% @param Members The members of the chat
%% @returns ok.
send_chat(Chat_Name, Creator, Members) ->
    Chat_ID = database_api:create_chat(Chat_Name, Creator, Members),
    case Chat_ID of
        {error, Reason} ->
            %% TODO: Fix error handeling
            {error, Reason};
        _ ->
            %%[database_api:insert_chat_id(Username) || Username <- Members],
            chat_server ! {chat_request, Chat_Name, Chat_ID, Creator, Members},
            ok
    end.

-spec create_thread(Server_Name, Username, Root_Header, Root_Comment) -> ok when
      Server_Name :: list(Integer),
      Username :: list(Integer),
      Root_Header :: list(Integer),
      Root_Comment :: list(Integer).
%% @doc Creates a new forum thread
%% @param Server_Name the name of the server in which the thread will be placed
%% @param Username the name of the creator of the thread
%% @param Root_Header the header text of the thread
%% @param Room_Comment the body text of the thread
%% @returns ok
create_thread(Server_Name, Username, Root_Header, Root_Comment) ->
    case database_api:create_thread(Username, Server_Name, Root_Header, Root_Comment) of

        {error, _Reason} ->
            erlang:error('Error creating thread');
        Thread_ID ->
            case database_api:fetch_thread(Thread_ID) of
                {error, _Reason} ->
                    erlang:error('Error fetching thread');
                {Server, Creator, Header, Text, Timestamp, Commentlist} ->
                    chat_server ! {create_thread, Thread_ID, Server, Creator, Header, Text, Timestamp, Commentlist},
                    ok
            end

    end.

-spec insert_comment(Thread_ID, Index, Reply_Index, Username, Comment) -> ok when
      Thread_ID :: list(Integer),
      Index :: list(Integer),
      Reply_Index :: list(Integer),
      Username :: list(Integer),
      Comment :: list(Integer).
%% @doc Inserts a new comment into a thread
%% @param Thread_ID the ID of the thread
%% @param Index the index of the previous comment
%% @param Reply_Index the index of the comment being replied to
%% @param Username the username of the commenter
%% @param Text the content of the comment
%% @returns ok
insert_comment(Thread_ID, Index, Reply_Index, Username, Comment) ->
    case database_api:insert_comment(Thread_ID, Index, Reply_Index, Username, Comment) of
        {error, Reason} ->
            io:format("~n~p~n", [Reason]),
            erlang:error('Error inserting comment');
        {Thread_ID, Username, Comment, Rating, {Reply_User, Reply_Comment}} ->
            chat_server ! {insert_comment, Thread_ID, Username, Comment, Rating, Reply_User, Reply_Comment},
            ok
    end.

-spec upvote(Thread_ID, Index) -> ok when
      Thread_ID :: list(Integer),
      Index :: list(Integer).
%% @doc Upvotes a thread comment
%% @param Thread_ID the ID of the thread where the upvoted comment is located
%% @param Index the index of the upvoted comment in the thread
%% @returns ok
upvote(Thread_ID, Index) ->
    case database_api:upvote(Thread_ID, Index) of
        {error, _Reason} ->
            erlang:error('Error upvoting thread comment');
        Rating ->
            chat_server ! {Thread_ID, Rating, Index},
            ok
    end.

-spec downvote(Thread_ID, Index) -> ok when
      Thread_ID :: list(Integer),
      Index :: list(Integer).
%% @doc Downvotes a thread comment
%% @param Thread_ID the ID of the thread where the downvoted comment is located
%% @param Index the index of the downvoted comment in the thread
%% @returns ok
downvote(Thread_ID, Index) ->
    case database_api:downvote(Thread_ID, Index) of
        {error, _Reason} ->
            erlang:error('Error downvoting thread comment');
        Rating ->
            chat_server ! {Thread_ID, Rating, Index},
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
    auth_handler:start_token_server(),
    ok.

check_token(User, Token) ->
    token_server ! {check_token, Token, User, self()},
    receive
        token_ok ->
            true;
        _ ->
            false
    end.

            

loop(Connection_map) ->
    io:format("Connections: ~p~n", [Connection_map]),
    receive
        {login_user, Username, Magic_Token, DMs, FriendList, PID} ->
            io:format("In login_user loop~n DMs: ~p FriendList: ~p ~n", [DMs, FriendList]),
            case check_token(Username, Magic_Token) of
                true ->
                    io:format("About to encode ~p~n",[DMs]),
                    %% DMs === [{Chat_ID, Chat_Name, [{Sender,  Message}]}]
                    %{"messages", lists:map(fun({Src, Msg}) -> mochijson:encode({struct, [{"message", Msg}, {"username", Src}]}) end, Messages)}
                    Members = fun(ID) -> Members = database_api:fetch_chat_members(ID), [Member || {Member} <- Members] end,
                    List_of_DMs = [{struct, [{"chatName", Chat_Name}, 
                                             {"chatID", Chat_ID},
                                             {"members", {array, Members(Chat_ID)}},
                                             {"messages", {array, [{struct, [{"message", Msg}, {"username", Src}, {"timestamp", lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hr, Mi, Se]))}]} || {Src, Msg, {{Year,Month,Day}, {Hr,Mi,Se}}} <- Messages]}}
                                            ]} || {Chat_ID, Chat_Name, Messages} = _MsgS <- DMs],
                    io:format("MADE IT PAST LIST OF DMs: ~p~w~n~s~n", [List_of_DMs, length(List_of_DMs), FriendList]),
                    JSON_Message = mochijson:encode(
                                     {struct,[{"action", "init_login"},
                                              {"user_id", Username},
                                              {"list_of_dms", {array, List_of_DMs}},
                                              {"list_of_friends", {array, FriendList}},
                                              {"list_of_servers", {array, ["0"]}}]}),
                    io:format("ABOUT TO SEND INIT_LOGIN~n"),
                    PID ! {text, JSON_Message},
                    io:format("SENT INIT_LOGIN~n"),
                    loop(maps:put(Username, {PID, Magic_Token}, Connection_map));
                _ -> 
                    loop(Connection_map)
            end;
        {send_message, From_Username, Chat_ID, Message, Timestamp, PID} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "send_message"},
                                      {"chat_id", Chat_ID},
                                      {"user_id", From_Username},
                                      {"message", Message},
                                      {"timestamp", Timestamp}]}),
            io:format("Message being sent: ~p~n", [JSON_Message]),
            case database_api:fetch_chat_members(Chat_ID) of
                {error, _} -> io:format("FAILED TO FIND CHAT MEMBERS: ~s~n", [Chat_ID]), loop(Connection_map);
                Members -> 
                    io:format("CHAT MEMBERS: ~p~n", [Members]),
                    Is_member = 
                        fun(Username,  {Connected_PID, _}) ->
                                io:format("Checking user ~s ", [Username]),
                                case lists:keyfind(Username, 1, Members) of
                                    false -> io:format("FALSE~n"), false;
                                    _ -> 
                                        case Connected_PID of
                                            PID -> false;
                                            _ -> true
                                        end
                                end
                        end,
                    Member_map = maps:filter(Is_member, Connection_map),
                    maps:map(
                      fun(_Username, {Send_PID, _}) -> 
                              io:format("PID: ~p~n", [Send_PID]), 
                              Send_PID ! {text, JSON_Message} 
                      end, Member_map),
                    loop(Connection_map)
            end,
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
            {ok, {Friend_PID, _}} = maps:find(Friendname, Connection_map),
            Friend_PID ! {text, JSON_Message},
            loop(Connection_map);
        {chat_request, Chat_Name, Chat_ID, Creator, Members} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "chat_request"},
                                      {"status", "ok"},
                                      {"chat_name", Chat_Name},
                                      {"chat_id", Chat_ID},
                                      {"members", {array, Members}},
                                      {"creator", Creator}]}),
            
            Member_PIDs = [maps:find(Username, Connection_map) || Username <- Members],
            [PID ! {text, JSON_Message} || {ok, {PID, _}} <- Member_PIDs],
            loop(Connection_map);
        {logout_user, Username, Token} ->
            case maps:get(Username, Connection_map) of
                {_PID, Token} -> 
                    token_server ! {remove_token, Token, Username},
                    loop(maps:remove(Username, Connection_map));
                _ -> loop(Connection_map)
            end;

        {create_thread, Thread_ID, Server_Name, Username, Root_Header, Root_Comment, Timestamp, Commentlist} ->
            io:format("MADE IT PAST LIST OF DMs: ~p ~p ~p ~p ~p ~p ~p~n", [Thread_ID, Server_Name, Username, Root_Header, Root_Comment, Timestamp, Commentlist]),

            JSON_Message = mochijson:encode(
                             {struct,[{"action", "create_thread"},
                                      {"server_name", Server_Name},
                                      {"thread_id", Thread_ID},
                                      {"username", Username},
                                      {"root_post", {struct, [{"root_header", Root_Header},

                                                              {"root_comment", Root_Comment}]}},
                                      {"timestamp", Timestamp},
                                      {"commentList", {array, Commentlist}}
                                     ]}),
            io:format("Parsed JSON message create thread~n"),
            Fun = fun(_Username, {PID, _Magic_Token}) ->
                          PID ! {text, JSON_Message}
                  end,
            maps:map(Fun, Connection_map),
            loop(Connection_map);

        {dead_connection, From} ->
            maps:map(fun(Username, {PID, Token}) -> 
                             case PID of
                                 From -> token_server ! {remove_token, Token, Username};
                                 _ -> ok
                             end
                     end, Connection_map),

            loop(maps:filter(fun(_Username, {PID, _Token}) -> PID =/= From end, Connection_map));
        {insert_comment, Thread_ID, Username, Comment, Rating, Reply_User, Reply_Comment} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "insert_comment"},
                                      {"thread_id", Thread_ID},
                                      {"username", Username},
                                      {"comment", Comment},
                                      {"rating", Rating},
                                      {"reply", {struct, [{"reply_user", Reply_User},
                                                          {"reply_comment", Reply_Comment}]}}
                                     ]}),
            Fun = fun(_Username, {PID, _Magic_Token}) ->
                          PID ! {text, JSON_Message}
                  end,
            maps:map(Fun, Connection_map),
            loop(Connection_map);
        {upvote, Thread_ID, Rating, Index} ->
            io:format("UPVOTING~n"),
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "upvote"},
                                      {"thread_id", Thread_ID},
                                      {"rating", Rating},
                                      {"index", Index}]}),
            Fun = fun(_Username, {PID, _Magic_Token}) ->
                          PID ! {text, JSON_Message}
                  end,
            maps:map(Fun, Connection_map),
            loop(Connection_map);
        {downvote, Thread_ID, Rating, Index} ->
            JSON_Message = mochijson:encode(
                             {struct,[{"action", "downvote"},
                                      {"thread_id", Thread_ID},
                                      {"rating", Rating},
                                      {"index", Index}]}),
            Fun = fun(_Username, {PID, _Magic_Token}) ->
                          PID ! {text, JSON_Message}
                  end,
            maps:map(Fun, Connection_map),
            loop(Connection_map)
                
    end.
