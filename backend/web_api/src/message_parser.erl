-module(message_parser).
-export([handle_message/2]).

-spec handle_message(Msg, PID) -> ok when
      Msg :: bitstring,
      PID :: pid().
%% @doc Parses and takes appropriate action for a message from a user
%% @param Msg The message from the client in JSON format
%% @param PID The PID for the handler process
%% @returns ok For every message.
handle_message(Msg, PID) ->
    io:format("~p~n", [Msg]),
    io:format("~p~n", [mochijson:decode(Msg)]),
    case mochijson:decode(Msg) of
        {struct,[{"action", "register"},
                 {"username", Username},
                 {"password", Password}]} ->
            chat_server:register_user(Username, Password, "TODO: REAL TIMESTAMPS", PID);
        {struct,[{"action", "login"},
                 {"username", Username},
                 {"magictoken", {struct,[{"action", "login"},
                                         {"magic_token", Magic_token}]}}]} ->
            chat_server:login_user(Username, Magic_token, PID);
        {struct,[{"action", "send_message"},
                {"chat_id", Chat_ID},
                {"user_id", Username},
                {"message", Message},
                {"timestamp", Timestamp}]} ->
            chat_server:send_message(Username, Chat_ID, Message, Timestamp, PID);
        {struct,[{"action", "request_chat"},
                 {"chat_id", Chat_ID},
                 {"from", Username}]} ->
            chat_server:get_unread_messages(Chat_ID, Username, PID);
        {struct,[{"action", "friend_request"},
                 {"user_id", Friendname},
                 {"from", Username}]} ->
            chat_server:send_friend_request(Username, Friendname);
        {struct,[{"action", "chat_request"},
                 {"chat_name", Chat_Name},
                 {"from", Username},
                 {"members", {array,Members}}]} ->
            chat_server:send_chat(Chat_Name, Username, Members);
        {struct,[{"serverName", Server_Name},
                 {"thread_id", _Thread_ID},
                 {"username", Username},
                 {"root_post", {struct, [{"root_header", Root_Header},
                                        {"root_cooment", Root_Comment}]}},
                 {"timestamp", _Timestamp},
                 {"comments", {array, _CommentList}}]} ->
            chat_server:create_thread(Server_Name, Username, Root_Header, Root_Comment);
        {struct,[{"thread_id", Thread_ID},
                 {"index", Index},
                 {"reply_index", Reply_Index},
                 {"username", Username},
                 {"comment", Comment}]} ->
            chat_server:insert_comment(Thread_ID, Index, Reply_Index, Username, Comment);
         {struct,[{"action", "upvote"},
                 {"thread_id", Thread_ID},
                 {"index", Index}]} ->
            io:format("received upvote~n"),
            chat_server:upvote(Thread_ID, Index);
         {struct,[{"action", "downvote"},
                 {"thread_id", Thread_ID},
                 {"index", Index}]} ->
            chat_server:downvote(Thread_ID, Index);
        _ -> erlang:error('unknown message')
    end,
    ok.

            
