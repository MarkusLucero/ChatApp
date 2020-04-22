-module(message_parser).
-export([handle_message/2]).

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
                 {"magictoken", Magic_token}]} ->
            chat_server:login_user(Username, Magic_token, PID);
        {struct,[{"action", "send_message"},
                {"chat_id", Chat_ID},
                {"user_id", Username},
                {"message", Message},
                {"timestamp", _}]} ->
            chat_server:send_message(Username, Chat_ID, Message, "2020-04-21 15:30:31", PID);
        {struct,[{"action", "request_chat"},
                 {"chat_id", Chat_ID},
                 {"from", Username}]} ->
            chat_server:get_unread_messages(Chat_ID, Username, PID);
        _ -> erlang:error('unknown message')
    end,
    ok.

            
