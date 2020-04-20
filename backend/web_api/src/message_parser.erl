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
                 {"password", Password}]} ->
            chat_server:login_user(Username, Password, PID);
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
        _ -> erlang:error('unknown message')
    end,
    ok.

            
