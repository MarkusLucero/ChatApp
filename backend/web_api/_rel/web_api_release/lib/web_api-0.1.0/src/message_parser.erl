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
        {struct,[{"action", "send-message"},
                {"chat-id", Chat_ID},
                {"user-id", Username},
                {"message", Message},
                {"timestamp", Timestamp}]} ->
            chat_server:send_message(Username, Chat_ID, Message, Timestamp, PID);
        {struct,[{"action", "request-chat"},
                 {"chat-id", Chat_ID},
                 {"from", Username}]} ->
            chat_server:get_unread_messages(Chat_ID, Username, PID);
        _ -> erlang:error('unknown message')
    end,
    ok.

            
