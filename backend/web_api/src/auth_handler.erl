-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3, register_user/4]).
-export([start_token_server/0]).


login(Username, Password, Req0, Opts) ->
    io:format("Logging in ~p~n", [Username]),
    Hashed_Password = password_utils:hash_password(Password),
    case database_api:fetch_user(Username) of
        {Username, Stored_Password, _} ->
            io:format("Stored: ~w~nCalced: ~w~n", [Stored_Password, Hashed_Password]),
            case string:equal(Hashed_Password, Stored_Password) of
                false ->
                    Body = <<"Wrong password!">>,
                    Req3 = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts};
                true ->
                    Magic_Token = password_utils:get_magic_token(),
                    io:format("AUTH SUCCESS FOR USER: ~p with token ~p~n", [Username, Magic_Token]),
                    Body = mochijson:encode(
                             {struct,[{"action", "login"},
                                      {"magic_token", Magic_Token}]}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    token_server ! {add_token, Magic_Token, Username},
                    {ok, Req3, Opts}
            end;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            Body = <<"No such user!">>,
            Req3 = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
            {ok, Req3, Opts}
    end.

register_user(Username, Password, Req0, Opts) ->
    case database_api:fetch_user(Username) of
        {error, _} -> 
            io:format("Registering user ~w~n", [Username]),
            database_api:insert_user(Username, password_utils:hash_password(Password), "2020-10-10 00:00:00"),
            Body = <<"Registration success!">>,
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0);
                _ ->
            Body = <<"User exists!">>,
            Req3 = cowboy_req:reply(403, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
            {ok, Req3, Opts}
    end.    

friend_request(Username, Friendname, Req0, Opts) ->
   case database_api:insert_friend(Username, Friendname) of
       ok ->
           %%chat_server:send_friend_request(Username, Friendname),
           Body = mochijson:encode(
                    {struct, [{"action", "friend_request"},
                              {"status", "ok"},
                              {"friend", Friendname}]}),
           Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
           {ok, Req3, Opts};
       _ ->
           Body = <<"Friend request failed!">>,
           Req3 = cowboy_req:reply(403, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
           {ok, Req3, Opts}
   end.    


commentList2JSON({Thread_ID, Username, Comment, Rating, {Reply_User, Reply_Comment}}) ->
    Reply = {struct, [{"reply_user", Reply_User},
                      {"reply_comment", Reply_Comment}]},
    {struct, [{"thread_id", Thread_ID},
              {"user_id", Username},
              {"comment", Comment},
	      {"rating", Rating},
              {"reply", Reply}]}.

request_thread(Thread_ID, Magic_Token, Username, Req0, Opts) ->
    io:format("REQUESTING THREAD: ~p~n", [Thread_ID]),
    token_server ! {check_token, Magic_Token, Username, self()},
    receive
        {ok, Magic_Token} ->
            case database_api:fetch_thread(Thread_ID) of
                {error, _Reason} ->
                    Body = <<"Bad request">>,
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts};
                {Server, Creator, Header, Text, Timestamp, Comments} ->
                    CommentList = [commentList2JSON(Comment) || Comment <- Comments],
                    JSON_Response = {struct,[{"action", "fetch_thread"},
                                             {"server_name", Server},
					     {"creator", Creator},
                                             {"header", Header},
                                             {"text", Text},
                                             {"timestamp", Timestamp},
                                             {"comment_list", {array, CommentList}}]},
                    Body = mochijson:encode(JSON_Response),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts}
            end;
        _ -> 
            Body = <<"Bad Auth">>,
            Req3 = cowboy_req:reply(403, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
            {ok, Req3, Opts}
    end.

fetch_thread_JSON(Thread_ID) ->
    io:format("REQUESTING THREAD JSON: ~p~n", [Thread_ID]),
    case database_api:fetch_thread(Thread_ID) of
        {error, _Reason} ->
            erlang:error(badarg);
        {Server, Creator, Header, Text, Timestamp, Comments} ->
            CommentList = [commentList2JSON(Comment) || Comment <- Comments],
            {struct,[{"action", "fetch_thread"},
                     {"thread_id", Thread_ID},
                     {"server_name", Server},
                     {"creator", Creator},
                     {"header", Header},
                     {"text", Text},
                     {"timestamp", Timestamp},
                     {"comment_list", {array, CommentList}}]}
    end.

request_server_contents(Server_Name, Magic_Token, Username, Req0, Opts) ->
    token_server ! {check_token, Magic_Token, Username, self()},
    receive
        token_ok ->
            case database_api:fetch_thread_IDs() of
                {error, _Reason} ->
                    Body = <<"Bad request">>,
                    Req3 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts};
                ThreadList ->
                    Threads_JSON = lists:map(fun(ID) -> fetch_thread_JSON(ID) end, ThreadList),
                    Body = mochijson:encode({struct,[{"action", "fetch_server_contents"},
                                                     {"server_name", Server_Name},
                                                     {"threads", {array, Threads_JSON}}
                                                     ]}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts}
            end;
        _ -> 
            Body = <<"Bad Auth">>,
            Req3 = cowboy_req:reply(403, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
            {ok, Req3, Opts}
    end.

-spec init(Req, State) -> {ok, Req, Opts} when
      Req :: cowboy_req:req(),
      State :: any(),
      Opts :: any().
%% @doc Initializes an http connection and authenticates users
%% @param Req0 The request sent by the client
%% @param Opts The state of the handler.
%% @returns A message for the client, if request was a POST, a magic token is returned. Otherwise a passive aggressive message is returned.
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Data, _} = cowboy_req:read_body(Req0),
            case mochijson:decode(Data) of
                {struct,[{"action", "login"},
                         {"username", Username},
                         {"password", Password}]} ->
                    login(Username, Password, Req0, Opts);
                {struct,[{"action", "register"},
                         {"username", Username},
                         {"password", Password}]} ->
                    io:format("REGISTER~n"),
                    register_user(Username, Password, Req0, Opts);
                {struct,[{"action", "friend_request"},
                         {"username", Friendname},
                         {"password", Username}]} ->
                    io:format("FRIEND REQUEST~n"),
                    friend_request(Username, Friendname, Req0, Opts);
                {struct,[{"action", "logout"},
                         {"username", Username},
                         {"magic_token", Token}]} ->
                    chat_server:logout_user(Username, Token),
                    Body = <<"Logout ACK">>,
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">> }, Body, Req0),
                    {ok, Req3, Opts};
                {struct, [{"action", "fetch_thread"},
                          {"thread_id", Thread_ID},
                          {"magic_token", Magic_Token},
                          {"username", Username}]} ->
                    request_thread(Thread_ID, Magic_Token, Username, Req0, Opts);
                {struct, [{"action", "fetch_server_contents"},
                          {"server_name", Server_Name},
                          {"magic_token", Magic_Token},
                          {"username", Username}]} ->
                    request_server_contents(Server_Name, Magic_Token, Username, Req0, Opts);
                _ -> 
                    Body = <<"<h1>Strange request!</h1>">>,
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">> }, Body, Req0),
                    {ok, Req3, Opts}
            end;
        <<"GET">> ->
            Body = <<"<h1>DO NOT SEND A GET TO THIS SERVER</h1>">>,
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">> }, Body, Req0),
            {ok, Req3, Opts};
        _ ->
            Body = <<"<h1>STILL A BAD REQUEST</h1>">>,
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">> }, Body, Req0),
            {ok, Req3, Opts}
    end.

-spec terminate(Reason, Req, State) -> ok when
      Reason     :: normal | stop | timeout
                  | remote | {remote, cow_ws:close_code(), binary()}
                  | {error, badencoding | badframe | closed | atom()}
                  | {crash, error | exit | throw, any()},
      Req :: cowboy_req:req(),
      State :: any().
%% @doc Terminates http connection
%% @param Reason The reason for termination.
%% @param Req The request to the server.
%% @param State The state of the handler.
%% @returns ok For all terminations.
terminate(_Reason, _Req, _State) ->
        ok.

-spec start_token_server() -> ok.
%% @doc Starts the token server
%% @returns ok.
start_token_server() ->
    case whereis(token_server) of
        undefined -> 
            ok;
        _PID -> 
            unregister(token_server)
    end,
    register(token_server, spawn(fun() -> token_server_loop(maps:new()) end)),
    ok.


token_server_loop(Token_map) ->
    receive
        {remove_token, Token, Username} ->
            case maps:get(Username, Token_map) of
                Token -> 
                    token_server_loop(maps:remove(Username, Token_map));
                _ -> 
                    token_server_loop(Token_map)
            end;
        {add_token, Token, User} ->
            io:format("Adding token: ~p~n", [Token]),
            New_map = maps:put(User, Token, Token_map),
            token_server_loop(New_map);
        {check_token, Token, User, From} ->
            io:format("Checking token: ~p~n", [Token]),
            case maps:find(User, Token_map) of
                {ok, Token} ->
                    From ! token_ok;
                error ->
                    From ! token_not_ok;
                _ ->
                    From ! token_not_ok
            end,
            token_server_loop(Token_map);
        _ ->
            token_server_loop(Token_map)
    end.
