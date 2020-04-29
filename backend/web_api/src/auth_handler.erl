-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).


login(Username, Password, Req0, Opts) ->
    io:format("Logging in ~p~n", [Username]),
    Hashed_Password = password_utils:hash_password(Password),
    case database_api:fetch_user(Username) of
        {Username, Stored_Password, _} ->
            io:format("Stored: ~w~nCalced: ~w~n", [Stored_Password, Hashed_Password]),
            case binary:match(Hashed_Password, Stored_Password) of
                nomatch ->
                    Body = <<"Wrong password!">>,
                    Req3 = cowboy_req:reply(401, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
                    {ok, Req3, Opts};
                _ ->
                    io:format("AUTH SUCCESS FOR USER: ~p~n", [Username]),
                    Magic_Token = password_utils:get_magic_token(),
                    Body = mochijson:encode(
                             {struct,[{"action", "login"},
                                      {"magic_token", Magic_Token}]}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
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
            io:format("Registering user ~p~n", [Username]),
            ok = database_api:insert_user(Username, password_utils:hash_password(Password), "FOO"),
            Body = <<"Registration success!">>,
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0);
                _ ->
            Body = <<"User exists!">>,
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
                _ -> 
                    Body = <<"<h1>DO NOT SEND A GET TO THIS SERVER</h1>">>,
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
