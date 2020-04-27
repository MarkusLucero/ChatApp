-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

get_magic_token() ->
    tbi.

hash_password(_Password) ->
    tbi.

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
	    {struct,[{"action", "login"},
		     {"username", Username},
		     {"password", Password}]} = mochijson:decode(Data),

	    Hashed_Password = hash_password(Password),
	    Stored_Password = database_api:fetch_password(Username),
	    case string:equal(Hashed_Password, Stored_Password) of
		true ->
		    Magic_Token = get_magic_token(),
		    Body = mochijson:encode(
                             {struct,[{"action", "login"},
				      {"magic_token", Magic_Token}]}),
		    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
		    {ok, Req3, Opts};
		false ->
		    Body = <<"Wrong password!">>,
		    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
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
