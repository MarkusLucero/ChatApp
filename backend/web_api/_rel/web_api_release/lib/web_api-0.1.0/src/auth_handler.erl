-module(auth_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

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
            Body = <<"Sorry, client ID is not yet implemented">>,
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">> }, Body, Req0),
            {ok, Req3, Opts};
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
