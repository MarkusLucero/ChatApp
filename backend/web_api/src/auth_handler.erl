-module(auth_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
        {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Method, Req2} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            Body = <<"Sorry, client ID is not yet implemented">>;
        <<"GET">> ->
            Body = <<"<h1>DO NOT SEND A GET TO THIS SERVER</h1>">>;
        _ ->
            Body = <<"<h1>STILL A BAD REQUEST</h1>">>
    end,
    {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
        ok.
