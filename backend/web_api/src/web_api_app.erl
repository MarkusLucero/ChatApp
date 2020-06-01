-module(web_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(Type, Args) -> {ok, pid() | ignore | {error, any()}} when 
      Type :: any(),
      Args :: any().
%% @doc Starts the Web API listener
%% @param Type IGNORED
%% @param Args IGNORED
%% @returns ok
start(_Type, _Args) ->
    chat_server:start(),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", auth_handler, []},
                                             {"/websocket", web_api_handler, []}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    web_api_sup:start_link().

-spec stop(State) -> ok when 
      State :: any().
%% @doc Stops the Web API listener
%% @param State The state of the listener
%% @returns ok
stop(_State) ->
        ok.
