-module(web_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

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

stop(_State) ->
        ok.
