-module(web_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/", cowboy_static, {priv_file, web_api, "index.html"}},
                                             {"/websocket", web_api_handler, []},
                                             {"/static/[...]", cowboy_static, {priv_dir, web_api, "static"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    web_api_sup:start_link().

stop(_State) ->
        ok.
