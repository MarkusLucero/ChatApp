-module(web_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid() | ignore | {error, any()}}.
%% @doc Starts the supervisor for the Web API node
%% @returns ok
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, integer(), integer()}, list()}}.
%% @doc Starts the supervisor for the Web API node
%% @param List MUST BE EMPTY LIST
%% @returns ok
init([]) ->
    Procs = [],
    {ok, {{one_for_one, 1, 5}, Procs}}.
