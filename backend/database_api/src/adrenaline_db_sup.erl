-module(adrenaline_db_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
%% @doc Starts the supervisor for the database service.
%% @returns `{ok, pid()}' on success
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => erl_to_sql,
                    start => {erl_to_sql, start_link, []},
                    restart => permanent,
                    shutdown => 500,
                    type => worker,
                    modules => [erl_to_sql]}],
    {ok, {SupFlags, ChildSpecs}}.
