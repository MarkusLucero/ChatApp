-module(web_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([shell_test_start/0]).

-spec start_link() -> {ok, pid() | ignore | {error, any()}}.
%% @doc Starts the supervisor for the Web API node
%% @returns ok
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

shell_test_start() ->
    {ok, Pid} = supervisor:start_link(?MODULE, []),
    unlink(Pid).

%% @doc Starts the supervisor for the Web API node
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => token_server,
                    start => {token_server, start_link, []},
                    restart => permanent,
                    shutdown => 500,
                    type => worker,
                    modules => [token_server]},
                  #{id => chat_server,
                    start => {chat_server, start_link, []},
                    restart => permanent,
                    shutdown => 500,
                    type => worker,
                    modules => [chat_server]}],
    {ok, {SupFlags, ChildSpecs}}.
