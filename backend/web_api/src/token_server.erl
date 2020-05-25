-module(token_server).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, add_token/2, remove_token/2, check_token/2]).

-spec add_token(term(), term()) -> ok.
%% @doc Associates a token with a user.
%% @param Token the token
%% @param User the user
%% @returns `ok'
add_token(Token, User) ->
    gen_server:cast(?MODULE, {add_token, Token, User}).

-spec remove_token(term(), term()) -> ok.
%% @doc Removes a token-user association, if it exists.
%% @param Token the token
%% @param User the user
%% @returns `ok'
remove_token(Token, User) ->
    gen_server:cast(?MODULE, {remove_token, Token, User}).

-spec check_token(term(), term()) -> token_ok | token_not_ok.
%% @doc Checks if a token is associated with a user.
%% @param Token the token
%% @param User the user
%% @returns `token_ok' or `token_not_ok'
check_token(Token, User) ->
    gen_server:call(?MODULE, {check_token, Token, User}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    Table = ets:new(token_table, []),
    {ok, Table}.

handle_call({check_token, Token, User}, _From, Table) ->
    case ets:lookup(Table, Token) of
        [{Token,User}] ->
            {reply, token_ok, Table};
        _ ->
            {reply, token_not_ok, Table}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_token, Token, User}, Table) ->
    ets:insert(Table, {Token, User}),
    {noreply, Table};

handle_cast({remove_token, Token, User}, Table) ->
    ets:match_delete(Table, {Token, User}),
    {noreply, Table};

handle_cast(_Request, State) ->
    {noreply, State}.
