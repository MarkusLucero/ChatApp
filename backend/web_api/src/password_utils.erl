-module(password_utils).

-export([get_magic_token/0]).
-export([hash_password/1]).

%%32 chars should be enough
%%33 126
get_magic_token() ->
    [33 + rand:uniform(93) || _ <- lists:seq(1, 32)].

hash_password(Password) ->
    Hash = crypto:hash(sha3_256, Password),
    io:format("Hash: ~p~n", [Hash]),
    lists:flatten([integer_to_list(X, 16) || <<X>> <= Hash]).
