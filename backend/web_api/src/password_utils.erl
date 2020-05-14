%% @doc This module provides some helpful utilities for passwords and hashing

-module(password_utils).

-export([get_magic_token/0]).
-export([hash_password/1]).

-spec get_magic_token() -> list().
%% @doc Generates a magic token for a user to authenticate their connections
%% @returns A 32 character long random magic token (printable ASCII)
%% 32 chars should be enough, though no actual entropy calculations have been done
%% It's also not entirely clear whether this actually uses cryptographically secure random
get_magic_token() ->
    [33 + rand:uniform(93) || _ <- lists:seq(1, 32)].


-spec hash_password(list()) -> list().
%% @doc Hashes a password with 256 bit sha3 (NEITHER WITH SALT NOR PEPPER)
%% @param Password The password to hash
%% @returns A 256 bit hashed password in string format
hash_password(Password) ->
    Hash = crypto:hash(sha3_256, Password),
    io:format("Hash: ~p~n", [Hash]),
    lists:flatten([integer_to_list(X, 16) || <<X>> <= Hash]).
