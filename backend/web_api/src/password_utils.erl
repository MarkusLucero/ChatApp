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




%%%%%%%%%%%%%%%%%%%%%%
%% TESTS START HERE %%
%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Just checks the length of the magic tokens (REGRESSION TEST)
magic_token_test() ->
    [?_assertEqual(32, lists:length(?MODULE:get_magic_token())) || _ <- lists:seq(1, 10000)].

%% This one just exists to make sure noone messes up and uses the wrong hashing algorithm (REGRESSION TEST)
hash_test() ->
    "73A83D6DAEE2AF7E94211D4CA4978A9FA13F46CF1B7DEFD7C2743EDFE24E7C3" = password_utils:hash_password("adrenaline"),
    "4876E8F21DD719FA3D3EB88EB6A936BD5D35287CBFEE546BE9CB86C6BF1031" = password_utils:hash_password("our security isnt very good"),
    "FFFA816B58824F3F4C872ECAC1D18F9EB7F2123470D437EAFE3FF75E1C3923" = password_utils:hash_password("secure_password123").
-endif.
