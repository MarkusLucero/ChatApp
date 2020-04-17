-module(database_api).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([start/0, stop/0, insert_user/2, insert_chat/4, fetch_user/1, fetch_chat/1]).



-spec start() -> ok.

start() ->
    DB = erl_to_sql:init("PostgreSQL test", "testuser1", "1234"),
    register(database, DB),
    ok.

-spec stop() -> ok.

stop() ->
    database ! stop,
    ok.

-spec insert_user(UserName, TimeStamp) -> ok when
      UserName::list(),
      TimeStamp::list().

insert_user(UserName, TimeStamp) ->
    database ! {insert_user, UserName, TimeStamp},
    ok.

-spec insert_chat(From_Username, Chat_ID, {TimeStamp, Msg}, Status) -> ok when
      From_Username::list(),
      Chat_ID::list(),
      Msg::list(),
      TimeStamp::list(),
      Status::term().

insert_chat(From_Username, Chat_ID, {TimeStamp, Msg}, Status) ->
    database ! {insert_chat, From_Username, Chat_ID, {TimeStamp, Msg}, Status},
    ok.


-spec fetch_user(UserName) -> {UserName, TimeStamp} when
      UserName::list(),
      TimeStamp::list().

fetch_user(UserName) ->
    database ! {fetch_user, UserName, self()},
    
    receive
	{error, no_user} ->
	    {error, "UserName not found in database."};
	{UserName, TimeStamp} ->
	    {UserName, TimeStamp};
	Msg ->
	    io:format("database_api:fetch_user/1 Unhandled message: ~p~n", [Msg])
    end.

fetch_chat(Chat_ID) ->
    database ! {fetch_chat, Chat_ID, self()},
    
    receive
	{_,_,Content} ->
	    Content;
	{error, _} ->
	    {error, "Chat_ID not found in database."};
	Msg ->
	    io:format("database_api:fetch_chat/1 Unhandled message: ~p~n", [Msg])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% Eunit test cases  %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the database_api:test() function to this module.

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by database_api:test()

start_test_() ->
    [?_assertEqual(start(), ok)
    ].

insert_user_test() ->
    %% This will produce a badmatch error if left hand side (ok) dont match with the right hand side (return value of insert_user). i.e this test will fail if they dont match. 
    ok = insert_user("testuser1","2020-10-19 01:00:00").

insert_chat_test() ->
    ok = insert_chat("testuser1", "chat_id_1",{"2020-10-19 01:00:00", "test message1!!!"}, 1),
    ok = insert_chat("testuser2", "chat_id_1",{"2020-10-19 01:00:05", "test message2!!!"}, 0).

fetch_user_test() ->
    {UserName, TimeStamp} = fetch_user("testuser1"),
    "testuser1" = UserName,
    "2020-10-19 1:0:0" = TimeStamp,
    
    {error, "UserName not found in database."} = fetch_user("invalid_username").

fetch_chat_test() ->
    [{Sender1, Msg1, Status1},{Sender2, Msg2, Status2}] = fetch_chat("chat_id_1"),
    "testuser1" = Sender1,
    "testuser2" = Sender2,
    "test message1!!!" = Msg1,
    "test message2!!!" = Msg2,
    1 = Status1,
    0 = Status2,

   
    {error, "Chat_ID not found in database."} = fetch_chat("Invalid chat_ID").

stop_test_() ->
    database ! {remove_user, "testuser1"},
    database ! {remove_table, "chat_id_1"}, 
    timer:sleep(1000),
    [?_assertEqual(stop(), ok)
    ].
