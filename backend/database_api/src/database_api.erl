%% @author Joakim Hansson <joakim.hansson92@gmail.com>
%% @doc This api providedes functions to insert/fetch chat up data To/from a PosgesSQL database. Calls to insert functions will never be delayed by the database. Calls to fetch function may be delayed if the database is busy. 

-module(database_api).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([start/0, stop/0, insert_user/3, insert_chat/4, fetch_user/1, fetch_chat/1]).


%% @doc Initilize odbc connection and logs in to database.
-spec start() -> ok.

start() ->
    DB = erl_to_sql:init("PostgreSQL test", "testuser1", "1234"),
%%  io:format("database:start() -> DB = ~p~n", [DB]),
    register(database, DB),
    ok.

%% @doc stop connection with database then terminates.
-spec stop() -> ok.

stop() ->
    database ! stop,
    ok.

%% @doc Store information about a user in the database. This function is asyncronous (the caller will not have to waite for the actual write to database to happen).
%% @param Username The users ID.
%% @param Password The users login password.
%% @param Timestamp The time a user registered.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec insert_user(Username, Password, TimeStamp) -> ok when 
      Username::list(),
      Password::list(),
      TimeStamp::list().

insert_user(Username, Password, TimeStamp) ->
%%  io:format("Insert_user(~p, ~p, ~p)~n", [Username, Password, TimeStamp]),
    database ! {insert_user,Username, Password, TimeStamp},
    ok.

%% @doc Store information about a chat in the databse. This function is asyncronous (the caller will not have to waite for the actual write to database to happen).5
%% @param From_Username The users ID that sent this message.
%% @param Chat_ID ID of the chat.
%% @param TimeStamp The time message was sent.
%% @param Msg The message to store.
%% @param Status An Integer indicator of if a the message has been delivered. 1/0 delivered/undelivered.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec insert_chat(From_Username, Chat_ID, {TimeStamp, Msg}, Status) -> ok when
      From_Username::list(),
      Chat_ID::list(),
      Msg::list(),
      TimeStamp::list(),
      Status::term().

insert_chat(From_Username, Chat_ID, {TimeStamp, Msg}, Status) ->
%%  io:format("Insert_chat(~p, ~p, ~p, ~p, ~p)~n", [From_Username, Chat_ID, TimeStamp, Msg, Status]),
    database ! {insert_chat, From_Username, Chat_ID, {TimeStamp, Msg}, Status},
    ok.


%% @doc Fetch information about a user from the database. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Username The users ID..
%% @returns {Username, Timestamp} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_user(Username) -> {Username, TimeStamp} when 
      Username::list(),
      TimeStamp::list().

fetch_user(Username) ->
    database ! {fetch_user,Username, self()},
    
    receive
	{error, no_user} ->
	    {error, "Username not found in database."};
	{Username, Password, TimeStamp} ->
	    {Username, Password, TimeStamp};
	Msg ->
	    io:format("database_api:fetch_user/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch information about a chat from the database. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Chat_ID The chat ID..
%% @returns [{Sender, Msg, Status}] if fetch from database was successfull, {error, Reason} if not.
-spec fetch_chat(Chat_ID) -> Chat_Data when 
      Chat_ID::list(),
      Chat_Data:: term().

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
    odbc:start(),
    [?_assertEqual(start(), ok)
    ].

insert_user_test() ->
    %% This will produce a badmatch error if left hand side (ok) dont match with the right hand side (return value of insert_user). i.e this test will fail if they dont match. 
    ok = insert_user("testuser1", "testpassword1","2020-10-19 01:00:00").

insert_chat_test() ->
    ok = insert_chat("testuser1", "chat_id_1",{"2020-10-19 01:00:00", "test message1!!!"}, 1),
    ok = insert_chat("testuser2", "chat_id_1",{"2020-10-19 01:00:05", "test message2!!!"}, 0).

fetch_user_test() ->
    {Username, Password, TimeStamp} = fetch_user("testuser1"),
    "testuser1" = Username,
    "testpassword1" = Password,
    "2020-10-19 1:0:0" = TimeStamp,
    
    {error, "Username not found in database."} = fetch_user("invalid_username").

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
