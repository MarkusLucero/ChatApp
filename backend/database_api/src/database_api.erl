%% @author Joakim Hansson <joakim.hansson92@gmail.com>
%% @doc This api providedes functions to insert/fetch chat up data To/from a PosgesSQL database. Calls to insert functions will never be delayed by the database. Calls to fetch function may be delayed if the database is busy. 

-module(database_api).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([start/0, stop/0, insert_user/3, insert_friend/2, create_chat/4, insert_chat/4, fetch_user/1, fetch_friendlist/1, fetch_chat/1, fetch_chat_members/1, fetch_chat_undelivered/1]).


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

%% @doc Insert a friend to a users friendlist. This function is asyncronous.
%% @param Username The users ID.
%% @param Friend The friend to be assosiated with the User.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec insert_friend(Username, Friend) -> ok when 
      Username::list(),
      Friend::list().

insert_friend(Username, Friend) ->
%%  io:format("Insert_friend(~p, ~p)~n", [Username, Friend]),
    database ! {insert_friend,Username, Friend, self()},
    receive
	{error, no_user} ->
	     {error, "Username not found in database."};
	{error, no_friend} ->
	     {error, "Friend not found in database."};
	ok ->
	    ok;
	Msg ->
	    io:format("database_api:insert_friend/2 Unhandled message: ~p~n", [Msg])
     end.


%% @doc Creates a new chat table in the database. This function is asyncronous (the caller will not have to waite for the actual write to database to happen).
%% @param From_Username The users ID that sent this message.
%% @param Chat_ID ID of the chat.
%% @param TimeStamp The time message was sent.
%% @param Msg The message to store.
%% @param Status An Integer indicator of if a the message has been delivered. 1/0 delivered/undelivered.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec create_chat(From_Username, Chat_ID, {TimeStamp, Msg}, Status) -> ok when
      From_Username::list(),
      Chat_ID::list(),
      Msg::list(),
      TimeStamp::list(),
      Status::term().

create_chat(Chat_ID, Chat_Name, Creator, Members) ->
%%  io:format("Insert_chat(~p, ~p, ~p, ~p, ~p)~n", [From_Username, Chat_ID, TimeStamp, Msg, Status]),
    database ! {create_chat, Chat_ID, Chat_Name, Creator, Members},
    ok.

%% @doc Store information about a chat in the database. This function is asyncronous (the caller will not have to waite for the actual write to database to happen).5
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
%% @returns {ok, {Username, Timestamp}} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_user(Username) -> {Username, TimeStamp} when 
      Username::list(),
      TimeStamp::list().

fetch_user(Username) ->
    database ! {fetch_user,Username, self()},
    
    receive
	{error, no_user} ->
	    {error, "Username not found in database."};
	{Username, Password, TimeStamp} ->
	    {ok, {Username, Password, TimeStamp}};
	Msg ->
	    io:format("database_api:fetch_user/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch the firendlist of a user from the database. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Username The users ID..
%% @returns {ok, {Username, [{friend1}, {friend2}...]}} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_friendlist(Username) -> {Username, Friends} when 
      Username::list(),
      Friends::term().

fetch_friendlist(Username) ->
    database ! {fetch_friendlist, Username, self()},
    
    receive
	{error, no_user} ->
	    {error, "No user id exists for that username"};
	{error, no_friendlist} ->
	    {error, "No friends exists for that username"};
	{ok, Friends} ->
	    {ok, {Username, Friends}};
	Msg ->
	    io:format("database_api:fetch_friendlist/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch information about a chat from the database. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Chat_ID The chat ID..
%% @returns {ok,[{Sender, Msg, Status}]} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_chat(Chat_ID) -> Chat_Data when 
      Chat_ID::list(),
      Chat_Data::term().

fetch_chat(Chat_ID) ->
    database ! {fetch_chat, Chat_ID, self()},
    
    receive
	{_,_,Content} ->
	    {ok, Content};
	{error, _} ->
	    {error, "Chat_ID not found in database."};
	Msg ->
	    io:format("database_api:fetch_chat/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch the usernames from all mebers of a chat. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Chat_ID The chat ID..
%% @returns {ok,[{"member1"}, {"member2"},..]} if fetch from database was successfull, {error, Reason} if not.
fetch_chat_members(Chat_ID) ->
    database ! {fetch_chat_members, Chat_ID, self()},
    
    receive
	{_,_,Content} ->
	    {ok, Content};
	{error, _} ->
	    {error, "Chat_ID not found in database."};
	Msg ->
	    io:format("database_api:fetch_chat_members/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch all undelivered messages of a certain chat. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Chat_ID The chat ID.
%% @returns {ok,[{Sender, Msg, Status}]} if fetch from database was successfull, {error, Reason} if not.
fetch_chat_undelivered(Chat_ID) ->
    database ! {fetch_chat_undelivered, Chat_ID, self()},
    receive
	{_,_,Content} ->
	    {ok, Content};
	{error, no_chat} ->
	    {error, "Chat_ID not found in database."};
	Msg ->
	    io:format("database_api:fetch_chat_undelivered/1 Unhandled message: ~p~n", [Msg])
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
    ok = insert_user("testuser1", "testpassword1","2020-10-19 01:00:00"),
    ok = insert_user("testfriend1", "testpassword1","2020-10-19 01:00:00").

insert_friend_test() ->
    ok = insert_friend("testuser1", "testfriend1"),
    {error, "Username not found in database."} = insert_friend("invalid username", "testfriend1"),
    {error, "Friend not found in database."} = insert_friend("testuser1", "invalid friend"),
    {error, "Username not found in database."} = insert_friend("invalid username", "invalid friend").

create_chat_test() ->
    ok = create_chat("chat_id1", "festchatten","Boris", ["testuser1", "testfriend1"]),
    ok = create_chat("chat_id2", "skolchatten","Anna", ["testuser1"]).

insert_chat_test() ->
    ok = insert_chat("testuser1", "festchatten",{"2020-10-19 01:00:00", "test message1!!!"}, 1),
    ok = insert_chat("testfriend1", "skolchatten",{"2020-10-19 01:00:05", "test message2!!!"}, 0).

fetch_user_test() ->
    {ok,{Username, Password, TimeStamp}} = fetch_user("testuser1"),
    "testuser1" = Username,
    "testpassword1" = Password,
    "2020-10-19 1:0:0" = TimeStamp,
    
    {error, "Username not found in database."} = fetch_user("Invalid username").

fetch_chat_test() ->
    {ok, [{Sender1, Msg1, Status1}]} = fetch_chat("festchatten"),
    {ok, [{Sender2, Msg2, Status2}]} = fetch_chat("skolchatten"),
    "testuser1" = Sender1,
    "testfriend1" = Sender2,
    "test message1!!!" = Msg1,
    "test message2!!!" = Msg2,
    1 = Status1,
    0 = Status2,

   
    {error, "Chat_ID not found in database."} = fetch_chat("Invalid chat_ID").



 fetch_chat_members_test() ->
    {ok, [{"testuser1"}, {"testfriend1"}]} = fetch_chat_members("festchatten"),

    {error, "Chat_ID not found in database."} = fetch_chat_members("Invalid chat_ID").

fetch_friendlist_test() ->
    {ok, {_, [{Friend}]}} = fetch_friendlist("testuser1"),
    "testfriend1" = Friend,
    {error,"No user id exists for that username"} = fetch_friendlist("Invalid username").

stop_test_() ->
   %% database ! reset_tests,
    %% database ! {remove_user, "testuser1"},
    %% database ! {remove_user, "testfriend1"},
    %% database ! {remove_table, "chat_id_1"},
    %% database ! {remove_friendlist, "testuser1"}, 
    timer:sleep(1000),
    [?_assertEqual(stop(), ok)
    ].
