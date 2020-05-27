%% @author Joakim Hansson <joakim.hansson92@gmail.com>
%% @doc This api providedes functions to insert/fetch chat up data To/from a PosgesSQL database.

-module(database_api).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-export([start/0, stop/0, insert_user/3, insert_friend/2, create_chat/3, insert_chat/4, fetch_user/1, fetch_friendlist/1, fetch_chat/1, fetch_chat_members/1, fetch_chat_undelivered/1, fetch_all_chats/1, create_thread/4, fetch_thread/1, insert_comment/5, fetch_thread_IDs/0, upvote/2, downvote/2]).


get_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second])).

%% @doc Initilize odbc connection and logs in to database.
-spec start() -> ok.

start() ->
    DB = erl_to_sql:init("PostgreSQL test", "adrenaline", "1234"),
    %%  io:format("database:start() -> DB = ~p~n", [DB]),
    register(database, DB),
    ok.

%% @doc stop connection with database then terminates.
-spec stop() -> ok.

stop() ->
    database ! stop,
    ok.

%% @doc Store information about a user in the database. 
%% @param Username The users ID.
%% @param Password The users login password.
%% @param Timestamp The time a user registered.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec insert_user(Username, Password, TimeStamp) -> ok when 
      Username::list(),
      Password::list(),
      TimeStamp::list().

insert_user(Username, Password, _TimeStamp) ->
    database ! {insert_user,Username, Password, get_timestamp(), self()},

    receive
        ok ->
            ok;
        {error, user_exist} ->
            {error, "Username already exist in database"};
        Msg ->
            {error, Msg}
    end.


%% @doc Insert a friend to a users friendlist.
%% @param Username The users ID.
%% @param Friend The friend to be assosiated with the User.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec insert_friend(Username, Friend) -> ok when 
      Username::list(),
      Friend::list().

insert_friend(Username, Friend) ->
    database ! {insert_friend,Username, Friend, self()},
    receive
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Msg ->
            io:format("database_api:insert_friend/2 Unhandled message: ~p~n", [Msg])
    end.


%% @doc Creates a new chat table in the database.
%% @param From_Username The users ID that sent this message.
%% @param Chat_ID ID of the chat.
%% @param TimeStamp The time message was sent.
%% @param Msg The message to store.
%% @param Status An Integer indicator of if a the message has been delivered. 1/0 delivered/undelivered.
%% @returns ok if write to database was successfull, {error, Reason} if not.
-spec create_chat(From_Username, {TimeStamp, Msg}, Status) -> ok when
      From_Username::list(),
      Msg::list(),
      TimeStamp::list(),
      Status::term().

create_chat(Chat_Name, Creator, Members) ->
    database ! {create_chat, Chat_Name, Creator, Members, self()},
    receive
        {ok, Chat_ID} ->
            Chat_ID;
        {error, Reason} ->
            {error, Reason};
        Msg ->
            io:format("databse_api:create_chat/3 Unhandeled message ~p~n", [Msg])
    end.

%% @doc Store information about a chat in the database.
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

insert_chat(From_Username, Chat_ID, {_TimeStamp, Msg}, Status) ->
    database ! {insert_chat, From_Username, Chat_ID, {get_timestamp(), Msg}, Status, self()},
    receive
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Fetch information about a user from the database.
%% @param Username The users ID..
%% @returns {ok, {Username, Timestamp}} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_user(Username) -> {Username, TimeStamp} when 
      Username::list(),
      TimeStamp::list().

fetch_user(Username) ->
    database ! {fetch_user,Username, self()},
    io:format("~p~n", [Username]),
    receive
        {error, no_user} ->
            {error, "Username not found in database."};
        {Username, Password, TimeStamp} ->
            {Username, Password, TimeStamp};
        Msg ->
            io:format("database_api:fetch_user/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch the firendlist of a user from the database.
%% @param Username The users ID..
%% @returns [{friend1}, {friend2}...] if fetch from database was successfull, {error, Reason} if not.
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
            Friends;
        Msg ->
            io:format("database_api:fetch_friendlist/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch information about a chat from the database.
%% @param Chat_ID The chat ID..
%% @returns {Chat_ID, Chat_Name, [{Sender, Msg, Status}]} if fetch from database was successfull, {error, Reason} if not.
-spec fetch_chat(Chat_ID) -> Chat_Data when 
      Chat_ID::list(),
      Chat_Data::term().

fetch_chat(Chat_ID) ->
    database ! {fetch_chat, Chat_ID, self()},

    receive
        {error, _} ->
            {error, "Chat_ID not found in database."};

        {Chat_ID, Chat_Name, Content} ->
            {Chat_ID, Chat_Name, Content};

        Msg ->
            io:format("database_api:fetch_chat/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch the usernames from all mebers of a chat. This function can be stuck waiting for a time when trying to fetch from database.
%% @param Chat_ID The chat ID..
%% @returns [{"member1", "member2",..}] if fetch from database was successfull, {error, Reason} if not.
fetch_chat_members(Chat_ID) ->
    database ! {fetch_chat_members, Chat_ID, self()},

    receive
        {_,_, Members} ->
            Members;
        {error, _} ->
            {error, "Chat_ID not found in database."};
        Msg ->
            io:format("database_api:fetch_chat_members/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch all undelivered messages of a certain chat.
%% @param Chat_ID The chat ID.
%% @returns [{Sender, Msg, Status}] if fetch from database was successfull, {error, Reason} if not.
fetch_chat_undelivered(Chat_ID) ->
    database ! {fetch_chat_undelivered, Chat_ID, self()},
    receive
        {_,_,Content} ->
            Content;
        {error, no_chat} ->
            {error, "Chat_ID not found in database."};
        Msg ->
            io:format("database_api:fetch_chat_undelivered/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Fetch all chats and messages asociated with a user.
%% @param Username The username to fetch chats from.
%% @returns [{Chat_ID, Chat_Name, [{Sender, Msg, Timestamp}]}] if fetch from database was successfull, [] (empty list) if not.
fetch_all_chats(Username) ->
    database ! {fetch_all_chats, Username, self()},

    receive
        {error, _} ->
            [];

        {ok, Chats} ->
            Chats;

        Msg ->
            io:format("database_api:fetch_all_chat/1 Unhandled message: ~p~n", [Msg])
    end.




%% @doc Creates a new thread and store it in database.
%% @param Username The username of the creator.
%% @param Server The ID of the Server that the thread is associated with.
%% @param Header The root header text of the thread.
%% @param Text The root text of the thread.
%% @returns Thread_ID if successfull, {error, Reason} if not.
-spec create_thread(Username, Server, Header, Text) -> Thread_ID when
      Username::list(),
      Server::list(),
      Header::list(),
      Text::list(),
      Thread_ID::term().

create_thread(Username, Server, Header, Text) ->
    database ! {create_thread, Username, Server, Header, Text, get_timestamp(), self()},

    receive
        {error, Reason} ->
            {error, Reason};
        {ok, Thread_ID} ->
            Thread_ID;
        Msg ->
            io:format("database_api:create_thread/4 Unhandled message: ~p~n", [Msg])
    end.

%% @doc fetches information about a thread from the database. This also include all the comments made on that thread.
%% @param Thread_ID The thread ID to be fetched.
%% @returns EXAMPLE: {"1", "skooben", "Header text", "Main text", "2020-1-1 00:00:00", [Comments]} if successfull, {error, Reason} if not.
-spec fetch_thread(Thread_ID) -> {Server, Creator, Header, Text, Timestamp, Commentlist} when
      Thread_ID::list(),
      Creator::list(),
      Server::list(),
      Header::list(),
      Timestamp::list(),
      Commentlist::list(),
      Text::list().

fetch_thread(Thread_ID) ->
    database ! {fetch_thread, Thread_ID, self()},
    receive
        {error, Reason} ->
            {error, Reason};
        {ok, {Server, Creator, Header, Text, Timestamp, null}} ->
           {Server, Creator, Header, Text, Timestamp, []};
        {ok, {Server, Creator, Header, Text, Timestamp, Commentlist}} ->
            {Server, Creator, Header, Text, Timestamp, Commentlist};
        Msg ->
            io:format("database_api:fetch_thread/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc fetches all thread IDs on the database
%% @returns EXAMPLE: {"1", "skooben", "Header text", "Main text", "2020-1-1 00:00:00", [{Commendata}]} if successfull, {error, Reason} if not.
-spec fetch_thread_IDs() -> list(list()).

fetch_thread_IDs() ->
    database ! {fetch_thread_IDs, self()},
    receive
        {error, Reason} ->
            {error, Reason};
        {ok, ThreadList} ->
           [ID || {ID} <- ThreadList];
        Msg ->
            io:format("database_api:fetch_thread/1 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Inserts a comment on a thread.
%% @param Thread_ID The thread ID to be commented.
%% @param Index The index of the comment made on the thread. Set this to "0" if this is the first comment.
%% @param Reply_Index The index of the comment this comment is replying. Set this to "" if no reply should be included.
%% @param Username The username that wrote this comment.
%% @param Text The actual comment/text that was written.
%% @returns Comment if successfull (Ex {"Thread_ID", "Username", "Text", {"Reply_Username", "Reply_Text"}}). {error, Reason} if not.
-spec insert_comment(Thread_ID, Index, Reply_Index, Username, Text) -> Comment when
      Thread_ID::list(),
      Username::list(),
      Index::list(),
      Reply_Index::list(),
      Text::list(),
      Comment::term().

insert_comment(Thread_ID, Index, Reply_Index, Username, Text) ->
    database ! {insert_comment, Thread_ID, Index, Reply_Index, Username, Text, get_timestamp(), self()},
    receive
        {error, Reason} ->
            {error, Reason};
        {ok, Comment_ID} ->
            Comment_ID;
        Msg ->
            io:format("database_api:insert_comment/5 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Upvotes a comment on in a thread.
%% @param Thread_ID The thread ID to the thread where the comment belongs.
%% @param Index The index of the comment. Set this to "0" if this is the first comment.
%% @returns The new rating if successfull, Ex 5 or -3. {error, Reason} if not.
-spec upvote(Thread_ID, Index) -> Rating when
      Thread_ID::list(),
      Index::list(),
      Rating:: integer.

upvote(Thread_ID, Index) ->
    database ! {upvote, Thread_ID, Index, self()},
    receive
        {error, Reason} ->
            {error, Reason};
        {ok, Rating} ->
            Rating;
        Msg ->
            io:format("database_api:upvote/2 Unhandled message: ~p~n", [Msg])
    end.

%% @doc Downvotes a comment on in a thread.
%% @param Thread_ID The thread ID to the thread where the comment belongs.
%% @param Index The index of the comment. Set this to "0" if this is the first comment.
%% @returns The new rating if successfull, Ex 5 or -3. {error, Reason} if not.
-spec downvote(Thread_ID, Index) -> Rating when
      Thread_ID::list(),
      Index::list(),
      Rating:: integer.

downvote(Thread_ID, Index) ->
    database ! {downvote, Thread_ID, Index, self()},
    receive
        {error, Reason} ->
            {error, Reason};
        {ok, Rating} ->
            Rating;
        Msg ->
            io:format("database_api:downvote/2 Unhandled message: ~p~n", [Msg])
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
    database ! reset_tests,
    %% This will produce a badmatch error if left hand side (ok) dont match with the right hand side (return value of insert_user). i.e this test will fail if they dont match. 
    ok = insert_user("testuser1", "testpassword1","2020-10-19 01:00:00"),
    ok = insert_user("testfriend1", "testpassword1","2020-10-19 01:00:00"),
    {error, _} = insert_user("testuser1", "testpassword1","2020-10-19 01:00:00").

insert_friend_test() ->
    ok = insert_friend("testuser1", "testfriend1"),
    {error, _} = insert_friend("invalid username", "testfriend1"),
    {error, _} = insert_friend("testuser1", "invalid friend"),
    {error, _} = insert_friend("invalid username", "invalid friend").

create_chat_test() ->

    _ = create_chat("festchatten","testuser1", ["testuser1", "testfriend1"]),
    _ = create_chat("skolchatten","testuser1", ["testuser1"]).

insert_chat_test() ->
    database ! {get_group_id,"festchatten", self()},
    receive
        Group_ID1 ->
            ok = insert_chat("testuser1", Group_ID1,{"2020-10-19 01:00:00", "test message1!!!"}, 1)
    end,
    database ! {get_group_id, "skolchatten", self()},
    receive
        Group_ID2 ->
            ok = insert_chat("testfriend1", Group_ID2,{"2020-10-19 01:00:05", "test message2!!!"}, 0)
    end.

fetch_user_test() ->
    {Username, Password, _} = fetch_user("testuser1"),
    "testuser1" = Username,
    "testpassword1" = Password,

    {error, _} = fetch_user("Invalid username").

fetch_chat_test() ->
    database ! {get_group_id,"festchatten", self()},
    receive
        Group_ID1 ->
            {_, "festchatten", [{Sender1, Msg1, Status1}]} = fetch_chat(Group_ID1),         
            "testuser1" = Sender1,
            "test message1!!!" = Msg1,
            1 = Status1
    end,
    database ! {get_group_id, "skolchatten", self()},
    receive
        Group_ID2 ->
            {_, "skolchatten", [{Sender2, Msg2, Status2}]} = fetch_chat(Group_ID2),          
            "testfriend1" = Sender2,
            "test message2!!!" = Msg2,
            0 = Status2
    end,

    {error, "Chat_ID not found in database."} = fetch_chat("Invalid Chat_ID").



fetch_chat_members_test() ->
    database ! {get_group_id,"festchatten", self()},
    receive
        Group_ID ->
            [{"testuser1"}, {"testfriend1"}] = fetch_chat_members(Group_ID)
    end,

    {error, "Chat_ID not found in database."} = fetch_chat_members("Invalid chat_ID").

fetch_friendlist_test() ->
    [{Friend}] = fetch_friendlist("testuser1"),
    "testfriend1" = Friend,
    {error,"No user id exists for that username"} = fetch_friendlist("Invalid username").


create_and_fetch_thread_test() ->
    Thread_ID = create_thread("testuser1", "0","Test thread", "This is the root thread."),
    {error, _} = create_thread("invalid user", "0","Test thread", "This is the root thread."),

    {Server, Creator, Header, Text, _Timestamp, Commentlist} = fetch_thread(Thread_ID),
    Server = "0",
    Creator = "testuser1",
    Header = "Test thread",
    Text = "This is the root thread.",
    Commentlist = [],
    {error, _} = fetch_chat("Invalid Thread_ID").

insert_comment_test() ->
    Thread_ID = create_thread("testuser1", "0","Test thread 2", "This is the second root thread."),
    Comment1 = insert_comment(Thread_ID, "0", "", "testuser1", "text test"),
    Comment2 = insert_comment(Thread_ID, "1", "0", "testfriend1", "reply text test"),
    {error, _} = insert_comment("-1", "0", "2", "invalid", "reply text test"),

    {Server, Creator, Header, Text, _Timestamp, [Comment1, Comment2]} = fetch_thread(Thread_ID),
    Server = "0",
    Creator = "testuser1",
    Header = "Test thread 2",
    Text = "This is the second root thread.",
    {Thread_ID, "testuser1", "text test", 0, {"",""}} = Comment1,
    {Thread_ID, "testfriend1", "reply text test", 0, {"testuser1", "text test"}} = Comment2.

upvote_and_downvote_test() ->
    Thread_ID = create_thread("testuser1", "0","Test thread 3", "This is the third root thread."),
    insert_comment(Thread_ID, "0", "", "testuser1", "text test"),
    insert_comment(Thread_ID, "1", "0", "testfriend1", "reply text test"),

    upvote(Thread_ID, "0"),
    upvote(Thread_ID, "0"),
    downvote(Thread_ID, "0"),
    downvote(Thread_ID, "1"),
    downvote(Thread_ID, "1"),

    {Server, Creator, Header, Text, _Timestamp, [Comment1, Comment2]} = fetch_thread(Thread_ID),
    Server = "0",
    Creator = "testuser1",
    Header = "Test thread 3",
    Text = "This is the third root thread.",
    {Thread_ID, "testuser1", "text test", 1, {"",""}} = Comment1,
    {Thread_ID, "testfriend1", "reply text test", -2, {"testuser1", "text test"}} = Comment2.




stop_test_() ->
    database ! reset_tests,
    %% database ! {remove_user, "testuser1"},
    %% database ! {remove_user, "testfriend1"},
    %% database ! {remove_table, "chat_id_1"},
    %% database ! {remove_friendlist, "testuser1"}, 
    timer:sleep(1000),
    [?_assertEqual(stop(), ok)
    ].
