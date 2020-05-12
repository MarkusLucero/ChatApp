-module(erl_to_sql).

-export([init/3, stringify_timestamp/1]).

-spec init(DSN, UID, PWD) -> DB when
      DSN :: list(),
      UID :: list(),
      PWD :: list(),
      DB  :: pid().

init(DSN, UID, PWD) ->
    odbc:start(),
    spawn(fun() -> loop(DSN, UID, PWD) end).

loop(DSN, UID, PWD) ->
    {ok, Ref} = odbc:connect("DSN="++DSN++";UID="++UID++";PWD="++PWD, []),
    loop(Ref).

loop(Ref) ->
    receive
        {insert_user, Username, Password, Timestamp, From} ->
	    insert_user(Ref, Username, Password, Timestamp, From);
        
        {insert_friend, Username, Friend, From} ->
	    insert_friend(Ref, Username, Friend, From);

        {create_chat, Chat_Name, _, Members, From} ->
	    create_chat(Ref, Chat_Name, unused, Members, From);
        
        {insert_chat, From_Username, Chat_ID, Message, Status, From} ->
            insert_chat(Ref, From_Username, Chat_ID, Message, Status, From);

        {fetch_user, Username, From} ->
	    fetch_user(Ref, Username, From);

	{fetch_friendlist, Username, From} ->
           fetch_friendlist(Ref, Username, From);

        {fetch_chat, Chat_ID, From} ->
           fetch_chat(Ref, Chat_ID, From);

        {fetch_chat_members, Chat_ID, From} -> 
	    fetch_chat_members(Ref, Chat_ID, From);

        {fetch_all_chats, Username, From} ->
	    fetch_all_chats(Ref, Username, From);

	{create_thread, Username, Server, Header, Root, From} ->
	    create_thread(Ref, Username, Server, Header, Root, From);

        stop ->
            odbc:disconnect(Ref),
            odbc:stop(),
            exit(succeful_exit);
        
        reset_tests ->
            odbc:sql_query(Ref, "DELETE FROM messages * WHERE username = 'testuser1';"),
            odbc:sql_query(Ref, "DELETE FROM messages * WHERE username = 'testfriend1';"),
            odbc:sql_query(Ref, "DELETE FROM group_users * WHERE username = 'testuser1';"),
	    odbc:sql_query(Ref, "DELETE FROM group_users * WHERE username = 'testfriend1';"),
            odbc:sql_query(Ref, "DELETE FROM groups * WHERE groupname = 'festchatten';"),
            odbc:sql_query(Ref, "DELETE FROM groups * WHERE groupname = 'skolchatten';"),
            odbc:sql_query(Ref, "DELETE FROM friendlist * WHERE username = 'testuser1';"),
            odbc:sql_query(Ref, "DELETE FROM friendlist * WHERE username = 'testfriend1';"),
	    odbc:sql_query(Ref, "DELETE FROM thread * WHERE username = 'testuser1';"),
            odbc:sql_query(Ref, "DELETE FROM users * WHERE username = 'testuser1';"),
            odbc:sql_query(Ref, "DELETE FROM users * WHERE username = 'testfriend1';");

        {remove_table, Table} ->
            odbc:sql_query(Ref, "DROP TABLE chat" ++ Table ++ ";");

        {remove_user, Username} ->
            odbc:sql_query(Ref, "DELETE FROM users WHERE username = '" ++ Username ++ "';");

        {remove_friendlist, Username} ->
            odbc:sql_query(Ref, "DELETE FROM friend_list WHERE username = '" ++ Username ++ "';");
        
        {get_group_id, Username, From} ->
            From ! fetch_group_id(Ref, Username);
        
        Msg ->
            io:format("database_api:loop/1 Unhandled message: ~p~n", [Msg])
    end,
    loop(Ref).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Write all main functionality to communicate with Postgres below  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_user(Ref, Username, Password, Timestamp, From) ->
    Status = (catch odbc:sql_query(Ref, "INSERT INTO users (username, password, timestamp) VALUES('" ++ Username ++ "', '" ++ Password ++ "' ,'" ++ Timestamp ++"')")),
    case Status of
	{updated, 1} ->
	    From ! ok;
	{error, Reason} ->
	    From ! {error, Reason};
	Msg ->
	    From ! {error, Msg}
    end,
    loop(Ref).

insert_friend(Ref, Username, Friend, From) ->
    User_ID = fetch_user_id(Ref, Username),
    Friend_ID = fetch_user_id(Ref, Friend),

    case User_ID of
	{error, Reason} ->
	    From ! {error, Reason},
	    loop(Ref);
	_ ->
	    ok
    end,
    case Friend_ID of
	{error, Reason2} ->
	    From ! {error, Reason2};
	_ ->
	    odbc:sql_query(Ref, "INSERT INTO friendlist (user_id, friend_id, username, friendname, status) VALUES('" ++ User_ID ++ "', '" ++ Friend_ID ++ "', '" ++ Username ++ "', '" ++ Friend ++ "', 0);"),
	    From ! ok
    end,
    loop(Ref).
	
create_chat(Ref, Chat_Name, _, Members, From) ->
    Status = (catch odbc:sql_query(Ref, "INSERT INTO groups (groupname) VALUES ('"++ Chat_Name ++ "');")),
    case Status of
     	{updated, 1} ->
	    ok;
	{error, Reason} ->
	    From ! {error, Reason},
	    loop(Ref)
    end,
    Chat_ID = fetch_group_id(Ref, Chat_Name),
    case Chat_ID of
	{error, Reason2} ->
	    From ! {error, Reason2};
	_ ->
	    add_group_members(Ref, Chat_ID, Members),
	    From ! {ok, Chat_ID}
    end,
    loop(Ref).
  
insert_chat(Ref, From_Username, Chat_ID, {Timestamp, Msg}, Status, From) ->
    User_ID = fetch_user_id(Ref, From_Username),
    Success = (catch odbc:sql_query(Ref, "INSERT INTO messages (username, user_id, group_id, message, status, timestamp) VALUES ('"++ From_Username ++"', '"++ User_ID ++ "', '" ++ Chat_ID ++ "', '" ++ Msg ++ "', '" ++ integer_to_list(Status) ++ "', '" ++ Timestamp ++ "');")),
    case Success of
	{updated, 1} ->
	    From ! ok;
	{error, Reason} ->
	    From ! {error, Reason}
    end,
    loop(Ref).

fetch_user(Ref, Username, From) ->
    Content = odbc:sql_query(Ref, "SELECT username, password, timestamp FROM users WHERE username = '"++ Username ++ "';"),
    case Content of
	{selected,_,[]} ->
	    From ! {error, no_user};
	{selected,_,[{Username, Password, Timestamp}]} ->
	    From ! { Username, Password, stringify_timestamp( Timestamp)}
    end,
    loop(Ref).

fetch_friendlist(Ref, Username, From) ->
    User_ID = fetch_user_id(Ref, Username),
    case User_ID of
	{error, _} ->
	    From ! {error, no_user},
	    loop(Ref);
	_ ->

	    ok
    end,
    Content = (catch odbc:sql_query(Ref, "SELECT friendname FROM friendlist WHERE user_id = '" ++ User_ID ++ "';")),
    case Content of
	{selected,_,[]} ->
	    From ! {error, no_friendlist};
	{selected,_,Friendlist} ->
	    From ! {ok, Friendlist}
    end,
    loop(Ref).

fetch_chat(Ref, Chat_ID, From) ->
    Content = (catch odbc:sql_query(Ref, "SELECT username, message, status FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),

    case Content of
	{error, Reason} ->
	    From ! {error, Reason};
	{_,_,Messages} ->
	    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
	    From ! {Chat_ID, Chat_Name, Messages}
    end,
    loop(Ref).

fetch_chat_members(Ref, Chat_ID, From) ->
    Content = (catch odbc:sql_query(Ref, "SELECT username FROM group_users WHERE group_id = '" ++ Chat_ID ++ "';")),
    From ! Content,
    loop(Ref).

fetch_all_chats(Ref, Username, From) ->
    {selected, _, Chat_IDS} = (catch odbc:sql_query(Ref, "SELECT group_id FROM group_users WHERE username = '" ++ Username ++ "';")),
    case Chat_IDS of 
	[] ->
	    From ! {error, "No chats found in database"};
	_ ->
	    All_Chats = fetch_all_chats_helper(Chat_IDS, [], Ref),
	    From ! {ok, All_Chats}
    end,
    loop(Ref).

create_thread(Ref, Username, Server, Header, Text, From) ->
    User_ID = fetch_user_id(Ref, Username),
    case User_ID of
	{error, Reason1} ->
	    From ! {error, Reason1},
	    loop(Ref);
	_ ->
	    ok
    end,
    Status = (catch odbc:sql_query(Ref, "INSERT INTO thread (username, user_id, server_id, root_header, root_text) VALUES ('" ++ Username ++ "', '" ++ User_ID ++ "', " ++ Server ++ ", '" ++ Header ++ "', '" ++ Text ++ "');")),
    case Status of
     	{updated, 1} ->
	    Thread_ID =  odbc:sql_query(Ref, "SELECT thread_id FROM thread WHERE (username = '" ++ Username ++ "' AND root_header = '" ++ Header ++ "');"),
	    From !  {ok, Thread_ID};
	{error, Reason2} ->
	    From ! {error, Reason2}
    end,
    loop(Ref).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions  %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_user_id(Ref, Username) ->
    ID = odbc:sql_query(Ref, "SELECT user_id FROM users WHERE username = '" ++ Username ++ "';"),
    case ID of  
        {selected, _, [{User_ID}]} ->
	    User_ID;
        Error ->
            {error, Error}
    end.
fetch_group_id(Ref, Groupname) ->
    ID = odbc:sql_query(Ref, "SELECT group_id FROM groups WHERE groupname = '" ++ Groupname ++ "';"),
    
    case ID of
        {selected, _, [{Group_ID}]} ->
            Group_ID;
        Error ->
            {error, Error}
    end.

add_group_members(Ref, Group_ID, [Member]) ->
    M = fetch_user_id(Ref, Member),
    odbc:sql_query(Ref, "INSERT INTO group_users (group_id, user_id, username) VALUES ('" ++ Group_ID ++ "', '" ++ M ++ "', '" ++ Member ++ "');");

add_group_members(Ref, Group_ID, [X|Members]) ->
    M = fetch_user_id(Ref, X),
    odbc:sql_query(Ref, "INSERT INTO group_users (group_id, user_id, username) VALUES ('"++ Group_ID ++ "', '" ++ M ++ "', '" ++ X ++ "');"),
    add_group_members(Ref, Group_ID, Members).

fetch_all_chats_helper([{Chat_ID}], All_Chats, Ref) ->
    {_,_,Messages} = (catch odbc:sql_query(Ref, "SELECT username, message FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
    New_Chat_List = All_Chats ++ [{Chat_ID, Chat_Name, Messages}],
    New_Chat_List;

fetch_all_chats_helper([{Chat_ID}|Tail], All_Chats, Ref) ->
    {_,_,Messages} = (catch odbc:sql_query(Ref, "SELECT username, message FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
    New_Chat_List = All_Chats ++ [{Chat_ID, Chat_Name, Messages}],
    fetch_all_chats_helper(Tail, New_Chat_List, Ref).

%% PostgreSQL return TIMESTAMP as a multi-tuple e.g {{2020,03,19}, {13,7,0}} where the date and time are integers. This function converts that tuple to a string on format "2020-03-10 13:7:0". Maybe it would be easier to compare different timestamps if we keep them as integers???
stringify_timestamp({Date,Time}) ->
    {Year,Month,Day} = Date,
    {Hour,Minute,Second} = Time,
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).
