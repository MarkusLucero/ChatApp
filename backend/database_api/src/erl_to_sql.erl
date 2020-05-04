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
        {insert_user, Username, Password, Timestamp} ->
	    odbc:sql_query(Ref, "INSERT INTO users (username, password, timestamp) VALUES('" ++ Username ++ "', '" ++ Password ++ "' ,'" ++ Timestamp ++"')");
	
        {insert_friend, Username, Friend, From} ->
            User_ID = fetch_user_id(Ref, Username),
            Friend_ID = fetch_user_id(Ref, Friend),

            case User_ID of
                {error, _} ->
                    From ! {error, no_user},
                    loop(Ref);
                _ ->
                    ok
            end,
            case Friend_ID of
                {error, _} ->
                    From ! {error, no_friend};
                _ ->
                    odbc:sql_query(Ref, "INSERT INTO friendlist (user_id, friend_id, username, friendname, status) VALUES('" ++ User_ID ++ "', '" ++ Friend_ID ++ "', '" ++ Username ++ "', '" ++ Friend ++ "', 0);"),
                    From ! ok
            end;

        {create_chat, Chat_Name, _, Members, From} ->
            odbc:sql_query(Ref, "INSERT INTO groups (groupname) VALUES ('"++ Chat_Name ++ "');"),
            Chat_ID = fetch_group_id(Ref, Chat_Name),
            case Chat_ID of
                {error, Reason} ->
                    From ! {error, Reason};
                _ ->
                    add_group_members(Ref, Chat_ID, Members),
                    From ! {ok, Chat_ID}
            end;
        
        {insert_chat, From_Username, Chat_ID, { Timestamp, Msg}, Status} ->
            
            %% odbc:sql_query(Ref, "CREATE TABLE IF NOT EXISTS chat" ++ Chat_ID ++ "  (from_user VARCHAR(50) NOT NULL, message TEXT NOT NULL, status INT, time_stamp TIMESTAMP NOT NULL);"),
            User_ID = fetch_user_id(Ref, From_Username),
            odbc:sql_query(Ref, "INSERT INTO messages (username, user_id, group_id, message, status, timestamp) VALUES ('"++ From_Username ++"', '"++ User_ID ++ "', '" ++ Chat_ID ++ "', '" ++ Msg ++ "', '" ++ integer_to_list(Status) ++ "', '" ++ Timestamp ++ "');");

            %% case Group_ID of
            %%  {error, Reason} ->
            %%      From ! {error, Reason};
            %%  _ ->
            %%      odbc:sql_query(Ref, "INSERT INTO messages (username, groupname, user_id, group_id, message, status, timestamp) VALUES ('"++ From_Username ++"', '"++ Chat_Name ++"', '"++ User_ID ++ "', '" ++ Group_ID ++ "', '" ++ Msg ++ "', '" ++ integer_to_list(Status) ++ "', '" ++ Timestamp ++ "');")

            %% end;

        {fetch_user, Username, From} ->
            Content = odbc:sql_query(Ref, "SELECT username, password, timestamp FROM users WHERE username = '"++ Username ++ "';"),
            case Content of
                {selected,_,[]} ->
                    From ! {error, no_user};
                {selected,_,[{Username, Password, Timestamp}]} ->
                    From ! { Username, Password, stringify_timestamp( Timestamp)}
            end;

        {fetch_friendlist, Username, From} ->
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
            end;


        {fetch_chat, Chat_ID, From} ->
	 %%  io:format("------------ ~p~n", [Chat_ID]),
	    Content = (catch odbc:sql_query(Ref, "SELECT username, message, status FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
                   
            case Content of
                {error, Reason} ->
                    From ! {error, Reason};
                {_,_,Messages} ->
		    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
		    From ! {Chat_ID, Chat_Name, Messages}
            end;

        {fetch_chat_members, Chat_Name, From} ->
            Chat_ID = fetch_group_id(Ref, Chat_Name),
            
            case Chat_ID of
                {error, Reason} ->
                    From ! {error, Reason};
                _ ->
                    Content = (catch odbc:sql_query(Ref, "SELECT username FROM group_users WHERE group_id = '" ++ Chat_ID ++ "';")),
                    From ! Content
            end;

	{fetch_all_chats, Username, From} ->
	    {selected, _, Chat_IDS} = (catch odbc:sql_query(Ref, "SELECT group_id FROM group_users WHERE username = '" ++ Username ++ "';")),
	    All_Chats = fetch_all_chats_helper(Chat_IDS, [], Ref),
	    From ! All_Chats;

        stop ->
            odbc:disconnect(Ref),
            odbc:stop(),
            exit(succeful_exit);
        
        reset_tests ->
            odbc:sql_query(Ref, "DELETE FROM messages *;"),
            odbc:sql_query(Ref, "DELETE FROM group_users *;"),
            odbc:sql_query(Ref, "DELETE FROM groups *;"),
            odbc:sql_query(Ref, "DELETE FROM friendlist *;"),
            odbc:sql_query(Ref, "DELETE FROM users *;");

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



fetch_user_id(Ref, Username) ->
    ID = odbc:sql_query(Ref, "SELECT user_id FROM users WHERE username = '" ++ Username ++ "';"),
    case ID of  
        {selected, _, [{User_ID}]} ->
        User_ID;
        _ ->
            {error, "Invalid username"}
    end.
fetch_group_id(Ref, Groupname) ->
    ID = odbc:sql_query(Ref, "SELECT group_id FROM groups WHERE groupname = '" ++ Groupname ++ "';"),
    
    case ID of
        {selected, _, [{Group_ID}]} ->
            Group_ID;
        _ ->
	    {error, "Invalid_groupname"}
    end.

add_group_members(Ref, Group_ID, [Member]) ->
    M = fetch_user_id(Ref, Member),
    odbc:sql_query(Ref, "INSERT INTO group_users (group_id, user_id, username) VALUES ('" ++ Group_ID ++ "', '" ++ M ++ "', '" ++ Member ++ "');");

add_group_members(Ref, Group_ID, [X|Members]) ->
    M = fetch_user_id(Ref, X),
    odbc:sql_query(Ref, "INSERT INTO group_users (group_id, user_id, username) VALUES ('"++ Group_ID ++ "', '" ++ M ++ "', '" ++ X ++ "');"),
    add_group_members(Ref, Group_ID, Members).

fetch_all_chats_helper([{Chat_ID}], All_Chats, Ref) ->
    {_,_,Messages} = (catch odbc:sql_query(Ref, "SELECT username, message, status FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
    New_Chat_List = All_Chats ++ {Chat_ID, Chat_Name, Messages},
    New_Chat_List;

fetch_all_chats_helper([{Chat_ID}|Tail], All_Chats, Ref) ->
    {_,_,Messages} = (catch odbc:sql_query(Ref, "SELECT username, message FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
    {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
    New_Chat_List = All_Chats ++ {Chat_ID, Chat_Name, Messages},
    fetch_all_chats_helper(Tail, New_Chat_List, Ref).

%% PostgreSQL return TIMESTAMP as a multi-tuple e.g {{2020,03,19}, {13,7,0}} where the date and time are integers. This function converts that tuple to a string on format "2020-03-10 13:7:0". Maybe it would be easier to compare different timestamps if we keep them as integers???
stringify_timestamp({Date,Time}) ->
    {Year,Month,Day} = Date,
    {Hour,Minute,Second} = Time,
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).
