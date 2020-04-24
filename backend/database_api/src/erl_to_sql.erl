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
	    odbc:sql_query(Ref, "INSERT INTO users VALUES('" ++ Username ++ "', '" ++ Password ++ "' ,'" ++ Timestamp ++"')");

	{insert_friend, Username, Friend, From} ->
	    UserExists = (catch odbc:sql_query(Ref, "SELECT * FROM users WHERE user_name = '" ++ Username ++ "';")),
	    FriendExists = (catch odbc:sql_query(Ref, "SELECT * FROM users WHERE user_name = '" ++ Friend ++ "';")),
	    case UserExists of
		{selected, _, []} ->
		    From ! {error, no_user},
		    loop(Ref);
		_ ->
		    ok
	    end,
	    case FriendExists of
		{selected, _, []} ->
		    From ! {error, no_friend};
		_ ->
		    odbc:sql_query(Ref, "INSERT INTO friend_list VALUES('" ++ Username ++ "', '" ++ Friend ++ "')"),
		    From ! ok
	    end;
	
	{insert_chat, From_Username, Chat_ID, { Timestamp, Msg}, Status} ->
	    
	    odbc:sql_query(Ref, "CREATE TABLE IF NOT EXISTS chat" ++ Chat_ID ++ "  (from_user VARCHAR(50) NOT NULL, message TEXT NOT NULL, status INT, time_stamp TIMESTAMP NOT NULL);"),

	    odbc:sql_query(Ref, "INSERT INTO chat" ++ Chat_ID ++ " VALUES ('"++ From_Username ++ "', '" ++ Msg ++ "', '" ++ integer_to_list(Status) ++ "', '" ++ Timestamp ++ "');");

	{fetch_user, Username, From} ->
	    Content = odbc:sql_query(Ref, "SELECT user_name, password, registered FROM users WHERE user_name = '"++ Username ++ "';"),
	    case Content of
		{selected,_,[]} ->
		    From ! {error, no_user};
		{selected,_,[{Username, Password, Timestamp}]} ->
		    From ! { Username, Password, stringify_timestamp( Timestamp)}
	    end;

	{fetch_friendlist, Username, From} ->
	    Content = (catch odbc:sql_query(Ref, "SELECT friend FROM friend_list WHERE user_name = '" ++ Username ++ "';")),
	    case Content of
		{selected,_,[]} ->
		    From ! {error, no_friendlist};
		{selected,_,Friendlist} ->
		    From ! {ok, Friendlist}
	    end;


	{fetch_chat, Chat_ID, From} ->
	    Content = (catch odbc:sql_query(Ref, "SELECT from_user, message, status FROM chat" ++ Chat_ID ++ ";")),
	    From ! Content;

	{fetch_chat_members, Chat_ID, From} ->
	    Content = (catch odbc:sql_query(Ref, "SELECT from_user FROM chat" ++ Chat_ID ++ ";")),
	    From ! Content;

	stop ->
	    odbc:disconnect(Ref),
	    odbc:stop(),
	    exit(succeful_exit);

	{remove_table, Table} ->
	    odbc:sql_query(Ref, "DROP TABLE chat" ++ Table ++ ";");

	{remove_user, Username} ->
	    odbc:sql_query(Ref, "DELETE FROM users WHERE user_name = '" ++ Username ++ "';");

	{remove_friendlist, Username} ->
	    odbc:sql_query(Ref, "DELETE FROM friend_list WHERE user_name = '" ++ Username ++ "';");
	
        Msg ->
            io:format("database_api:loop/1 Unhandled message: ~p~n", [Msg])
    end,
    loop(Ref).

%% PostgreSQL return TIMESTAMP as a multi-tuple e.g {{2020,03,19}, {13,7,0}} where the date and time are integers. This function converts that tuple to a string on format "2020-03-10 13:7:0". Maybe it would be easier to compare different timestamps if we keep them as integers???
stringify_timestamp({Date,Time}) ->
    {Year,Month,Day} = Date,
    {Hour,Minute,Second} = Time,
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).
