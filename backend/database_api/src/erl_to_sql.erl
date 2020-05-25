-module(erl_to_sql).

-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([insert_user/3, insert_friend/2, insert_chat/4, create_chat/3, remove_table/1, remove_user/1,
         remove_friendlist/1, fetch_user/1, fetch_friendlist/1, fetch_chat/1, fetch_chat_members/1,
         fetch_all_chats/1, get_group_id/1, reset_tests/0, create_thread/5, fetch_thread/1,
         fetch_thread_IDs/0, insert_comment/6, stop/0]).

-define(DSN, "PostgreSQL test").
-define(UID, "adrenaline").
-define(PWD, "1234").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% server implementation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used to start server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% called when server starts up
init(_Args) ->
    process_flag(trap_exit, true),
    odbc:start(),
    {ok, _DBRef} = odbc:connect("DSN="++?DSN++";UID="++?UID++";PWD="++?PWD, []).

% called before server shuts down
terminate(_Reason, DBRef) ->
    odbc:disconnect(DBRef).

% not used but required by `gen_server`
handle_cast(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%
%% server interface %%
%%%%%%%%%%%%%%%%%%%%%%

-type db_value_result() :: {ok, term()} | {error, term()}.
-type db_status_result() :: ok | {error, term()}.

-spec insert_user(string(), string(), string()) -> db_status_result().
insert_user(Username, Password, Timestamp) ->
    gen_server:call(?MODULE, {insert_user, Username, Password, Timestamp}).

-spec insert_friend(string(), string()) -> db_status_result().
insert_friend(Username, Friend) ->
    gen_server:call(?MODULE, {insert_friend, Username, Friend}).

-spec insert_chat(string(), string(), {string(), string()}, integer()) -> db_status_result().
insert_chat(From_Username, Chat_ID, {Timestamp, Msg}, Status) ->
    gen_server:call(?MODULE, {insert_chat, From_Username, Chat_ID, Timestamp, Msg, Status}).

-spec create_chat(string(), string(), list(string())) -> db_status_result().
create_chat(Chat_Name, Creator, Members) ->
    gen_server:call(?MODULE, {create_chat, Chat_Name, Creator, Members}).

-spec remove_table(string()) -> db_status_result().
remove_table(Table) -> 
    gen_server:call(?MODULE, {remove_table, Table}).

-spec remove_user(string()) -> db_status_result().
remove_user(Username) ->
    gen_server:call(?MODULE, {remove_user, Username}).

-spec remove_friendlist(string()) -> db_status_result().
remove_friendlist(Username) ->
    gen_server:call(?MODULE, {remove_friendlist, Username}).

-spec fetch_user(string()) -> db_value_result().
fetch_user(Username) ->
    gen_server:call(?MODULE, {fetch_user, Username}).

-spec fetch_friendlist(string()) -> db_value_result().
fetch_friendlist(Username) ->
    gen_server:call(?MODULE, {fetch_friendlist, Username}).

-spec fetch_chat(string()) -> db_value_result().
fetch_chat(Chat_ID) ->
    gen_server:call(?MODULE, {fetch_chat, Chat_ID}).

-spec fetch_chat_members(string()) -> db_value_result().
fetch_chat_members(Chat_ID) ->
    gen_server:call(?MODULE, {fetch_chat_members, Chat_ID}).

-spec fetch_all_chats(string()) -> db_value_result().
fetch_all_chats(Username) ->
    gen_server:call(?MODULE, {fetch_all_chats, Username}).

-spec get_group_id(string()) -> string() | {error, term()}.
get_group_id(Username) ->
    gen_server:call(?MODULE, {get_group_id, Username}).

-spec reset_tests() -> db_status_result().
reset_tests() ->
    gen_server:call(?MODULE, reset_tests).

-spec create_thread(string(), string(), string(), string(), string()) -> db_status_result().
create_thread(Username, Server, Header, Text, Timestamp) ->
    gen_server:call(?MODULE, {create_thread, Username, Server, Header, Text, Timestamp}).

-spec fetch_thread(string()) -> db_value_result().
fetch_thread(Thread_ID) ->
    gen_server:call(?MODULE, {fetch_thread, Thread_ID}).

-spec fetch_thread_IDs() -> db_value_result().
fetch_thread_IDs() ->
    gen_server:call(?MODULE, fetch_thread_ids).

-spec insert_comment(string(), string(), string(), string(), string(), string()) -> db_status_result().
insert_comment(Thread_ID, Parent_ID, Reply_ID, Username, Text, Timestamp) ->
    gen_server:call(?MODULE, {insert_comment, Thread_ID, Parent_ID, Reply_ID, Username, Text, Timestamp}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Write all main functionality to communicate with Postgres below  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({insert_user, Username, Password, Timestamp}, _From, Ref) ->
    Status = (catch odbc:sql_query(Ref, "INSERT INTO users (username, password, timestamp) VALUES('" ++ Username ++ "', '" ++ Password ++ "' ,'" ++ Timestamp ++"');")),
    case Status of
        {updated, 1} ->
            {reply, ok, Ref};
        {error, Reason} ->
            {reply, {error, Reason}, Ref};
        Msg ->
            {reply, {error, Msg}, Ref}
    end;

handle_call({insert_friend, Username, Friend}, _From, Ref) ->
    User_ID = fetch_user_id(Ref, Username),
    Friend_ID = fetch_user_id(Ref, Friend),

    case User_ID of
        {error, Reason} ->
            {reply, {error, Reason}, Ref};
        _ ->
            case Friend_ID of
                {error, Reason2} ->
                    {reply, {error, Reason2}, Ref};
                _ ->
                    odbc:sql_query(Ref, "INSERT INTO friendlist (user_id, friend_id, username, friendname, status) VALUES('" ++ User_ID ++ "', '" ++ Friend_ID ++ "', '" ++ Username ++ "', '" ++ Friend ++ "', 0);"),
                    {reply, ok, Ref}
            end
    end;
        
handle_call({create_chat, Chat_Name, _, Members}, _From, Ref) ->
    Status = (catch odbc:sql_query(Ref, "INSERT INTO groups (groupname) VALUES ('"++ Chat_Name ++ "');")),
    case Status of
        {error, Reason} ->
            {reply, {error, Reason}, Ref};
        {updated, 1} ->
            Chat_ID = fetch_group_id(Ref, Chat_Name),
            case Chat_ID of
                {error, Reason2} ->
                    {reply, {error, Reason2}, Ref};
                _ ->
                    add_group_members(Ref, Chat_ID, Members),
                    {reply, {ok, Chat_ID}, Ref}
            end
    end;

handle_call({insert_chat, From_Username, Chat_ID, Timestamp, Msg, Status}, _From, Ref) ->
    User_ID = fetch_user_id(Ref, From_Username),
    Success = (catch odbc:sql_query(Ref, "INSERT INTO messages (username, user_id, group_id, message, status, timestamp) VALUES ('"++ From_Username ++"', '"++ User_ID ++ "', '" ++ Chat_ID ++ "', '" ++ Msg ++ "', '" ++ integer_to_list(Status) ++ "', '" ++ Timestamp ++ "');")),
    case Success of
        {updated, 1} ->
            {reply, ok, Ref};
        {error, Reason} ->
            {reply, {error, Reason}, Ref}
    end;

handle_call({fetch_user, Username}, _From, Ref) ->
    Content = odbc:sql_query(Ref, "SELECT username, password, timestamp FROM users WHERE username = '"++ Username ++ "';"),
    case Content of
        {selected,_,[]} ->
            {reply, {error, no_user}, Ref};
        {selected,_,[{Username, Password, Timestamp}]} ->
            {reply, {Username, Password, stringify_timestamp(Timestamp)}, Ref}
    end;

handle_call({fetch_friendlist, Username}, _From, Ref) ->
    User_ID = fetch_user_id(Ref, Username),
    case User_ID of
        {error, _} ->
            {reply, {error, no_user}, Ref};
        _ ->
            Content = (catch odbc:sql_query(Ref, "SELECT friendname FROM friendlist WHERE user_id = '" ++ User_ID ++ "';")),
            case Content of
                {selected,_,[]} ->
                    {reply, {error, no_friendlist}, Ref};
                {selected,_,Friendlist} ->
                    {reply, {ok, Friendlist}, Ref}
            end
    end;

handle_call({fetch_chat, Chat_ID}, _From, Ref) ->
    Content = (catch odbc:sql_query(Ref, "SELECT username, message, status FROM messages WHERE group_id = '" ++ Chat_ID ++ "';")),
    case Content of
        {error, Reason} ->
            {reply, {error, Reason}, Ref};
        {_,_,Messages} ->
            {selected,_,[{Chat_Name}]} = odbc:sql_query(Ref, "SELECT groupname FROM groups WHERE group_id = '" ++ Chat_ID ++ "';"),
            {reply, {Chat_ID, Chat_Name, Messages}, Ref}
    end;

handle_call({fetch_chat_members, Chat_ID}, _From, Ref) ->
    Content = (catch odbc:sql_query(Ref, "SELECT username FROM group_users WHERE group_id = '" ++ Chat_ID ++ "';")),
    {reply, Content, Ref};

handle_call({fetch_all_chats, Username}, _From, Ref) ->
    {selected, _, Chat_IDS} = (catch odbc:sql_query(Ref, "SELECT group_id FROM group_users WHERE username = '" ++ Username ++ "';")),
    case Chat_IDS of 
        [] ->
            {reply, {error, "No chats found in database"}, Ref};
        _ ->
            All_Chats = fetch_all_chats_helper(Chat_IDS, [], Ref),
            {reply, {ok, All_Chats}, Ref}
    end;

handle_call({create_thread, Username, Server, Header, Text, Timestamp}, _From, Ref) ->
    User_ID = fetch_user_id(Ref, Username),
    case User_ID of
        {error, Reason1} ->
            {reply, {error, Reason1}, Ref};
        _ ->
            Status = (catch odbc:sql_query(Ref, "INSERT INTO thread (username, user_id, server_id, root_header, root_text, timestamp) VALUES ('" ++ Username ++ "', '" ++ User_ID ++ "', " ++ Server ++ ", '" ++ Header ++ "', '" ++ Text ++ "', '" ++ Timestamp ++ "');")),
            case Status of
                {updated, 1} ->
                    NewStatus = odbc:sql_query(Ref, "SELECT thread_id FROM thread WHERE (username = '" ++ Username ++ "' AND timestamp = '" ++ Timestamp ++ "');"),
                    case NewStatus of
                        {selected,_,[{Thread_ID}]}->
                            odbc:sql_query(Ref, "INSERT INTO threadlist (server_id, thread_id) VALUES ('" ++ Server ++ "', '" ++ Thread_ID ++ "');"),
                            {reply, {ok, Thread_ID}, Ref};
                        {selected,_,Thread_IDS}->
                            [{ID} | _Tail] = lists:reverse(Thread_IDS),
                            odbc:sql_query(Ref, "INSERT INTO threadlist (server_id, thread_id) VALUES ('" ++ Server ++ "', '" ++ ID ++ "');"),
                            {reply, {ok, ID}, Ref}
                    end;
                {error, Reason2} ->
                    {reply, {error, Reason2}, Ref}
            end
    end;

handle_call({fetch_thread, Thread_ID}, _From, Ref) ->
    {selected, _, [Thread]} = (catch odbc:sql_query(Ref, "SELECT server_id, username, root_header, root_text, timestamp FROM thread WHERE thread_id = " ++ Thread_ID ++ ";")),
    case Thread of 
        [] ->
            {reply, {error, "Thread not found in database"}, Ref};
        {Server, Creator, Header, Text, Timestamp} ->
            {reply, {ok, {Server, Creator, Header, Text, stringify_timestamp(Timestamp), fetch_all_comments(Ref, Thread_ID)}}, Ref}
    end;

handle_call(fetch_thread_ids, _From, Ref) ->
    {selected, _, Threadlist} = (catch odbc:sql_query(Ref, "SELECT thread_id FROM threadlist;")),
    {reply, {ok, Threadlist}, Ref};

handle_call({insert_comment, Thread_ID, Index, Reply_Index, Username, Text, Timestamp}, _From, Ref) ->
    User_ID = fetch_user_id(Ref, Username),
    case User_ID of
        {error, Reason1} ->
            {reply, {error, Reason1}, Ref};
        _ ->
            Status = case Reply_Index of
                         "" ->
                             (catch odbc:sql_query(Ref, "INSERT INTO commentlist (thread_id, user_id, username, index, text, timestamp) VALUES (" ++ Thread_ID ++ ", " ++ User_ID ++ ", '" ++ Username ++ "', " ++ Index ++ ", '" ++ Text ++ "', '" ++ Timestamp ++ "');"));
                         _ ->
                             (catch odbc:sql_query(Ref, "INSERT INTO commentlist (thread_id, user_id, username, index, reply_index, text, timestamp) VALUES (" ++ Thread_ID ++ ", " ++ User_ID ++ ", '" ++ Username ++ "', " ++ Index ++ ", " ++ Reply_Index ++ ", '" ++ Text ++ "', '" ++ Timestamp ++ "');"))
                     end,
            case Status of
                {updated, 1} ->
                    {selected,_,[{Comment_ID}]} =  odbc:sql_query(Ref, "SELECT commentlist_id FROM commentlist WHERE (thread_id = " ++ Thread_ID ++ " AND index = " ++ Index ++ ");"),
                    Comment = fetch_comment(Ref, Comment_ID),
                    {reply, {ok, Comment}, Ref};
                {error, Reason2} ->
                    {reply, {error, Reason2}, Ref}
            end
    end;

handle_call({get_group_id, Username}, _From, Ref) ->
    Result = fetch_group_id(Ref, Username),
    {reply, Result, Ref};

handle_call(reset_tests, _From, Ref) ->
    odbc:sql_query(Ref, "DELETE FROM messages * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM messages * WHERE username = 'testfriend1';"),
    odbc:sql_query(Ref, "DELETE FROM group_users * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM group_users * WHERE username = 'testfriend1';"),
    odbc:sql_query(Ref, "DELETE FROM groups * WHERE groupname = 'festchatten';"),
    odbc:sql_query(Ref, "DELETE FROM groups * WHERE groupname = 'skolchatten';"),
    odbc:sql_query(Ref, "DELETE FROM friendlist * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM friendlist * WHERE username = 'testfriend1';"),
    odbc:sql_query(Ref, "DELETE FROM commentlist * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM commentlist * WHERE username = 'testfriend1';"),
    odbc:sql_query(Ref, "DELETE FROM threadlist * WHERE thread_id in (SELECT thread_id FROM thread WHERE username = 'testuser1');"),
    odbc:sql_query(Ref, "DELETE FROM thread * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM users * WHERE username = 'testuser1';"),
    odbc:sql_query(Ref, "DELETE FROM users * WHERE username = 'testfriend1';"),
    {reply, ok, Ref}.


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


fetch_comment(Ref, Comment_ID) ->
    Status = (catch odbc:sql_query(Ref, "SELECT commentlist_id, thread_id, index, reply_index, username, text, timestamp FROM commentlist WHERE commentlist_id = " ++ Comment_ID ++ ";")),

    case Status of
	{selected, _, []} ->
	    erlang:error("fetch_comment/2: No Comment with that ID.");
	{selected, _, [Comment]} ->
	    refactor_comment(Ref,Comment);
	Error ->
	    io:format("~p~n",[Error]),
	    erlang:error("fetch_comment/: Postgres failed for unkown reason.")
    end.

fetch_all_comments(Ref, Thread_ID) ->
    Status = (catch odbc:sql_query(Ref, "SELECT commentlist_id, thread_id, index, reply_index, username, text, timestamp FROM commentlist WHERE thread_id = '" ++ Thread_ID ++ "';")),

    case Status of
	{selected, _, []} ->
	    [];
	{selected, _, Commentlist} ->
	    Refactored_Comments = [refactor_comment(Ref,E) || E <- Commentlist],
	    Refactored_Comments;
	Error ->
	    io:format("fetch_all_comments/2: CommentList = ~p~n", [Error]),
	    Error
    end.


%% PostgreSQL return TIMESTAMP as a multi-tuple e.g {{2020,03,19}, {13,7,0}} where the date and time are integers. This function converts that tuple to a string on format "2020-03-10 13:7:0". Maybe it would be easier to compare different timestamps if we keep them as integers???
stringify_timestamp({Date,Time}) ->
    {Year,Month,Day} = Date,
    {Hour,Minute,Second} = Time,
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day) ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).


refactor_comment(Ref, {_Comment_ID, Thread_ID, _Index, Reply_Index, Username, Text, _Timestamp}) ->    
    case Reply_Index of
	null ->
	    {Thread_ID, Username, Text, {"", ""}};
	_ ->
	    {selected, _, [{Reply_User, Reply_Text}]} = (catch odbc:sql_query(Ref, "SELECT username, text FROM commentlist WHERE (thread_id = " ++ Thread_ID ++ " AND index =  " ++ Reply_Index ++ ");")),
	    {Thread_ID, Username, Text, {Reply_User, Reply_Text}}
    end.


