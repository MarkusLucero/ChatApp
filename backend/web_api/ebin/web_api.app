{application, 'web_api', [
	{description, "A web api for Chat Up!"},
	{vsn, "0.1.0"},
	{modules, ['auth_handler','chat_server','database_api','erl_to_sql','message_parser','mochijson','mochijson2','web_api_app','web_api_handler','web_api_sup']},
	{registered, [web_api_sup]},
	{applications, [kernel,stdlib,odbc,cowboy]},
	{mod, {web_api_app, []}},
	{env, []}
]}.