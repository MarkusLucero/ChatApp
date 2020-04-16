{application, 'web_api', [
	{description, "A web api for Chat Up!"},
	{vsn, "0.1.0"},
	{modules, ['chat_server','web_api_app','web_api_handler','web_api_sup']},
	{registered, [web_api_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {web_api_app, []}},
	{env, []}
]}.