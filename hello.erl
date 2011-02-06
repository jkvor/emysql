	-module(hello).
	-export([run/0]).

	run() ->

		crypto:start(),
		application:start(emysql),

		TO = application:get_env(emysql, default_timeout),
		io:format("time out: ~p~n", [TO]),

		emysql:add_pool(hello_pool, 1,
			"hello_username", "hello_password", "localhost", 3306,
			"hello_database", utf8),

		emysql:execute(hello_pool,
			<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	    Result = emysql:execute(hello_pool,
    		<<"select hello_text from hello_table">>),

		io:format("~n~p~n", [Result]).
		