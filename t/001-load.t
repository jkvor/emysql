#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").

main(_) ->
    etap:plan(unknown),
	
	etap_application:load_ok(crypto, "Application 'crypto' loaded"),
	etap_application:load_ok(emysql, "Application 'emysql' loaded"),
	
	[etap_can:loaded_ok(Module, lists:concat(["Module '", Module, "' loaded."])) || Module <- mysql:modules()],

	application:set_env(emysql, pools, [
		{test1, [
			{size, 2},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]},
		{test2, [
			{size, 3},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),

	etap_application:start_ok(crypto, "Application 'crypto' started"),
	etap_application:start_ok(emysql, "Application 'emysql' started"),
	
	%% CHECK INITIAL STATE FOR CORRECT POOL AND CONNECTION RECORDS
	(fun() ->
		Pools = mysql_conn_mgr:pools(),
		etap:is(length(Pools), 2, "state contains correct number of pools"),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		{value, Pool2} = lists:keysearch(test2, 2, Pools),
		etap:is(is_record(Pool1, pool) andalso is_record(Pool2, pool), true, "state contains pool records"),
		etap:is(length(Pool1#pool.connections), 2, "pool1 contains correct number of connections"),
		etap:is(length(Pool2#pool.connections), 3, "pool2 contains correct number of connections"),
		[etap:is(Connection#connection.state, available, "pool1 connection is in available state") || Connection <- Pool1#pool.connections],
		[etap:is(Connection#connection.state, available, "pool2 connection is in available state") || Connection <- Pool2#pool.connections],
		ok
	 end)(),
	
	(fun() ->
		Conn1 = mysql_conn_mgr:lock_connection(test1),
		Pools = mysql_conn_mgr:pools(),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		[if
			C =:= Conn1 ->
				etap:is(C#connection.state, locked, "locked connection is locked");
			true ->
				etap:is(C#connection.state, available, "unlocked connection is available")
		 end || C <- Pool1#pool.connections],
		ok
	 end)(),
	
	(fun() ->
		Conn2 = mysql_conn_mgr:lock_connection(test1),
		Pools = mysql_conn_mgr:pools(),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		[etap:is(C#connection.state, locked, "connection is locked") || C <- Pool1#pool.connections],
		etap:is(mysql_conn_mgr:lock_connection(test1), unavailable, "all connections are locked"),
		ok
	 end)(),

	etap:is((catch mysql_conn_mgr:lock_connection(undefined)), {'EXIT', pool_not_found}, "pool_not_found error returned successfully"),
	etap:is((catch mysql_conn_mgr:unlock_connection(#connection{pool_id=test1})), {'EXIT', connection_not_found}, "connection_not_found error returned successfully"),
		
	application:stop(emysql),
	application:load(emysql),
	application:set_env(emysql, pools, [
		{test1, [
			{size, 0},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),	
	application:start(emysql),
	etap:is((catch mysql_conn_mgr:lock_connection(test1)), {'EXIT', connection_pool_is_empty}, "connection_pool_is_empty error returned successfully"),

    etap:end_tests().
