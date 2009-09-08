#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").

main(_) ->
    etap:plan(14),
	error_logger:tty(false),
	application:start(crypto),
	mysql_statements:start_link(),
	mysql_conn_mgr:start_link(test1, 2, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),
	
	spawn(fun() -> mysql:execute(test1, "SELECT SLEEP(1)"), etap:is(1,1,"pid1 finished sleeping") end),
	spawn(fun() -> mysql:execute(test1, "SELECT SLEEP(1)"), etap:is(1,1,"pid2 finished sleeping") end),
	
	timer:sleep(10),
	
	etap:is((mysql:execute(test1, "show tables")):rows(), [], "fetched empty table list"),
	
	timer:sleep(1000),
	
	[etap:is(Conn#connection.state, available, "connection unlocked") || Conn <- (hd(mysql_conn_mgr:pools()))#pool.connections],
	
	etap:is(mysql_conn_mgr:waiting(), queue:new(), "waiting queue is empty"),
	
	spawn(fun() -> etap:is((catch mysql:execute(test1, "SELECT SLEEP(10)")), {'EXIT',mysql_timeout}, "timeout ok") end),
	spawn(fun() -> etap:is((catch mysql:execute(test1, "SELECT SLEEP(10)")), {'EXIT',mysql_timeout}, "timeout ok") end),
	
	timer:sleep(10),
	
	etap:is((catch mysql:execute(test1, "show tables")), {'EXIT',connection_lock_timeout}, "timed out waiting for connection"),
		
	[etap:is(Conn#connection.state, locked, "connection locked") || Conn <- (hd(mysql_conn_mgr:pools()))#pool.connections],
	etap:is(mysql_conn_mgr:waiting(), queue:new(), "waiting queue is empty"),
		
	timer:sleep(5000),
	
	[etap:is(Conn#connection.state, available, "connection unlocked") || Conn <- (hd(mysql_conn_mgr:pools()))#pool.connections],
	
	etap:end_tests().