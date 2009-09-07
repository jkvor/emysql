#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").

main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	application:start(crypto),
	mysql_conn_mgr:start_link(test1, 4, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),
	
	Foo = mysql:execute(test1, "CREATE TABLE foo (
									id int(32) NOT NULL AUTO_INCREMENT, 
									name varchar(50) NOT NULL, 
									PRIMARY KEY (id)
								)"),
	etap:is(is_record(Foo, mysql_ok_packet), true, "create table returned ok packet"),
	
	etap:is(mysql:prepare(create_foo, "INSERT INTO foo (name) VALUES (?)"), ok, "prepared statements"),
	
	#state{pools=[Pool], statements=Stmts} = mysql_conn_mgr:info(),
	
	etap:is(gb_trees:to_list(Stmts), [{create_foo, "INSERT INTO foo (name) VALUES (?)"}], "statements in state match"),
	
	[begin
		etap:is(is_record(mysql_conn:execute(Conn, create_foo, ["conn " ++ Conn#connection.id]), mysql_ok_packet), true, "execute prepared stmt ok")
	 end || Conn <- Pool#pool.connections],
	
	etap:is(length((mysql:execute(test1, "SELECT * FROM foo")):rows()), 4, "correct number of rows were inserted"),
	
	etap:is(mysql:prepare(foo_all, "SELECT * FROM foo"), ok, "prepared statement"),
	etap:is(mysql:prepare(foo_by_id_name, "SELECT * FROM foo WHERE id = ? and name like ?"), ok, "prepared statement"),
	
	etap:is(mysql:execute(test1, foo_all), mysql:execute(test1, foo_all, []), "statements with and without empty args list matches"),
	etap:is(mysql:execute(test1, "SELECT * FROM foo"), mysql:execute(test1, "SELECT * FROM foo", []), "queries with and without empty args list matches"),
	
	etap:is(
		mysql:execute(test1, foo_by_id_name, [1, "conn%"]), 
		mysql:execute(test1, "SELECT * FROM foo WHERE id = 1 and name like 'conn%'"),
		"statement and query with same args return matching results"
	),
	
	etap:end_tests().
	