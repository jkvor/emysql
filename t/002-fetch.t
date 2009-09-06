#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-record(foo, {extra, name, id}).

main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	
	application:start(crypto),
	application:load(emysql),
	application:set_env(emysql, pools, [
		{test1, [
			{size, 1},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),	
	application:start(emysql),

	ShowTables = mysql:execute(test1, "SHOW TABLES"),
	[mysql:execute(test1, "DROP TABLE " ++ Table) || [Table] <- ShowTables:rows()],

	Foo = mysql:execute(test1, "CREATE TABLE foo (id int(32) NOT NULL AUTO_INCREMENT, name varchar(50) NOT NULL, falafel varchar(20) NULL, hummus float DEFAULT 0.0, PRIMARY KEY (id))"),
	etap:is(is_record(Foo, mysql_ok_packet), true, "create table returned ok packet"),
	
	[begin
		Abc = mysql:execute(test1, "INSERT INTO foo (name) VALUES ('abc" ++ integer_to_list(I) ++ "')"),
		etap:is(Abc:insert_id(), I, "auto increment value ok")
	 end || I <- lists:seq(1,5)],

	AllFoo = mysql:execute(test1, "SELECT * FROM foo"),
	io:format("~p~n", [AllFoo:field_list()]),
	io:format("~p~n", [AllFoo:rows()]),
	A = AllFoo:as_record(foo, record_info(fields, foo)),
	io:format("~p~n", [A]),
	
    etap:end_tests().
