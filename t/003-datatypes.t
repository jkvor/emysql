#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").

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


	TblDef = "CREATE TABLE foo (" ++
		"foo_dec DECIMAL," ++
		"foo_tiny TINYINT," ++
		"foo_long LONG," ++
		"foo_float FLOAT," ++
		"foo_double DOUBLE," ++
		"foo_timestamp TIMESTAMP," ++
		"foo_int INT," ++
		"foo_date DATE," ++
		"foo_time TIME," ++
		"foo_datetime DATETIME," ++
		"foo_year YEAR," ++
		"foo_varchar VARCHAR(255)," ++
		"foo_bit BIT," ++
		"foo_blob BLOB )",
	Foo = mysql:execute(test1, TblDef),
	etap:is(is_record(Foo, mysql_ok_packet), true, "create table ok"),
	
	FooInsert = "INSERT INTO foo VALUES (
		1.0,
		2,
		999999999,
		1.333333333,
		1.33333333333333333333,
		'2009-01-01 12:12:12',
		100,
		'2009-01-01',
		'12:12:12',
		'2009-01-01 12:12:12',
		2009,
		'asdf',
		1,
		'asdf'
	)",
	Insert = mysql:execute(test1, FooInsert),
	etap:is(is_record(Insert, mysql_ok_packet), true, "insert data ok"),

	Select = mysql:execute(test1, "SELECT * FROM foo"),
	[Row] = Select:rows(),
	
	etap:is(lists:nth(1, Row), 1, "decimal matches"),
	etap:is(lists:nth(2, Row), 2, "tinyint matches"),
	% etap:is(lists:nth(3, Row), 999999999, "long matches"),
	% etap:is(lists:nth(4, Row), 1.333333333, "float matches"),
	% etap:is(lists:nth(5, Row), 1.33333333333333333333, "double matches"),
	% etap:is(lists:nth(6, Row), 6, "timestamp matches"),
	% etap:is(lists:nth(7, Row), 7, "int matches"),
	% etap:is(lists:nth(8, Row), 8, "date matches"),
	% etap:is(lists:nth(9, Row), 1, "time matches"),
	% etap:is(lists:nth(10, Row), 1, "datetime matches"),
	% etap:is(lists:nth(11, Row), 1, "year matches"),
	% etap:is(lists:nth(12, Row), 1, "varchar matches"),
	% etap:is(lists:nth(13, Row), 1, "bit matches"),
	% etap:is(lists:nth(14, Row), 1, "blob matches"),
	
	
    etap:end_tests().
