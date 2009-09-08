#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").
-record(foo, {extra, name, id}).

main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	application:start(crypto),
	mysql_statements:start_link(),
	mysql_conn_mgr:start_link(test1, 1, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),

	Foo = mysql:execute(test1, "CREATE TABLE foo (
									id int(32) NOT NULL AUTO_INCREMENT, 
									name varchar(50) NOT NULL, 
									falafel varchar(20) NULL, 
									hummus float DEFAULT 0.0, 
									PRIMARY KEY (id)
								)"),
	etap:is(is_record(Foo, mysql_ok_packet), true, "create table returned ok packet"),
	
	[begin
		Abc = mysql:execute(test1, "INSERT INTO foo (name) VALUES ('abc" ++ integer_to_list(I) ++ "')"),
		etap:is(Abc:insert_id(), I, "auto increment value ok")
	 end || I <- lists:seq(1,5)],

	AllFoo = mysql:execute(test1, "SELECT * FROM foo"),
	ExpectedRows = [
		[1,<<"abc1">>,undefined,0],
        [2,<<"abc2">>,undefined,0],
        [3,<<"abc3">>,undefined,0],
        [4,<<"abc4">>,undefined,0],
        [5,<<"abc5">>,undefined,0]
	],
	etap:is(AllFoo:rows(), ExpectedRows, "row data matches"),

	AllRecs = AllFoo:as_record(foo, record_info(fields, foo)),
	ExpectedRecs = [
	 {foo,undefined,<<"abc1">>,1},
	 {foo,undefined,<<"abc2">>,2},
	 {foo,undefined,<<"abc3">>,3},
	 {foo,undefined,<<"abc4">>,4},
	 {foo,undefined,<<"abc5">>,5}
	],
	etap:is(AllRecs, ExpectedRecs, "record data matches"),
	
    etap:end_tests().
