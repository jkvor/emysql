%%%-------------------------------------------------------------------
%%% File     : Emysql/test/unicode_SUITE.erl
%%% Descr    : Suite #3: Test for unicode value conversions
%%% Author   : H. Diedrich
%%% Created  : 12/14/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/: 
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(unicode_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

-record(hello_record, {hello_text}).

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,60}}].

%% Mandatory list of test cases and test groups, and skip orders. 
%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------

all() -> 
    [delete_all,
     insert_only,
	 insert_only_unicode_binary,
	 insert_only_unicode_liststring,
	 insert_and_read_back_as_recs,
	 select_by_prepared_statement,

	 % these tests are easiest to read
	 straighttalk_write_binary_ascii_directly,
	 straighttalk_write_binary_unicode_directly,
	 straighttalk_write_binary_all_ascii_as_unicode_directly,
	 straighttalk_write_binary_unicode_as_ascii_directly,
	 straighttalk_write_liststring_ascii_directly,
	 straighttalk_write_liststring_unicode_directly,
	 straighttalk_write_liststring_all_ascii_as_unicode_directly,
	 straighttalk_write_liststring_unicode_as_ascii_directly,
	 straighttalk_write_binary_ascii_via_statement,
	 straighttalk_write_binary_unicode_via_statement,
	 straighttalk_write_binary_all_ascii_as_unicode_via_statement,
	 straighttalk_write_binary_unicode_as_ascii_via_statement,
	 straighttalk_write_liststring_ascii_via_statement,
	 straighttalk_write_liststring_unicode_via_statement,
	 straighttalk_write_liststring_all_ascii_as_unicode_via_statement,
	 straighttalk_write_liststring_unicode_as_ascii_via_statement,
	 
	 % these are tests using nested worker functions
	 test_read_back_directly_function,
	 test_read_back_by_statement_function,
	 test_stmt_and_read_back_directly_function,
	 test_stmt_and_read_back_stmt_function,
	 test_ascii_binary_direct,
	 test_ascii_binary_via_parameter,
	 test_ascii_liststring_via_parameter,
	 test_ascii_binary,
	 test_ascii_liststring,
	 test_unicode_binary,
	 test_unicode_liststring,
	 
	 test_ascii_quote_as_binary,
	 test_ascii_quote_as_liststring,
	 test_unicode_quote_as_binary,
	 test_unicode_quote_as_liststring,
	 test_ascii_quote_and_text_as_binary,
	 test_ascii_quote_and_text_as_liststring,
	 test_unicode_quote_and_text_as_binary,
	 test_unicode_quote_and_text_as_liststring,
	 test_ascii_quote_and_trailing_text_as_binary,
	 test_ascii_quote_and_trailing_text_as_liststring,
	 test_unicode_quote_and_trailing_text_as_binary,
	 test_unicode_quote_and_trailing_text_as_liststring,
	 
	 test_worker_quartet_single_functions,
	 test_worker_quartet,
	 test_escaping,
	 test_escaping_with_non_ascii,
	 test_escaping_with_non_ascii_leading_umlaut
	 ].

     % plain_parameters,
     % binary_parameters]

%% Optional suite pre test initialization
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------

init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),

	emysql:prepare(stmt_insert, 
		<<"INSERT INTO hello_table SET hello_text = ?">>),

	emysql:prepare(stmt_select, 
		<<"SELECT * FROM hello_table">>),

    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
	emysql:remove_pool(test_pool),
    ok.

%%--------------------------------------------------------------------
%%
%% Human Understandable Tests.
%%
%%--------------------------------------------------------------------
%% These tests are the easiest to read.
%% They also serve to make sure that at least some tests don't
%% completely miss the point for cleverness.

straighttalk_write_binary_ascii_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,

    ok.


straighttalk_write_binary_unicode_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"/utf8>>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello Wørld!"/utf8>>,

    ok.


straighttalk_write_binary_all_ascii_as_unicode_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'"/utf8>>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!"/utf8>>,

    ok.


straighttalk_write_binary_unicode_as_ascii_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello W?rld!">>,
	% note: that's what MySQL can send back (and store) for non-UTF-8 chars.

    ok.



straighttalk_write_liststring_ascii_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		"INSERT INTO hello_table SET hello_text = 'Hello World!'"),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
	% note: returns always binary

    ok.


straighttalk_write_liststring_unicode_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_list(
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"/utf8>>)),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	 <<"Hello Wørld!"/utf8>> = String,
	% note: returns are always binary

    ok.


straighttalk_write_liststring_all_ascii_as_unicode_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_list(
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'"/utf8>>)),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!"/utf8>>,
	String = <<"Hello World!">>,
	% note: returns are always binary

    ok.


straighttalk_write_liststring_unicode_as_ascii_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"),
		% note: automatically made utf8

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	<<"Hello Wørld!"/utf8>> = String,
	% note: returns are always binary

    ok.

straighttalk_write_binary_ascii_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!">>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,

    ok.


straighttalk_write_binary_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello Wørld!"/utf8>>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello Wørld!"/utf8>>,

    ok.


straighttalk_write_binary_all_ascii_as_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!"/utf8>>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!"/utf8>>,

    ok.


straighttalk_write_binary_unicode_as_ascii_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello Wørld!">>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello W?rld!">>,
	% note: that's what MySQL can send back (and store) for non-UTF-8 chars.

    ok.



straighttalk_write_liststring_ascii_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, ["Hello World!"]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
	% note: returns always binary

    ok.


straighttalk_write_liststring_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [unicode:characters_to_list(<<"Hello Wørld!"/utf8>>)]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	 <<"Hello Wørld!"/utf8>> = String,
	% note: returns are always binary

    ok.


straighttalk_write_liststring_all_ascii_as_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!"/utf8>>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!"/utf8>>,
	String = <<"Hello World!">>,
	% note: returns are always binary

    ok.


straighttalk_write_liststring_unicode_as_ascii_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, ["Hello Wørld!"]),
		% note: automatically made utf8

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	<<"Hello Wørld!"/utf8>> = String,
	% note: returns are always binary

    ok.

%%--------------------------------------------------------------------
%%
%% Preliminary Tests of basic functionality.
%%
%%--------------------------------------------------------------------


%% A test case. The ok is irrelevant. What matters is, if it returns.
%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------

%% Test Case: Delete all records in the test database
%%--------------------------------------------------------------------
delete_all(_) ->
	
    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),
    ok.


%% Test Case: Make an Insert
%%--------------------------------------------------------------------
insert_only(_) ->
	
    emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    ok.

%% Test Case: Make an Insert
%%--------------------------------------------------------------------
insert_only_unicode_binary(_) ->
	
    emysql:execute(test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"/utf8>>),

    ok.

%% Test Case: Make an Insert
%%--------------------------------------------------------------------
insert_only_unicode_liststring(_) ->
	
    emysql:execute(test_pool,
    	unicode:characters_to_list(
        <<"INSERT INTO hello_table SET hello_text = 'Hellö Wørld!'"/utf8>>)),

    ok.

%% Test Case: Make an Insert and Select it back, reading out as Record
%%--------------------------------------------------------------------
insert_and_read_back_as_recs(_) ->
	
    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result: ~p~n", [Result]),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Recs: ~p~n", [Recs]),

	% the test
	Recs = [{hello_record,<<"Hello World!">>}],

	ok.
	

%% Test Case: Create a Prepared Statement and make a Select with it
%%--------------------------------------------------------------------
select_by_prepared_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	emysql:prepare(test_stmt, 
		<<"SELECT * from hello_table WHERE hello_text like ?">>),

	Result = emysql:execute(test_pool, test_stmt, ["Hello%"]),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result: ~p~n", [Result]),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Recs: ~p~n", [Recs]),

	% the test
	Recs = [{hello_record,<<"Hello World!">>}],

	[{hello_record,BinString}] = Recs,

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result String (unicode string): ~ts~n", [BinString]),

	% the test
	BinString = <<"Hello World!">>,

    ok.


%%--------------------------------------------------------------------
%%
%% Workers and tests of workers.
%%
%%--------------------------------------------------------------------


%% Worker: Read back and check, using SELECT directly
%%--------------------------------------------------------------------
%% Note: coming back from the database is always a binary.

read_back_directly(Value) when is_list(Value) ->
	read_back_directly(unicode:characters_to_binary(Value));
	
read_back_directly(Value) when is_binary(Value) ->

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Read Back executing SELECT directly, expecting: ~p~n", [Value]),
	% io:format("                           expecting (unicode): ~ts~n", [Value]),
	io:format("                           expecting (integer): ~w~n", [Value]),


	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result: ~p~n", [Result]),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Recs: ~p~n", [Recs]),

	% the test
	Recs = [{hello_record, Value}],

	[{hello_record,BinString}] = Recs,

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result String (unicode string): ~ts~n", [BinString]),

	% the test
	BinString = Value,

    ok.


%% Worker: Read back and check, using SELECT by prepared statement
%%--------------------------------------------------------------------
%% Note: coming back from the database is always a binary.

read_back_by_statement(Value) when is_list(Value) ->
	read_back_by_statement(unicode:characters_to_binary(Value));

read_back_by_statement(Value) when is_binary(Value) ->

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Read Back using SELECT through prepared statement, expecting: ~p~n", [Value]),
	io:format("                                        expecting (unicode): ~ts~n", [Value]),
	io:format("                                        expecting (integer): ~w~n", [Value]),

	Result = emysql:execute(test_pool, stmt_select),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result: ~p~n", [Result]),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Recs: ~p~n", [Recs]),

	% the test
	Recs = [{hello_record, Value}],

	[{hello_record,BinString}] = Recs,

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Result String (unicode string): ~ts~n", [BinString]),

	% the test
	BinString = Value,

    ok.


%% Test Case: Write and read back a binary. This tests the worker function.
%%--------------------------------------------------------------------
test_read_back_directly_function(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	read_back_directly(<<"Hello World!">>),
	read_back_directly("Hello World!"),

    ok.

%% Test Case: Write and read back. This tests the select statement.
%%--------------------------------------------------------------------
test_read_back_by_statement_function(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	read_back_by_statement(<<"Hello World!">>),
	read_back_by_statement("Hello World!"),

    ok.

%% Test Case: Write and read back. This tests the insert statement.
%%--------------------------------------------------------------------
test_stmt_and_read_back_directly_function(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!">>]),
	read_back_directly(<<"Hello World!">>),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, ["Hello World!"]),
	read_back_directly(<<"Hello World!">>),

    ok.

%% Test Case: Write and read back. This tests both statements.
%%--------------------------------------------------------------------
test_stmt_and_read_back_stmt_function(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!">>]),
	read_back_by_statement(<<"Hello World!">>),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, ["Hello World!"]),
	read_back_by_statement(<<"Hello World!">>),

    ok.


%% Test Case: ASCII binary as direct insert statement.
%%--------------------------------------------------------------------
test_ascii_binary_direct(_) ->

	Value = <<"Hello World X!">>,

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value)])),

	read_back_directly(Value),
	read_back_by_statement(Value),

    ok.


%% Test Case: ASCII binary as parameter to prepared insert statement.
%%--------------------------------------------------------------------
test_ascii_binary_via_parameter(_) ->

	Value = <<"Hello World XX!">>,

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	read_back_directly(Value),
	read_back_by_statement(Value),

    ok.


%% Test Case: ASCII liststring as parameter to prepared insert statement.
%%--------------------------------------------------------------------
test_ascii_liststring_via_parameter(_) ->

	Value = "Hello World XXX!",
	Binary = unicode:characters_to_binary(Value),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	read_back_directly(Binary),
	read_back_by_statement(Binary),

    ok.


%% Worker: write binary as direct insert statement.
%%--------------------------------------------------------------------
worker_direct_insert(Value) when is_binary(Value) ->
	worker_direct_insert(Value,Value);

worker_direct_insert(Value) when is_list(Value) ->
	Binary = unicode:characters_to_binary(Value),
	worker_direct_insert(Value, Binary).


worker_direct_insert(Value, Expect) when is_binary(Value) ->

	io:format("Worker: write binary as direct insert statement: ~p = ~w.~n", [Value,Value]),
	io:format("******************************************~n", []),

	io:format("                                  quote() makes: ~p = ~w.~n", [emysql_util:quote(Value),emysql_util:quote(Value)]),

	X = emysql_util:any_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value)]),

	io:format("=> ~p = ~w.~n", [X,X]),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),


	emysql:execute(test_pool,
		emysql_util:any_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value)])),

	read_back_directly(Expect),
	read_back_by_statement(Expect),

    ok;

%% Worker: write liststring as direct insert statement.
%%--------------------------------------------------------------------
worker_direct_insert(Value, Binary) when is_list(Value) ->

	io:format("Worker: write liststring as direct insert statement.~n", []),
	io:format("**********************************************~n", []),

	io:format("                                  quote() makes: ~p = ~w.~n", [emysql_util:quote(Value),emysql_util:quote(Value)]),

	X = emysql_util:any_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value)]),

	io:format("=> ~p = ~w.~n", [X,X]),

	io:format("Binary: ~p = ~w.~n", [Binary,Binary]),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value)])),

	read_back_directly(Binary),
	read_back_by_statement(Binary),

    ok.


%% Worker: write binary as parameter to prepared insert statement.
%%--------------------------------------------------------------------
worker_insert_via_statement(Value) when is_binary(Value) ->
	worker_insert_via_statement(Value,Value);

worker_insert_via_statement(Value) when is_list(Value) ->
	Expect = unicode:characters_to_binary(Value),
	worker_insert_via_statement(Value,Expect).

worker_insert_via_statement(Value,Expect) when is_binary(Value) ->

	io:format("Worker: write binary as parameter to prepared insert statement.~n", []),
	io:format("*********************************************************~n", []),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	io:format("... read back directly~n", []),

	read_back_directly(Expect),

	io:format("... read back via statement~n", []),

	read_back_by_statement(Expect),
%$
    ok;
    
%% Worker: write liststring as parameter to prepared insert statement.
%%--------------------------------------------------------------------
worker_insert_via_statement(Value,Binary) when is_list(Value) ->

	io:format("Worker: write liststring as parameter to prepared insert statement.~n", []),
	io:format("*************************************************************~n", []),


    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	io:format("... read back directly~n", []),

	read_back_directly(Binary),

	io:format("... read back via statement~n", []),

	read_back_by_statement(Binary),

    ok.


%% Worker Test Case: ASCII binary.
%%--------------------------------------------------------------------
test_ascii_binary(_) ->

	Value = <<"Ahoy Vereld!">>,

	worker_direct_insert(Value),

	worker_insert_via_statement(Value),

    ok.

%% Worker Test Case: ASCII binary.
%%--------------------------------------------------------------------
test_ascii_liststring(_) ->

	Value = "Ahoy Vereld!",
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.


%%--------------------------------------------------------------------
%%
%% Begin of actual tests.
%%
%%--------------------------------------------------------------------

%% Test Case: Unicode binary.
%%--------------------------------------------------------------------
test_unicode_binary(_) ->

	Value = <<"Ahöy Vereld!!!"/utf8>>,
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Unicode binary.
%%--------------------------------------------------------------------
test_unicode_liststring(_) ->

	Value = unicode:characters_to_list(<<"Ahöy Vereld!!!!"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.


%% Test Case: Single Quote as binary.
%%--------------------------------------------------------------------
test_ascii_quote_as_binary(_) ->

	Value = <<"'">>,
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote as liststring.
%%--------------------------------------------------------------------
test_ascii_quote_as_liststring(_) ->

	Value = "'",
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_as_binary(_) ->

	Value = unicode:characters_to_list(<<"'"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_as_liststring(_) ->

	Value = unicode:characters_to_list(<<"'"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with text as binary.
%%--------------------------------------------------------------------
test_ascii_quote_and_text_as_binary(_) ->

	Value = <<"Hank's">>,
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with text as liststring.
%%--------------------------------------------------------------------
test_ascii_quote_and_text_as_liststring(_) ->

	Value = "Hank's",
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with text as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_and_text_as_binary(_) ->

	Value = unicode:characters_to_list(<<"Hank's"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with text as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_and_text_as_liststring(_) ->

	Value = unicode:characters_to_list(<<"Hank's"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with trailing text as binary.
%%--------------------------------------------------------------------
test_ascii_quote_and_trailing_text_as_binary(_) ->

	Value = <<"'gha!">>,
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with trailing text as liststring.
%%--------------------------------------------------------------------
test_ascii_quote_and_trailing_text_as_liststring(_) ->

	Value = "'gha!",
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with trailing text as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_and_trailing_text_as_binary(_) ->

	Value = unicode:characters_to_list(<<"'gha!"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Test Case: Single Quote with trailing text as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_and_trailing_text_as_liststring(_) ->

	Value = unicode:characters_to_list(<<"'gha!"/utf8>>),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%%--------------------------------------------------------------------
%%
%% More integrated tests.
%%
%%--------------------------------------------------------------------


%% Worker: insert as binary.
%%--------------------------------------------------------------------
worker_insert_as_binary(V) ->

	Value = list_to_binary(V),
	io:format("worker_insert_as_binary: ~p~n", [Value]),
	worker_direct_insert(Value, castrate(Value)),
	worker_insert_via_statement(Value, castrate(Value)),
    ok.

%% Worker: insert as liststring.
%%--------------------------------------------------------------------
worker_insert_as_liststring(Value) ->

	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Worker: insert as unicode binary.
%%--------------------------------------------------------------------
worker_as_unicode_binary(Value) ->

	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.

%% Worker: insert as unicode liststring.
%%--------------------------------------------------------------------
worker_insert_as_unicode_liststring(V) ->

	Value = unicode:characters_to_list(V),
	worker_direct_insert(Value),
	worker_insert_via_statement(Value),
    ok.


%% Test Case: Single Quote with worker quartet single functions.
%%--------------------------------------------------------------------
test_worker_quartet_single_functions(_) ->

	V = "'",
	worker_insert_as_binary(V),
	worker_insert_as_liststring(V),
	worker_as_unicode_binary(V),
	worker_insert_as_unicode_liststring(V),
	ok.

%% Worker: Quartet
%%--------------------------------------------------------------------
worker_quartet(V) ->

	worker_insert_as_binary(V),
	worker_insert_as_liststring(V),
	worker_as_unicode_binary(V),
	worker_insert_as_unicode_liststring(V),
	ok.

%% Test Case: Single Quote with worker quartet function.
%%--------------------------------------------------------------------
test_worker_quartet(_) ->

	worker_quartet("'"),
	ok.

%% Test Case: Odd character combinations to be escaped.
%%--------------------------------------------------------------------
test_escaping(_) ->

	worker_quartet("'"),
	worker_quartet("''"),
	worker_quartet("'b!"),
	worker_quartet("s'"),
	worker_quartet("'t'"),
	worker_quartet("t't"),
	worker_quartet("'''"),
	worker_quartet("s'''"),
	worker_quartet("''ss"),
	worker_quartet("\\"),
	worker_quartet("\""),
	worker_quartet("\"\""),
	worker_quartet("\"'\""),
	worker_quartet("\\'"),
	worker_quartet("\\\""),
	worker_quartet("\\\"h\""),
	ok.

%% Test Case: Odd character combinations to be escaped with non asciis.
%%--------------------------------------------------------------------
test_escaping_with_non_ascii(_) ->

	worker_quartet("'öüx"),
	worker_quartet("''ö"),
	worker_quartet("'b!ö"),
	worker_quartet("s'ö"),
	worker_quartet("'t'ö"),
	worker_quartet("t'tö"),
	worker_quartet("'''ö"),
	worker_quartet("s'''ö"),
	worker_quartet("''ssö"),
	worker_quartet("\\ö"),
	worker_quartet("\"ö"),
	worker_quartet("\"\"ö"),
	worker_quartet("\"'\"ö"),
	worker_quartet("\\'ö"),
	worker_quartet("\\\"ö"),
	worker_quartet("\\\"h\"ö"),
	worker_quartet("ö'"),
	worker_quartet("ö''"),
	worker_quartet("ö'b!"),
	worker_quartet("ös'"),
	worker_quartet("ö't'"),
	worker_quartet("öt't"),
	worker_quartet("ö'''"),
	worker_quartet("ös'''"),
	worker_quartet("ö''ss"),
	worker_quartet("ö\\"),
	worker_quartet("ö\""),
	worker_quartet("ö\"\""),
	worker_quartet("ö\"'\""),
	worker_quartet("ö\\'"),
	worker_quartet("ö\\\""),
	worker_quartet("ö\\\"h\""),

	ok.

%% Test Case: Odd character combinations to be escaped with non asciis.
%%--------------------------------------------------------------------
test_escaping_with_non_ascii_leading_umlaut(_) ->

	worker_quartet("ü'öüx"),
	worker_quartet("ü'ö"),
	worker_quartet("ü''ö"),
	worker_quartet("ü'b!ö"),
	worker_quartet("üs'ö"),
	worker_quartet("ü't'ö"),
	worker_quartet("üt'tö"),
	worker_quartet("ü'''ö"),
	worker_quartet("üs'''ö"),
	worker_quartet("ü''ssö"),
	worker_quartet("ü\\ö"),
	worker_quartet("ü\"ö"),
	worker_quartet("ü\"\"ö"),
	worker_quartet("ü\"'\"ö"),
	worker_quartet("ü\\'ö"),
	worker_quartet("ü\\\"ö"),
	worker_quartet("ü\\\"h\"ö"),
	worker_quartet("üö'"),
	worker_quartet("üö''"),
	worker_quartet("üö'b!"),
	worker_quartet("üös'"),
	worker_quartet("üö't'"),
	worker_quartet("üöt't"),
	worker_quartet("üö'''"),
	worker_quartet("üös'''"),
	worker_quartet("üö''ss"),
	worker_quartet("üö\\"),
	worker_quartet("üö\""),
	worker_quartet("üö\"\""),
	worker_quartet("üö\"'\""),
	worker_quartet("üö\\'"),
	worker_quartet("üö\\\""),
	worker_quartet("üö\\\"h\""),

	ok.


%% make ? of chars > 127.
%% That is what MySQL makes with invalid UTF-8 characters.
castrate(Bin) when is_binary(Bin) ->
	list_to_binary(lists:reverse(castrate(binary_to_list(Bin), []))).
	% note: this is a bytewise inspection that works for unicode, too.

castrate([], Acc) ->
	Acc;
castrate([C | Rest], Acc) when C < 128 ->
	castrate(Rest, [C | Acc]);
castrate([_ | Rest], Acc) ->
	castrate(Rest, [$? | Acc]).
