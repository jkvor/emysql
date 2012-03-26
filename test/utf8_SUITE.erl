%%%-------------------------------------------------------------------
%%% File     : Emysql/test/utf8_SUITE.erl
%%% Descr    : Suite #3: Test for UTF-8 connection to UTF-8 DB.
%%% Author   : H. Diedrich
%%% Created  : 12/14/2011 hd
%%% Changed  : 03/26/2012 hd
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

-module(utf8_SUITE).
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
     straighttalk_write_binary_latin1_directly,
     straighttalk_write_binary_latin1_with_special_char_directly,
     straighttalk_write_binary_unicode_directly,
     straighttalk_write_liststring_latin1_directly,
     straighttalk_write_liststring_unicode_directly,
     straighttalk_write_liststring_latin1_with_special_char_directly,
     straighttalk_write_binary_latin1_via_statement, 
     straighttalk_write_binary_latin1_with_special_char_via_statement,
     straighttalk_write_binary_unicode_via_statement,
     straighttalk_write_binary_all_latin1_as_unicode_via_statement,
     straighttalk_write_liststring_latin1_via_statement,
     straighttalk_write_liststring_unicode_via_statement,
	 
	 % these are tests using nested worker functions
	 test_read_back_directly_function,
	 test_read_back_by_statement_function,
	 test_stmt_and_read_back_directly_function,
	 test_stmt_and_read_back_stmt_function,
	 test_latin1_binary_direct,
	 test_latin1_binary_via_parameter,
	 test_latin1_liststring_via_parameter,
	 test_latin1_binary,
	 test_latin1_liststring,
	 test_unicode_binary,
	 test_unicode_liststring,
	 
	 test_latin1_quote_as_binary,
	 test_latin1_quote_as_liststring,
	 test_unicode_quote_as_binary,
	 test_unicode_quote_as_liststring,
	 test_latin1_quote_and_text_as_binary,
	 test_latin1_quote_and_text_as_liststring,
	 test_unicode_quote_and_text_as_binary,
	 test_unicode_quote_and_text_as_liststring,
	 test_latin1_quote_and_trailing_text_as_binary,
	 test_latin1_quote_and_trailing_text_as_liststring,
	 test_unicode_quote_and_trailing_text_as_binary,
	 test_unicode_quote_and_trailing_text_as_liststring,
	 
	 test_worker_quartet_single_functions,
	 test_worker_quartet,
	 test_escaping,
     test_escaping_with_non_latin1,
	 test_escaping_with_non_latin1_leading_umlaut
	 ].

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
        "hello_utf8_database", utf8),

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
%% Human Understandable ('straighttalk') Tests.
%%
%%--------------------------------------------------------------------
%% These tests are the easiest to read.
%% They also serve to make sure that at least some tests don't
%% completely miss the point for cleverness.

straighttalk_write_binary_latin1_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
    % in = out

    ok.

straighttalk_write_binary_latin1_with_special_char_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'">>),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	<<"Hello W">> = String, 
    % in /= out:
    % The UTF-8 database truncated as a ø in Latin-1 is an invalid bit pattern.

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
    % in = out

    ok.


straighttalk_write_liststring_latin1_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		"INSERT INTO hello_table SET hello_text = 'Hello World!'"),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
    % in = out
	% note: returns are always binaries.

    ok.


straighttalk_write_liststring_unicode_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_list( % (*)
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"/utf8>>)),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	 <<"Hello Wørld!"/utf8>> = String,
    % in = out
	% note: returns are always binaries.
	
	Same = unicode:characters_to_list(
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"/utf8>>),
	Same = binary_to_list(
		<<"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'">>),

    ok.


straighttalk_write_liststring_latin1_with_special_char_directly(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		"INSERT INTO hello_table SET hello_text = 'Hello Wørld!'"),
		% note: automatically made utf8

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	<<"Hello Wørld!"/utf8>> = String,
    % in = out:
	% automatic conversion by driver for connection set to utf8.
	% note: returns are always binaries.

    ok.

straighttalk_write_binary_latin1_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!">>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
    % in = out

    ok.

straighttalk_write_binary_latin1_with_special_char_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),
    try
    	emysql:execute(test_pool, stmt_insert, [<<"Hello Wørld!">>]),
  	    exit(should_have_crashed)
    catch
       exit:{{invalid_utf8_binary,"Hello W",<<"ørld!">>}, {}} -> ok
    end,

    ok.

straighttalk_write_binary_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello Wørld!"/utf8>>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	<<"Hello Wørld!"/utf8>> = String,
    % in = out

    ok.


straighttalk_write_binary_all_latin1_as_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [<<"Hello World!"/utf8>>]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!"/utf8>>,
    % in = out

    ok.


straighttalk_write_liststring_latin1_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, ["Hello World!"]),

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

	String = <<"Hello World!">>,
    % in = out
	% note: returns are always binaries.

    ok.


straighttalk_write_liststring_unicode_via_statement(_) ->

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [unicode:characters_to_list(<<"Hello Wørld!"/utf8>>,utf8)]), % (*)

	Result = emysql:execute(test_pool, <<"SELECT * from hello_table">>),

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	[{hello_record, String}] = Recs,

    % N.B.
	<<"Hello Wørld!">> = list_to_binary(unicode:characters_to_list(<<"Hello Wørld!"/utf8>>)),
	% the code point of ø is the same for Unicode and Latin-1
	% even thought the bitpattern of UTF-8 and Latin-1 is not.
	% But unicode:characters_to_list/1/2 delivers codepoints,
	% not binary bitpatterns.

	 <<"Hello Wørld!"/utf8>> = String,
    % in = out
	% note: returns are always binaries.
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
	io:format("Result String (latin-1 string): ~ts~n", [BinString]),

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
	read_back_directly(list_to_binary(Value)); % will this break with codepoints > Latin-1?
	
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

	% find this output by clicking on the test name, then case name in test/index.html
	[{hello_record, ResultBin}] = Recs,
   	io:format("Result String: ~w~n", [ResultBin]),

	% the test
 	Value = ResultBin,

    ok.


%% Worker: Read back and check, using SELECT by prepared statement
%%--------------------------------------------------------------------
%% Note: coming back from the database is always a binary.

read_back_by_statement(Value) when is_list(Value) ->
	read_back_by_statement(list_to_binary(Value));

read_back_by_statement(Value) when is_binary(Value) ->

	% find this output by clicking on the test name, then case name in test/index.html
	io:format("Read Back using SELECT through prepared statement, expecting: ~p~n", [Value]),
	
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


	% find this output by clicking on the test name, then case name in test/index.html
	[{hello_record,BinString}] = Recs,
	io:format("Result String: ~w~n", [BinString]),

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


%% Test Case: Latin 1 binary as direct insert statement.
%%--------------------------------------------------------------------
test_latin1_binary_direct(_) ->

	Value = <<"Hello World X!">>,

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		unicode:characters_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value,latin1)])),

	read_back_directly(Value),
	read_back_by_statement(Value),

    ok.


%% Test Case: Latin 1 binary as parameter to prepared insert statement.
%%--------------------------------------------------------------------
test_latin1_binary_via_parameter(_) ->

	Value = <<"Hello World XX!">>,

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	read_back_directly(Value),
	read_back_by_statement(Value),

    ok.


%% Test Case: Latin 1 liststring as parameter to prepared insert statement.
%%--------------------------------------------------------------------
test_latin1_liststring_via_parameter(_) ->

	Value = "Hello World XXX!",
	Binary = unicode:characters_to_binary(Value),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	read_back_directly(Binary),
	read_back_by_statement(Binary),

    ok.


%% Worker: write binary as direct insert statement.
%%--------------------------------------------------------------------
worker_direct_insert(Value, QuoteEncoding, Expect) when is_binary(Value) ->

	io:format("Worker: write binary as direct insert statement: ~p = ~w. (Exp: ~w)~n", [Value,Value,Expect]),
	io:format("******************************************~n", []),

	io:format("                                  quote() makes: ~p = ~w.~n", [emysql_util:quote(Value,QuoteEncoding),emysql_util:quote(Value,QuoteEncoding)]),

	X = emysql_util:any_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value,QuoteEncoding)]),

	io:format("=> ~p = ~w.~n", [X,X]),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool,
		emysql_util:any_to_binary(["INSERT INTO hello_table SET hello_text = ",
			emysql_util:quote(Value,QuoteEncoding)])),

	read_back_directly(Expect),
	read_back_by_statement(Expect),

    ok.

%% Worker: write liststring as direct insert statement.
%%--------------------------------------------------------------------
worker_direct_insert(Value, Binary) when is_list(Value) ->

	io:format("Worker: write liststring as direct insert statement.~n", []),
	io:format("**********************************************~n", []),

	io:format("                                  quote() makes: ~p = ~w.~n", [emysql_util:quote(Value),emysql_util:quote(Value)]),

	io:format("Binary: ~p = ~w.~n", [Binary,Binary]),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, "INSERT INTO hello_table SET hello_text = " ++ 
	    emysql_util:quote(Value)),

	read_back_directly(Binary),
	read_back_by_statement(Binary),

    ok.


%% Worker: write binary as parameter to prepared insert statement.
%%--------------------------------------------------------------------
worker_insert_via_statement(Value,_,Expect) when is_binary(Value) ->

	io:format("Worker: write binary as parameter to prepared insert statement.~n", []),
	io:format("*********************************************************~n", []),

    emysql:execute(test_pool, <<"DELETE FROM hello_table">>),

	emysql:execute(test_pool, stmt_insert, [Value]),

	io:format("... read back directly~n", []),

	read_back_directly(Expect),

	io:format("... read back via statement~n", []),

	read_back_by_statement(Expect),

    ok.
    
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


%% Worker Test Case: Latin 1 binary.
%%--------------------------------------------------------------------
test_latin1_binary(_) ->

	Value = <<"Ahoy Vereld!">>,

	worker_direct_insert(Value,latin1,Value),
	worker_insert_via_statement(Value,latin1,Value),

    ok.

%% Worker Test Case: Latin 1 binary.
%%--------------------------------------------------------------------
test_latin1_liststring(_) ->

	Value = "Ahoy Vereld!",
	worker_direct_insert(Value,list_to_binary(Value)),
	worker_insert_via_statement(Value,list_to_binary(Value)),
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
	worker_direct_insert(Value,utf8,Value),
	worker_insert_via_statement(Value,utf8,Value),
    ok.

%% Test Case: Unicode binary.
%%--------------------------------------------------------------------
test_unicode_liststring(_) ->

    Unicode = <<"Ahöy Vereld!!!!"/utf8>>,
	Value   = unicode:characters_to_list(Unicode),

    % N.B.
    true = unicode:characters_to_list(<<"Ahöy Vereld!!!!"/utf8>>) 
                    == binary_to_list(<<"Ahöy Vereld!!!!">>),

	worker_direct_insert(Value,Unicode),
	worker_insert_via_statement(Value,Unicode),
	% direct insert /= via statement
	% because the direct statement in this test suite is created
	% from the binary directly, then executed, while the statement
	% parameter is quoted, and for this must be 1) decoded into a 
	% list of codepoints (a string) and 2) back. The list resulting
	% from the decoding looks the same whether it was made from
	% UTF-8 or Latin-1 binaries. The encoding back into a binary
	% is then done using the set encoding for the connection,
	% Latin-1 in this case. Therefore, what is sent to the database
	% is Latin-1 when prepared statements are used with UTF-8 binaries.
	% The automatic quoting cannot be done with full ('direct')
	% query strings as this would quote their quotes. /hd mar-12
	
    ok.


%% Test Case: Single Quote as binary.
%%--------------------------------------------------------------------
test_latin1_quote_as_binary(_) ->

	Value = <<"'">>,
	worker_direct_insert(Value,latin1,Value),
	worker_insert_via_statement(Value,latin1,Value),
    ok.

%% Test Case: Single Quote as liststring.
%%--------------------------------------------------------------------
test_latin1_quote_as_liststring(_) ->

	Value = "'",
	worker_direct_insert(Value,list_to_binary(Value)),
	worker_insert_via_statement(Value,list_to_binary(Value)),
    ok.

%% Test Case: Single Quote as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_as_binary(_) ->

	Value = <<"'"/utf8>>,
	worker_direct_insert(Value,utf8,Value),
	worker_insert_via_statement(Value,utf8,Value),
    ok.

%% Test Case: Single Quote as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_as_liststring(_) ->

    Expect = <<"'"/utf8>>,
	Value = unicode:characters_to_list(Expect),
	worker_direct_insert(Value,Expect),
	worker_insert_via_statement(Value,Expect),
    ok.

%% Test Case: Single Quote with text as binary.
%%--------------------------------------------------------------------
test_latin1_quote_and_text_as_binary(_) ->

	Value = <<"Hank's">>,
	worker_direct_insert(Value,latin1,Value),
	worker_insert_via_statement(Value,latin1,Value),
    ok.

%% Test Case: Single Quote with text as liststring.
%%--------------------------------------------------------------------
test_latin1_quote_and_text_as_liststring(_) ->

	Value = "Hank's",
	worker_direct_insert(Value,list_to_binary(Value)),
	worker_insert_via_statement(Value,list_to_binary(Value)),
    ok.

%% Test Case: Single Quote with text as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_and_text_as_binary(_) ->

	Value = <<"Hank's"/utf8>>,
	worker_direct_insert(Value,utf8,Value),
	worker_insert_via_statement(Value,utf8,Value),
    ok.

%% Test Case: Single Quote with text as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_and_text_as_liststring(_) ->

    Expect = <<"Hank's"/utf8>>,
	Value = unicode:characters_to_list(Expect),
	worker_direct_insert(Value,Expect),
	worker_insert_via_statement(Value,Expect),
    ok.

%% Test Case: Single Quote with trailing text as binary.
%%--------------------------------------------------------------------
test_latin1_quote_and_trailing_text_as_binary(_) ->

	Value = <<"'gha!">>,
	worker_direct_insert(Value,latin1,Value),
	worker_insert_via_statement(Value,latin1,Value),
    ok.

%% Test Case: Single Quote with trailing text as liststring.
%%--------------------------------------------------------------------
test_latin1_quote_and_trailing_text_as_liststring(_) ->

	Value = "'gha!",
	worker_direct_insert(Value,list_to_binary(Value)),
	worker_insert_via_statement(Value,list_to_binary(Value)),
    ok.

%% Test Case: Single Quote with trailing text as unicode binary.
%%--------------------------------------------------------------------
test_unicode_quote_and_trailing_text_as_binary(_) ->

	Value = <<"'gha!"/utf8>>,
	worker_direct_insert(Value,utf8,Value),
	worker_insert_via_statement(Value,utf8,Value),
    ok.

%% Test Case: Single Quote with trailing text as unicode liststring.
%%--------------------------------------------------------------------
test_unicode_quote_and_trailing_text_as_liststring(_) ->

	Value = unicode:characters_to_list(Expected = <<"'gha!"/utf8>>),
	worker_direct_insert(Value,Expected),
	worker_insert_via_statement(Value,Expected),
    ok.

%%--------------------------------------------------------------------
%%
%% More integrated tests.
%%
%%--------------------------------------------------------------------


%% Worker: insert as binary.
%%--------------------------------------------------------------------
worker_insert_as_latin1_binary(V) ->

	Value = list_to_binary(V),
	io:format("~w~n", [list_to_binary(truncate(V))]),
	worker_direct_insert(Value, latin1, list_to_binary(truncate(V))),

    try 
        unicode:characters_to_list(Value,utf8), % catch case trigger
       	worker_insert_via_statement(Value, latin1, list_to_binary(truncate(V)))
    catch
        exit:{{invalid_utf8_binary,_,_}, {}} ->
            try
              	worker_insert_via_statement(Value, latin1, list_to_binary(truncate(V))),
   	            exit(should_have_crashed)
             catch
                exit:{{invalid_utf8_binary,_,_}, {}} -> ok
             end
    end,
    
    ok.
    
    % The UTF-8 database truncates from the first invalid bitcode.
    % Latin-1 bitcodes > 128 are invalid as UTF-8.
    % Decoding a Latin-1 binary as if it was UTF-8 can lead to errors.
    % These are tested for and caught here.

%% Worker: insert as liststring.
%%--------------------------------------------------------------------
worker_insert_as_latin1_liststring(Value) ->

	worker_direct_insert(Value,unicode:characters_to_binary(Value,utf8)),
	worker_insert_via_statement(Value,unicode:characters_to_binary(Value,utf8)),
    ok.

%% Worker: insert as unicode binary.
%%--------------------------------------------------------------------
worker_insert_as_unicode_binary(V) ->

    Value = unicode:characters_to_binary(V,latin1,utf8),
	worker_direct_insert(Value,utf8,Value), 
	worker_insert_via_statement(Value,utf8,Value),
    ok.

%% Worker: insert as unicode liststring.
%%--------------------------------------------------------------------
worker_insert_as_unicode_liststring(V) ->

	Value = unicode:characters_to_list(V,latin1),
	worker_direct_insert(Value,unicode:characters_to_binary(Value,utf8)),
    worker_insert_via_statement(Value,unicode:characters_to_binary(Value,utf8)),
    ok.


%% Test Case: Single Quote with worker quartet single functions.
%%--------------------------------------------------------------------
test_worker_quartet_single_functions(_) ->

	V = "'",
	worker_insert_as_latin1_binary(V),
	worker_insert_as_latin1_liststring(V),
	worker_insert_as_unicode_binary(V),
	worker_insert_as_unicode_liststring(V),
	ok.

%% Worker: Quartet
%%--------------------------------------------------------------------
worker_quartet(V) ->

	worker_insert_as_latin1_binary(V),
	worker_insert_as_latin1_liststring(V),
	worker_insert_as_unicode_binary(V),
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

%% Test Case: Odd character combinations to be escaped with non latin1s.
%%--------------------------------------------------------------------
test_escaping_with_non_latin1(_) ->

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

%% Test Case: Odd character combinations to be escaped with non latin1s.
%%--------------------------------------------------------------------
test_escaping_with_non_latin1_leading_umlaut(_) ->

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
%% That is what MySQL does to invalid UTF-8 characters.
castrate(Bin) when is_binary(Bin) ->
	list_to_binary(lists:reverse(castrate(binary_to_list(Bin), [])));
	% note: this is a bytewise inspection that works for unicode, too.

castrate(L) when is_list(L) ->
	lists:reverse(castrate(L, [])).
	% note: this is a bytewise inspection that works for unicode, too.

castrate([], Acc) ->
	Acc;
castrate([C | Rest], Acc) when C < 128 ->
	castrate(Rest, [C | Acc]);
castrate([_ | Rest], Acc) ->
	castrate(Rest, [$? | Acc]).

%% truncate at chars > 127.
%% That is what MySQL does to invalid UTF-8 characters.
truncate(Bin) when is_binary(Bin) ->
	list_to_binary(lists:reverse(truncate(binary_to_list(Bin), [])));
	% note: this is a bytewise inspection that works for unicode, too.

truncate(L) when is_list(L) ->
	lists:reverse(truncate(L, [])).

truncate([], Acc) ->
	Acc;
truncate([C | Rest], Acc) when C < 128 ->
	truncate(Rest, [C | Acc]);
truncate(_, Acc) ->
	Acc.

encode(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(binary_to_list(Bin));
encode(Bin) when is_list(Bin) ->
    unicode:characters_to_binary(Bin).
