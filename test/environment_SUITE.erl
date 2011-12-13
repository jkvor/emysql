%%%-------------------------------------------------------------------
%%% File     : Emysql/test/environment_SUITE.erl
%%% Descr    : Suite #1 - testing the test setup, db and pathes =
%%%            availability of crypto app, emysql app and test db. 
%%% Author   : H. Diedrich
%%% Created  : 12/13/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% THIS SUITE DOES NO ACTUAL TESTS BUT CHECKS THE TEST DATABASE ETC.
%%% Test Cases are in this high granularity for clear failure reports.
%%%
%%% Run from Emysql/: 
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(environment_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [initializing_crypto_app, 
     initializing_emysql_app,
	accessing_emysql_module,
	connecting_to_db_and_creating_a_pool,
     insert_a_record,
     select_a_record
     ].

% nothing, but forces call of clean up
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	emysql:remove_pool(environment_test_pool),
	ok.

% Test Case: Test if the crypt app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_crypto_app(_) ->
    crypto:start(),
    ok.

% Test Case: Test if the emysql app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_emysql_app(_) ->
    application:start(emysql),
    ok.

% Test Case: Test if the emysql module is available. This detects a path error.
%%--------------------------------------------------------------------
accessing_emysql_module(_) ->
    emysql:modules(),
    ok.

% Test Case: Test if we can connect to the test db and create a connection pool.
%%--------------------------------------------------------------------
connecting_to_db_and_creating_a_pool(_) ->
    emysql:add_pool(environment_test_pool, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),
    ok.

% Test Case: Test if we can insert a record.
%%--------------------------------------------------------------------
insert_a_record(_) ->
    emysql:execute(environment_test_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),
	ok.

% Test Case: Test if we can select records.
%%--------------------------------------------------------------------
select_a_record(_) ->
    Result = emysql:execute(environment_test_pool,
        <<"select hello_text from hello_table">>),
    io:format("~n~p~n", [Result]),
    ok.
