%%%-------------------------------------------------------------------
%%% File     : Emysql/test/as_record_SUITE.erl
%%% Descr    : Suite #8 - testing mysql_util:as_record
%%% Author   : M. Komar
%%% Created  : 07/05/2013 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/: 
%%%     make testutil
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(as_record_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [as_record].


%%--------------------------------------------------------------------
init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
	emysql:add_pool(test_pool, 1,
		"test_username", "test_password", "localhost", 3306,
		"test_database", utf8),
    Config.
    
% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	ok.

%% Test Case: Test if improved version of mysql_util:as_records work ok.
%%--------------------------------------------------------------------
-record(person, {surname, name, phone, socks}).
as_record(_) ->
	emysql:execute(test_pool, <<"DROP TABLE IF EXISTS as_record_test;">>),
	emysql:execute(test_pool, <<"CREATE TABLE as_record_test (name varchar(60), surname varchar(60), socks int)">>),

	emysql:prepare(ins, <<"INSERT INTO as_record_test (name, surname, socks) VALUES (?, ?, ?)">>),
	emysql:execute(test_pool, ins, [<<"Maxim">>, <<"Komar">>, 3]),

	% test
	Result = emysql:execute(test_pool, <<"select * from as_record_test">>),
	Expected = [#person{ surname = <<"Komar">>, name = <<"Maxim">>, socks=3 }],
	Expected = emysql_util:as_record(Result, person, record_info(fields, person)),
	ok.
