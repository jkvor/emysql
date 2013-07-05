%%%-------------------------------------------------------------------
%%% File     : Emysql/test/emysql_util_SUITE.erl
%%% Descr    : Suite - testing mysql_util
%%% Author   : Mike Oxford <moxford@gmail.com>
%%% Created  : 07/01/2013
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
-module(emysql_util_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [ 
      {group, as_dict_group},
      {group, as_proplist_group},
      {group, as_record_group}
    ].

groups() ->
    [
     {as_dict_group, [], [
			  dict_empty_test,
			  dict_single_test,
			  dict_multi_test
			 ]},
     {as_proplist_group, [], [
			      proplist_empty_test,
			      proplist_single_test,
			      proplist_multi_test
			     ]},
     {as_record_group, [], [
			    record_test
			   ]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    %% if this fails, focus on environment_SUITE to fix test setup.
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

%% Test Cases: Test if emysql_util:as_dict/1 works
%%--------------------------------------------------------------------
dict_empty_test(_) ->
    dict:from_list([]) =:= emysql_util:as_dict(get_empty_test()).

dict_single_test(_) ->
    dict:from_list([{<<"HelloField">>,<<"Hello">>}]) =:= emysql_util:as_dict(get_single_test()).

dict_multi_test(_) ->
    dict:from_list([
		    {<<"HelloField">>,<<"Hello">>},
		    {<<"HiField">>,<<"Hi">>},
		    {<<"ByeField">>,<<"Bye">>}
		   ])
	=:= emysql_util:as_dict(get_multi_test()).
%% Test Cases: Test if emysql_util:as_proplist/1 works
%%--------------------------------------------------------------------
proplist_empty_test(_) ->
    [] =:= emysql_util:as_proplist(get_empty_test()).

proplist_single_test(_) ->
    [{<<"HelloField">>,<<"Hello">>}] =:=  emysql_util:as_proplist(get_single_test()).

proplist_multi_test(_) ->
    [
     {<<"HelloField">>,<<"Hello">>},
     {<<"HiField">>,<<"Hi">>},
     {<<"ByeField">>,<<"Bye">>}
    ] 
	=:= emysql_util:as_proplist(get_multi_test()).
%% Test Cases: Test if emysql_util:as_record/3 works
%%--------------------------------------------------------------------
%% this test is from a previous author and imported here
-record(person, {surname, name, phone, socks}).
record_test() ->
    emysql:execute(test_pool, <<"DROP TABLE IF EXISTS as_record_test;">>),
    emysql:execute(test_pool, <<"CREATE TABLE as_record_test (name varchar(60), surname varchar(60), socks int)">>),
    
    emysql:prepare(ins, <<"INSERT INTO as_record_test (name, surname, socks) VALUES (?, ?, ?)">>),
    emysql:execute(test_pool, ins, [<<"Maxim">>, <<"Komar">>, 3]),
    
						% test
    Result = emysql:execute(test_pool, <<"select * from as_record_test">>),
    Expected = [#person{ surname = <<"Komar">>, name = <<"Maxim">>, socks=3 }],
    Expected = emysql_util:as_record(Result, person, record_info(fields, person)),
    ok.
%%--------------------------------------------------------------------
get_empty_test() ->
    {result_packet,5,
		  [{field,2,<<"def">>,<<>>,<<>>,<<>>,<<"HelloField">>,<<>>,253,
		    <<>>,33,15,1,31}],
     undefined,
     <<>>}.

get_single_test() ->
    {result_packet,5,
		  [{field,2,<<"def">>,<<>>,<<>>,<<>>,<<"HelloField">>,<<>>,253,
		    <<>>,33,15,1,31}],
		  [[<<"Hello">>]],
		  <<>>}.

get_multi_test() -> 
    {result_packet,7,
     [{field,2,<<"def">>,<<>>,<<>>,<<>>,<<"HelloField">>,<<>>,253,
       <<>>,33,15,1,31},
      {field,3,<<"def">>,<<>>,<<>>,<<>>,<<"HiField">>,<<>>,253,<<>>,33,
       6,1,31},
      {field,4,<<"def">>,<<>>,<<>>,<<>>,<<"ByeField">>,<<>>,253,<<>>,
       33,9,1,31}],
     [[<<"Hello">>,<<"Hi">>,<<"Bye">>]],
     <<>>}.



