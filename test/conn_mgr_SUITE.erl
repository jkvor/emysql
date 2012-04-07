%%%-------------------------------------------------------------------
%%% File     : Emysql/test/conn_mgr_SUITE.erl
%%% Descr    : Suite #7 - Testing connection manager. 
%%% Author   : R. Richardson
%%% Created  : 04/06/2012 ransomr
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

-module(conn_mgr_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [two_procs, no_lock_timeout].


%%--------------------------------------------------------------------
init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", utf8),
    Config.
    
% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	ok.

%% Test Case: Test two processes trying to share one connection
%% Test for Issue 9
%%--------------------------------------------------------------------
two_procs(_) ->
    Num = 2,
    process_flag(trap_exit, true),
    [spawn_link(fun test_proc/0)
     || _ <- lists:seq(1,Num)],
    [receive
	 {'EXIT', _, Reason} -> 
	     normal = Reason
     end
     || _ <- lists:seq(1,Num)],
    ok.

%% Process that will do a bunch of requests against mysql
test_proc() ->
    [
     #result_packet{} = emysql:execute(test_pool, "describe hello_table;")
     || _ <- lists:seq(1,1000)
    ].
     
%% Test Case: Make sure that the pool is still usable after a lock timeout
%% Test for race on connection where the connection is sent to a process
%% at the exact same time it hits the timeout.
%% See ransomr's first comment on Issue 9
%%--------------------------------------------------------------------
no_lock_timeout(_) ->
    Num = 2,
    OldEnv = application:get_env(emysql, lock_timeout),
    %% This is really hard to repro - the timing is very tricky.
    %% On my machine a timeout of 1 seems to reproduce it after about 10 attempts.
    %% Once it happens the connection is lost from the pool, so the pool needs to be reset.
    application:set_env(emysql, lock_timeout, 1),
    process_flag(trap_exit, true),
    [spawn_link(fun test_proc/0)
     || _ <- lists:seq(1,Num)],
    %% We expect one process to timeout, but the other should exit normally
    receive
	{'EXIT', _, Reason1} ->
	    connection_lock_timeout = Reason1
    end,
    receive
	{'EXIT', _, Reason2} ->
	    normal = Reason2
    end,
    case OldEnv of
	undefined ->
	    application:unset_env(emysql, lock_timeout);
	{ok, Timeout} ->
	    application:set_env(emysql, lock_timeout, Timeout)
    end,
    ok.
