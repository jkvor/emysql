%% Copyright (c) 2009 
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(mysql).
-behaviour(application).

-export([start/2, stop/1, init/1, modules/0]).
-export([prepare/2, increment_pool_size/2, decrement_pool_size/2, 
 	     execute/2, execute/3, execute/4, execute/5]).

-include("emysql.hrl").

start(_, _) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
stop(_) -> 
	[[begin
		mysql_conn:close_connection(Conn)
	end || Conn <- Pool#pool.connections] || Pool <- mysql_conn_mgr:pools()],
	ok.

init(_) ->
   {ok, {{one_for_one, 10, 10}, [
		{mysql_statements, {mysql_statements, start_link, []}, permanent, 5000, worker, [mysql_statements]},
		{mysql_conn_mgr, {mysql_conn_mgr, start_link, []}, permanent, 5000, worker, [mysql_conn_mgr]}
   ]}}.

modules() ->
    {ok, Modules} = application_controller:get_key(emysql, modules), Modules.

%% @spec prepare(StmtName, Statement) -> ok
%%		 StmtName = atom()
%%		 Statement = binary() | string()
prepare(StmtName, Statement) when is_atom(StmtName) andalso (is_list(Statement) orelse is_binary(Statement)) ->
	mysql_statements:add(StmtName, Statement).
	
increment_pool_size(PoolId, Num) when is_atom(PoolId), is_integer(Num) ->
	Conns = mysql_conn:open_n_connections(PoolId, Num),
	mysql_conn_mgr:add_connections(PoolId, Conns).
	
decrement_pool_size(PoolId, Num) when is_atom(PoolId), is_integer(Num) ->
	Conns = mysql_conn_mgr:remove_connections(PoolId, Num),
	[mysql_conn:close_connection(Conn) || Conn <- Conns],
	ok.
	
execute(PoolId, Query) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) ->
	execute(PoolId, Query, []);
	
execute(PoolId, StmtName) when is_atom(PoolId), is_atom(StmtName) ->
	execute(PoolId, StmtName, []).
		
execute(PoolId, Query, Args) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
	execute(PoolId, Query, Args, mysql_tcp:timeout());
	
execute(PoolId, StmtName, Args) when is_atom(PoolId), is_atom(StmtName), is_list(Args) ->
	execute(PoolId, StmtName, Args, mysql_tcp:timeout());
	
execute(PoolId, Query, Timeout) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_integer(Timeout) ->
	execute(PoolId, Query, [], Timeout);
	
execute(PoolId, StmtName, Timeout) when is_atom(PoolId), is_atom(StmtName), is_integer(Timeout) ->
	execute(PoolId, StmtName, [], Timeout).

%% @spec execute(PoolId, Query, Args, Timeout) -> Result
%%		 PoolId = atom()
%%		 Query = binary() | string()
%%		 Args = [any()]
%%		 Timeout = integer() (millisecond query timeout)
%%		 Result = mysql_ok_packet() | mysql_result_packet() | mysql_error_packet()	
execute(PoolId, Query, Args, Timeout) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	Connection = mysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {mysql_conn, execute, [Connection, Query, Args]});
	
%% @spec execute(PoolId, StmtName, Args, Timeout) -> Result
%%		 PoolId = atom()
%%		 StmtName = atom()
%%		 Args = [any()]
%%		 Timeout = integer() (millisecond query timeout)
%%		 Result = mysql_ok_packet() | mysql_result_packet() | mysql_error_packet()
execute(PoolId, StmtName, Args, Timeout) when is_atom(PoolId), is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	Connection = mysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {mysql_conn, execute, [Connection, StmtName, Args]}).
	
%%
%% NON-BLOCKING CONNECTION LOCKING
%% non-blocking means the process will not attempt to wait in line
%% until a connection is available. If no connections are available
%% then the result of these functions will be the atom unavailable.
%%
execute(PoolId, Query, Args, Timeout, nonblocking) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	case mysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {mysql_conn, execute, [Connection, Query, Args]});
		Other ->
			Other
	end;

execute(PoolId, StmtName, Args, Timeout, nonblocking) when is_atom(PoolId), is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	case mysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {mysql_conn, execute, [Connection, StmtName, Args]});
		Other ->
			Other
	end.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------	
monitor_work(Connection, Timeout, {M,F,A}) when is_record(Connection, connection) ->
	%% spawn a new process to do work, then monitor that process until
	%% it either dies, returns data or times out.
	Parent = self(),
	Pid = spawn(
		fun() ->
			Parent ! {self(), apply(M, F, A)}
		end),
	Mref = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mref, process, Pid, Reason} ->
			%% if the process dies, reset the connection
			%% and re-throw the error on the current pid
			mysql_conn:reset_connection(mysql_conn_mgr:pools(), Connection),
			exit(Reason);
		{Pid, Result} ->
			%% if the process returns data, unlock the
			%% connection and collect the normal 'DOWN'
			%% message send from the child process
			erlang:demonitor(Mref),
			mysql_conn_mgr:unlock_connection(Connection),
			receive {'DOWN', Mref, process, Pid, normal} -> ok after 0 -> ok end,
			Result
	after Timeout ->
		%% if we timeout waiting for the process to return,
		%% then reset the connection and throw a timeout error
		erlang:demonitor(Mref),
		mysql_conn:reset_connection(mysql_conn_mgr:pools(), Connection),
		exit(mysql_timeout)
	end;
monitor_work(Reason, _, _) -> Reason.
