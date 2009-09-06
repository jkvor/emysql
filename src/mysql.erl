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
-export([execute/2, execute/3]).

start(_, _) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) -> ok.

init(_) ->
   {ok, {{one_for_one, 10, 10}, [
       {mysql_conn_mgr, {mysql_conn_mgr, start_link, []}, permanent, 5000, worker, [mysql_conn_mgr]}
   ]}}.

modules() ->
    {ok, Modules} = application_controller:get_key(emysql, modules), 
	Modules.

execute(PoolId, Query) ->
	execute(PoolId, Query, mysql_tcp:timeout()).
	
execute(PoolId, Query, Timeout) ->
	Connection = mysql_conn_mgr:lock_connection(PoolId),
	monitor_work(Connection, Timeout, {mysql_conn, execute, [Connection, Query]}).
	
monitor_work(Connection, Timeout, {M,F,A}) ->
	Parent = self(),
	Pid = spawn(
		fun() ->
			Parent ! {self(), apply(M, F, A)}
		end),
	Mref = erlang:monitor(process, Pid),
	receive
		{'DOWN', Mref, process, Pid, Reason} ->
			mysql_conn_mgr:reset_connection(Connection),
			exit(Reason);
		{Pid, Result} ->
			erlang:demonitor(Mref),
			mysql_conn_mgr:unlock_connection(Connection),
			receive {'DOWN', Mref, process, Pid, normal} -> ok after 0 -> ok end,
			Result
	after Timeout ->
		erlang:demonitor(Mref),
		mysql_conn_mgr:reset_connection(Connection),
		exit(mysql_timeout)
	end.
		