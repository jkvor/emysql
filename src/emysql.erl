%% Copyright (c) 2009-2011
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Henning Diedrich <hd2010@eonblast.com>
%% Eonblast Corporation <http://www.eonblast.com>
%% 
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy, 
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following 
%% conditions:
%% 
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.


-module(emysql).

-export([start/0, stop/0, modules/0, default_timeout/0]).
-export([add_pool/8, remove_pool/1, prepare/2,
	increment_pool_size/2, decrement_pool_size/2,
	execute/2, execute/3, execute/4, execute/5]).

-include("emysql.hrl").

%% @spec start() -> ok
%% @doc Start the Emysql application.
%%
%% Simply calls `application:start(emysql).'
%%
%% === From the Erlang Manual ===
%% If the application is not already loaded, the application controller will
%% first load it using application:load/1. It will check the value of the
%% applications key, to ensure that all applications that should be started
%% before this application are running. The application controller then
%% creates an application master for the application. The application master
%% is the group leader of all the processes in the application. The
%% application master starts the application by calling the application
%% callback function start/2 in the module, and with the start argument,
%% defined by the mod key in the .app file.
%%
%% application:start(Application) is the same as calling 
%% application:start(Application, temporary). If a temporary application
%% terminates, this is reported but no other applications are terminated.
%%
%% See [http://www.erlang.org/doc/design_principles/applications.html]
%%
start() ->
	application:start(emysql).

%% @spec stop() -> ok
%% @doc Stop the Emysql application.
%% 
%% Simply calls `application:stop(emysql).'
%%
%% === From the Erlang Manual ===
%% It is always possible to stop an application explicitly by calling
%% application:stop/1. Regardless of the mode, no other applications will be
%% affected.
%%
%% See [http://www.erlang.org/doc/design_principles/applications.html]
%%
stop() ->
	application:stop(emysql).

modules() ->
	emysql_app:modules().

default_timeout() ->
	emysql_app:default_timeout().

add_pool(PoolId, Size, User, Password, Host, Port, Database, Encoding) ->
	Pool = #pool{
		pool_id = PoolId,
		size = Size,
		user = User,
		password = Password,
		host = Host,
		port = Port,
		database = Database,
		encoding = Encoding
	},
	Pool1 = emysql_conn:open_connections(Pool),
	emysql_conn_mgr:add_pool(Pool1).

remove_pool(PoolId) ->
	Pool = emysql_conn_mgr:remove_pool(PoolId),
	[emysql_conn:close_connection(Conn) || Conn <- lists:append(queue:to_list(Pool#pool.available), gb_trees:values(Pool#pool.locked))],
	ok.

increment_pool_size(PoolId, Num) when is_atom(PoolId), is_integer(Num) ->
	Conns = emysql_conn:open_n_connections(PoolId, Num),
	emysql_conn_mgr:add_connections(PoolId, Conns).

decrement_pool_size(PoolId, Num) when is_atom(PoolId), is_integer(Num) ->
	Conns = emysql_conn_mgr:remove_connections(PoolId, Num),
	[emysql_conn:close_connection(Conn) || Conn <- Conns],
	ok.

%% @spec prepare(StmtName, Statement) -> ok
%%		StmtName = atom()
%%		Statement = binary() | string()
%%
%% @doc Prepare a statement.
%% 
%% The atom given by parameter 'StmtName' is bound to the SQL string
%% 'Statement'. Calling ``execute(<Pool>, StmtName, <ParamList>)'' executes the
%% statement with parameters from ``<ParamList>''.
%%
%% This is not a mySQL prepared statement, but an implementation on the side of 
%% Emysql.
%%
%% === Sample ===
%% ```
%% -module(sample).
%% -export([run/0]).
%% 
%% run() ->
%% 
%% 	application:start(sasl),
%% 	crypto:start(),
%% 	application:start(emysql),
%% 
%% 	emysql:add_pool(hello_pool, 1,
%% 		"hello_username", "hello_password", "localhost", 3306,
%% 		"hello_database", utf8),
%% 
%% 	emysql:execute(hello_pool,
%% 		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),
%% 
%% 	emysql:prepare(hello_stmt, 
%% 		<<"SELECT * from hello_table WHERE hello_text like ?">>),
%% 
%% 	Result = emysql:execute(hello_pool, hello_stmt, ["Hello%"]),
%% 
%% 	io:format("~n~s~n", [string:chars($-,72)]),
%% 	io:format("~p~n", [Result]),
%% 
%%     ok.
%% '''
%% Output:
%% ```
%% {result_packet,32,
%%               [{field,2,<<"def">>,<<"hello_database">>,
%%                        <<"hello_table">>,<<"hello_table">>,
%%                        <<"hello_text">>,<<"hello_text">>,254,<<>>,33,
%%                        60,0,0}],
%%                [[<<"Hello World!">>]],
%%                <<>>}
%% ''' 
%% === Implementation ===
%%
%% Hands parameters over to emysql_statements:add/2:
%% ``emysql_statements:add(StmtName, Statement).'', which calls 
%% ``handle_call({add, StmtName, Statement}, _From, State)''.
%%
%% The statement is there added to the Emysql statement GB tree:
%% ... ```
%%				State#state{
%%					statements = gb_trees:enter(StmtName, {1, Statement},
%%						State#state.statements)
%% '''
%% Execution is called like this:
%% ```
%% execute(Connection, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
%% 	prepare_statement(Connection, StmtName),
%% 	case set_params(Connection, 1, Args, undefined) of
%% 		OK when is_record(OK, ok_packet) ->
%% 			ParamNamesBin = list_to_binary(string:join([[$@ | integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", ")),
%% 			StmtNameBin = atom_to_binary(StmtName, utf8),
%% 			Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary, " USING ", ParamNamesBin/binary>>,
%% 			emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
%% 		Error ->
%% 			Error
%% 	end.
%% '''
%%
%% @see emysql_statements:add/2
%% @see emysql_statements:handle/3
%% @see emysql_conn:execute/3

prepare(StmtName, Statement) when is_atom(StmtName) andalso (is_list(Statement) orelse is_binary(Statement)) ->
	emysql_statements:add(StmtName, Statement).

%% @spec execute(PoolId, Query|StmtName) -> Result | [Result]
%%		PoolId = atom()
%%		Query = binary() | string()
%%		StmtName = atom()
%%		Result = ok_packet() | result_packet() | error_packet()
%%
%% @doc Execute a query, prepared statement or a stored procedure.
%%
%% Same as `execute(PoolId, Query, [], default_timeout())'.
%%
%% The result is a list for stored procedure execution >= MySQL 4.1
%%
%% @see execute/4.
%% @see prepare/2.
%%
execute(PoolId, Query) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) ->
	execute(PoolId, Query, []);

execute(PoolId, StmtName) when is_atom(PoolId), is_atom(StmtName) ->
	execute(PoolId, StmtName, []).

%% @spec execute(PoolId, Query|StmtName, Args|Timeout) -> Result | [Result]
%%		PoolId = atom()
%%		Query = binary() | string()
%%		StmtName = atom()
%%		Args = [any()]
%%		Timeout = integer()
%%		Result = ok_packet() | result_packet() | error_packet()
%%
%% @doc Execute a query, prepared statement or a stored procedure.
%%
%% Same as `execute(PoolId, Query, Args, default_timeout())' 
%% or `execute(PoolId, Query, [], Timeout)'.
%%
%% Timeout is the query timeout in milliseconds.
%%
%% The result is a list for stored procedure execution >= MySQL 4.1
%%
%% @see execute/4.
%% @see prepare/2.
%%
execute(PoolId, Query, Args) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
	execute(PoolId, Query, Args, default_timeout());

execute(PoolId, StmtName, Args) when is_atom(PoolId), is_atom(StmtName), is_list(Args) ->
	execute(PoolId, StmtName, Args, default_timeout());

execute(PoolId, Query, Timeout) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_integer(Timeout) ->
	execute(PoolId, Query, [], Timeout);

execute(PoolId, StmtName, Timeout) when is_atom(PoolId), is_atom(StmtName), is_integer(Timeout) ->
	execute(PoolId, StmtName, [], Timeout).

%% @spec execute(PoolId, Query|StmtName, Args, Timeout) -> Result | [Result]
%%		PoolId = atom()
%%		Query = binary() | string()
%%		StmtName = atom()
%%		Args = [any()]
%%		Timeout = integer()
%%		Result = ok_packet() | result_packet() | error_packet()
%%
%% @doc Execute a query, prepared statement or a stored procedure.
%%
%% <ll>
%% <li>Opens a connection,</li>
%% <li>sends the query string, or statement atom, and</li>
%% <li>returns the result packet.</li>
%% </ll>
%%
%% Basically:
%% ```
%% Connection = emysql_conn_mgr:wait_for_connection(PoolId),
%% monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query_or_StmtName, Args]}).
%% '''
%% Timeout is the query timeout in milliseconds.
%%
%% The result is a list for stored procedure execution >= MySQL 4.1
%%
%% All other execute function eventually call this function.
%% 
%% @see execute/2.
%% @see execute/3. 
%% @see prepare/2.
%%
execute(PoolId, Query, Args, Timeout) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	Connection = emysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query, Args]});

execute(PoolId, StmtName, Args, Timeout) when is_atom(PoolId), is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	Connection = emysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, StmtName, Args]}).

%% NON-BLOCKING CONNECTION LOCKING
%% non-blocking means the process will not attempt to wait in line
%% until a connection is available. If no connections are available
%% then the result of these functions will be the atom unavailable.
%%
execute(PoolId, Query, Args, Timeout, nonblocking) when is_atom(PoolId) andalso (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	case emysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query, Args]});
		Other ->
			Other
	end;

execute(PoolId, StmtName, Args, Timeout, nonblocking) when is_atom(PoolId), is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	case emysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, StmtName, Args]});
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
			receive start ->
				Parent ! {self(), apply(M, F, A)}
			end
		end),
	Mref = erlang:monitor(process, Pid),
	Pid ! start,
	receive
		{'DOWN', Mref, process, Pid, Reason} ->
			%% if the process dies, reset the connection
			%% and re-throw the error on the current pid
			emysql_conn:reset_connection(emysql_conn_mgr:pools(), Connection),
			exit(Reason);
		{Pid, Result} ->
			%% if the process returns data, unlock the
			%% connection and collect the normal 'DOWN'
			%% message send from the child process
			erlang:demonitor(Mref, [flush]),
			emysql_conn_mgr:unlock_connection(Connection),
			Result
	after Timeout ->
		%% if we timeout waiting for the process to return,
		%% then reset the connection and throw a timeout error
		erlang:demonitor(Mref),
		exit(Pid, normal),
		emysql_conn:reset_connection(emysql_conn_mgr:pools(), Connection),
		exit(mysql_timeout)
	end.
