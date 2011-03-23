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


%% @doc The main Emysql module. 
%%
%% Emysql is implemented as an Erlang
%% <b>application</b>. The term has a special meaning in Erlang, see
%% [http://www.erlang.org/doc/design_principles/applications.html]
%%
%% This module exports functions to: 
%% <li><b>start</b> and <b>stop</b> the driver (the 'application'),</li>
%% <li><b>execute</b> queries or prepared statements,</li>
%% <li><b>prepare</b> such statements,</li>
%% <li>change the <b>connection pool</b> size.</li>
%% 
%% === Sample ===
%% ```
%% 	-module(sample).
%%	-export([run/0]).
%%	
%%	run() ->
%%	
%%		crypto:start(),
%%		emysql:start(),
%%	
%%		emysql:add_pool(hello_pool, 1,
%%			"hello_username", "hello_password", "localhost", 3306,
%%			"hello_database", utf8),
%%	
%%		emysql:execute(hello_pool,
%%			<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),
%%
%%		emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>),
%%
%%		Result = emysql:execute(mypoolname, my_stmt, [1]).
%%
%% '''
%%
%% === Implementation ===
%%
%% Under <b>Implementation</b>, you can find details about the
%% inner workings of Emysql. If you are new to Emysql, you may safely ignore
%% them.
%%
%% start(), stop(), modules() and default_timeout() are one-line 'fascades':
%% ```
%% 	start() -> application:start(emysql).                
%% 	stop() -> application:stop(emysql).                  
%% 	modules() -> emysql_app:modules().                   
%% 	default_timeout() -> emysql_app:default_timeout().   
%% '''
%%
%% execute() and prepare() are the bulk of the source
%% of this module. A lot gets repeated for default values in lesser arities.
%% The quintessential execute however is this, in execute/2:
%% ```
%% 	execute(PoolId, Query, Args, Timeout) 
%%		when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
%%		
%%			Connection = 
%%				emysql_conn_mgr:wait_for_connection(PoolId),
%%				monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query, Args]});
%% '''
%% As all executions, it uses the monitor_work/3 function to create a process to
%% asynchronously handle the execution.
%% 
%% The pool-related functions execute brief operations using the primitive
%% functions exported by `emysql_conn_mgr' and `emysql_conn_mgr'.
%% @end doc: hd feb 11

-module(emysql).

-export([	start/0, stop/0,
			add_pool/8, remove_pool/1, increment_pool_size/2, decrement_pool_size/2,
			prepare/2,
			execute/2, execute/3, execute/4, execute/5,
			default_timeout/0,
			modules/0	
		]).

% for record and constant defines
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
%% @end doc: hd feb 11
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
%% @end doc: hd feb 11
%%
stop() ->
	application:stop(emysql).

%% @spec modules() -> list()
%%
%% @doc Returns the list of Emysql modules.
%%
%% === Sample ===
%% ```
%%	$ erl
%%	1> crypto:start().
%%	2> application:start(emysql).
%%	3> emysql:modules().
%%	[emysql,emysql_auth,emysql_conn,emysql_conn_mgr,
%%	 emysql_statements,emysql_tcp,emysql_tracer,emysql_util,
%%   emysql_worker]
%% '''
%% === Implementation ===
%%
%% Simply a call to `emysql_app:modules()'.
%% @private
%% @end doc: hd feb 11
%%
modules() ->
	emysql_app:modules().

%% @spec default_timeout() -> Timeout
%%		Timeout = integer()
%%
%% @doc Returns the default timeout in milliseconds. As set in emysql.app.src,
%% or if not set, the value ?TIMEOUT as defined in include/emysql.hrl (8000ms).
%%
%% === Implementation ===
%%
%% src/emysql.app.src is a template for the emysql app file from which 
%% ebin/emysql.app is created during building, by a sed command in 'Makefile'.
%% @end doc: hd feb 11
%%
default_timeout() ->
	emysql_app:default_timeout().

%% @spec add_pool(PoolId, Size, User, Password, Host, Port, Database, Encoding) -> Result
%%		PoolId = atom()
%%		Size = integer()
%%		User = string()
%%		Password = string()
%%		Host = string()
%%		Port = integer()
%%		Database = string()
%%		Encoding = string()
%%		Result = {reply, {error, pool_already_exists}, state()} | {reply, ok, state() }
%%
%% @doc Synchronous call to the connection manager to add a pool.
%%
%% === Implementation ===
%%
%% Creates a pool record, opens n=Size connections and calls 
%% emysql_conn_mgr:add_pool() to make the pool known to the pool management.
%% emysql_conn_mgr:add_pool() is translated into a blocking gen-server call.
%% @end doc: hd feb 11

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

%% @spec remove_pool(PoolId) -> ok
%%		PoolId = atom()
%%
%% @doc Synchronous call to the connection manager to remove a pool.
%%
%% === Implementation ===
%%
%% Relies on emysql_conn:close_connection(Conn) for the proper closing of connections. Feeds
%% any connection in the pool to it, also the locked ones.
%% @end doc: hd feb 11

remove_pool(PoolId) ->
	Pool = emysql_conn_mgr:remove_pool(PoolId),
	[emysql_conn:close_connection(Conn) || Conn <- lists:append(queue:to_list(Pool#pool.available), gb_trees:values(Pool#pool.locked))],
	ok.

%% @spec increment_pool_size(PoolId, By) -> Result
%%		PoolId = atom()
%%		By = integer()
%%		Result = {reply, ok, State1} | {reply, {error, pool_not_found}, State}
%%
%% @doc Synchronous call to the connection manager to enlarge a pool.
%%
%% This opens n=By new connections and adds them to the pool of id PoolId.
%%
%% === Implementation ===
%%
%% Opens connections and then adds them to the pool by a call to
%% emysql_conn_mgr:add_connections().
%% 
%% That this function exposes the State and possibly pool_not_found
%% seems to be inconsistent with decrement_pool_size(), which invariably
%% returns 'ok'.
%% @end doc: hd feb 11

increment_pool_size(PoolId, Num) when is_integer(Num) ->
	Conns = emysql_conn:open_n_connections(PoolId, Num),
	emysql_conn_mgr:add_connections(PoolId, Conns).

%% @spec decrement_pool_size(PoolId, By) -> ok
%%		PoolId = atom()
%%		By = integer()
%%
%% @doc Synchronous call to the connection manager to shrink a pool.
%%
%% This reduces the connections by up to n=By, but it only drops and closes available
%% connections that are not in use at the moment that this function is called. Connections
%% that are waiting for a server response are never dropped. In heavy duty, this function 
%% may thus do nothing.
%%
%% If 'By' is higher than the amount of connections or the amount of available connections,
%% exactly all available connections are dropped and closed. 
%%
%%
%% === Implementation ===
%%
%% First gets a list of target connections from emysql_conn_mgr:remove_connections(), then
%% relies on emysql_conn:close_connection(Conn) for the proper closing of connections. 
%% @end doc: hd feb 11
%% 

decrement_pool_size(PoolId, Num) when is_integer(Num) ->
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
%% @end doc: hd feb 11

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
%% @see execute/3.
%% @see execute/4.
%% @see execute/5.
%% @see prepare/2.
%% @end doc: hd feb 11
%%
execute(PoolId, Query) when (is_list(Query) orelse is_binary(Query)) ->
	execute(PoolId, Query, []);

execute(PoolId, StmtName) when is_atom(StmtName) ->
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
%% @see execute/2.
%% @see execute/4.
%% @see execute/5.
%% @see prepare/2.
%% @end doc: hd feb 11
%%
execute(PoolId, Query, Args) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
	execute(PoolId, Query, Args, default_timeout());

execute(PoolId, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
	execute(PoolId, StmtName, Args, default_timeout());

execute(PoolId, Query, Timeout) when (is_list(Query) orelse is_binary(Query)) andalso is_integer(Timeout) ->
	execute(PoolId, Query, [], Timeout);

execute(PoolId, StmtName, Timeout) when is_atom(StmtName), is_integer(Timeout) ->
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
%% All other execute function eventually call this function.
%% 
%% @see execute/2.
%% @see execute/3.
%% @see execute/5.
%% @see prepare/2.
%% @end doc: hd feb 11
%%
execute(PoolId, Query, Args, Timeout) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	Connection = emysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query, Args]});

execute(PoolId, StmtName, Args, Timeout) when is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	Connection = emysql_conn_mgr:wait_for_connection(PoolId),
	monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, StmtName, Args]}).

%% @spec execute(PoolId, Query|StmtName, Args, Timeout, nonblocking) -> Result | [Result]
%%		PoolId = atom()
%%		Query = binary() | string()
%%		StmtName = atom()
%%		Args = [any()]
%%		Timeout = integer()
%%		Result = ok_packet() | result_packet() | error_packet()
%%
%% @doc Execute a query, prepared statement or a stored procedure - but return immediately, returning the atom 'unavailable', when no connection in the pool is readily available without wait.
%%
%% <ll>
%% <li>Checks if a connection is available,</li>
%% <li>returns 'unavailable' if not,</li>
%% <li>else as the other exception functions(): sends the query string, or statement atom, and</li>
%% <li>returns the result packet.</li>
%% </ll>
%%
%% Timeout is the query timeout in milliseconds.
%%
%% ==== Implementation ====
%%
%% Basically:
%% ```
%% {Connection, connection} = case emysql_conn_mgr:lock_connection(PoolId),
%% 		monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query_or_StmtName, Args]}).
%% '''
%%
%% The result is a list for stored procedure execution >= MySQL 4.1
%%
%% All other execute function eventually call this function.
%% 
%% @see execute/2.
%% @see execute/3. 
%% @see execute/4. 
%% @see prepare/2.
%% @end doc: hd feb 11
%%
execute(PoolId, Query, Args, Timeout, nonblocking) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) andalso is_integer(Timeout) ->
	case emysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, Query, Args]});
		Other ->
			Other
	end;

execute(PoolId, StmtName, Args, Timeout, nonblocking) when is_atom(StmtName), is_list(Args) andalso is_integer(Timeout) ->
	case emysql_conn_mgr:lock_connection(PoolId) of
		Connection when is_record(Connection, connection) ->
			monitor_work(Connection, Timeout, {emysql_conn, execute, [Connection, StmtName, Args]});
		Other ->
			Other
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% @spec monitor_work(Connection, Timeout, {M, F, A}) -> Result | exit()
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
%% @see execute/2.
%% @see execute/3.
%% @see execute/4.
%% @see execute/5.
%% @see prepare/2.
%% 
%% @private 
%% @end doc: hd feb 11
%%
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
		{'DOWN', Mref, process, Pid, {_, closed}} ->
			NewConnection = emysql_conn:renew_connection(emysql_conn_mgr:pools(), Connection),
			[_ | Args] = A,
		  monitor_work(NewConnection, Timeout ,{M, F, [NewConnection | Args]});
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
