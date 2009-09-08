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
-module(mysql_conn_mgr).
-behaviour(gen_server).

-export([start_link/0, start_link/8, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([pools/0, waiting/0, lock_connection/1, wait_for_connection/1, 
		 unlock_connection/1, replace_connection/2, find_pool/3, 
		 open_connection/1]).

-include("emysql.hrl").

-record(state, {pools, waiting=queue:new()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
start_link(PoolId, Size, User, Password, Host, Port, Database, Encoding) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolId, Size, User, Password, Host, Port, Database, Encoding], []).
	
pools() ->
	gen_server:call(?MODULE, pools, infinity).

waiting() ->
	gen_server:call(?MODULE, waiting, infinity).
		
lock_connection(PoolId) when is_atom(PoolId) ->
	do_gen_call({lock_connection, PoolId});

lock_connection(Connection) when is_record(Connection, connection) ->
	do_gen_call({lock_connection, Connection#connection.pool_id, Connection}).

wait_for_connection(Arg) when is_atom(Arg); is_record(Arg, connection) ->
	case lock_connection(Arg) of
		unavailable ->
			gen_server:call(?MODULE, start_wait, infinity),
			receive
				{connection, Connection} -> Connection
			after 5000 ->
				gen_server:call(?MODULE, cancel_wait, infinity),
				exit(connection_lock_timeout)
			end;
		Connection ->
			Connection
	end.
	
unlock_connection(Connection) ->
	do_gen_call({unlock_connection, Connection}).
	
replace_connection(OldConn, NewConn) ->
	do_gen_call({replace_connection, OldConn, NewConn}).

do_gen_call(Msg) ->
	case gen_server:call(?MODULE, Msg, infinity) of
		{error, Reason} ->
			exit(Reason);
		Result ->
			Result
	end.
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	Pools = initialize_pools(),
	Pools1 = [open_connections(Pool) || Pool <- Pools],
	{ok, #state{pools=Pools1}};
	
init([PoolId, Size, User, Password, Host, Port, Database, Encoding]) ->
	Pools = [
		open_connections(
			#pool{
				pool_id = PoolId, 
				size = Size,
				user = User,
				password = Password, 
				host = Host, 
				port = Port, 
				database = Database, 
				encoding = Encoding, 
				connections=[]
			}
		)
	],
	{ok, #state{pools=Pools}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(pools, _From, State) ->
	{reply, State#state.pools, State};
	
handle_call(waiting, _From, State) ->
	{reply, State#state.waiting, State};
		
handle_call(start_wait, {From, _Mref}, State) ->
	State1 = State#state{
		waiting = queue:in(From, State#state.waiting)
	},
	{reply, ok, State1};
	
handle_call(cancel_wait, {From, _Mref}, State) ->
	State1 = State#state{
		waiting = queue:filter(fun(Pid) -> Pid =/= From end, State#state.waiting)
	},
	{reply, ok, State1};
	
handle_call({lock_connection, PoolId}, _From, State) ->
	case find_next_connection_in_pool(State#state.pools, PoolId) of
		[Pool, OtherPools, Conn, OtherConns] ->
			NewConn = Conn#connection{state=locked},
			State1 = State#state{pools = [Pool#pool{connections=[NewConn|OtherConns]}|OtherPools]},
			{reply, NewConn, State1};
		Other ->
			{reply, Other, State}
	end;
	
handle_call({lock_connection, _PoolId, Connection}, _From, State) ->
	case find_connection_in_pool(State#state.pools, Connection) of
		[Pool, OtherPools, Conn, OtherConns] ->
			NewConn = Conn#connection{state=locked},
			State1 = State#state{pools = [Pool#pool{connections=[NewConn|OtherConns]}|OtherPools]},
			{reply, NewConn, State1};
		Other ->
			{reply, Other, State}		
	end;
	
handle_call({unlock_connection, Connection}, _From, State) ->
	case queue:is_empty(State#state.waiting) of
		true ->
			case find_connection_in_pool(State#state.pools, Connection) of
				[Pool, OtherPools, Conn, OtherConns] ->
					State1 = State#state{pools = [Pool#pool{connections=[Conn#connection{state=available}|OtherConns]}|OtherPools]},
					{reply, ok, State1};
				Other ->
					{reply, Other, State}
			end;
		false ->
			{{value, Pid}, Waiting} = queue:out(State#state.waiting),
			erlang:send(Pid, {connection, Connection}),
			{reply, ok, State#state{waiting = Waiting}}
	end;

handle_call({replace_connection, OldConn, NewConn}, _From, State) ->
	case find_connection_in_pool(State#state.pools, OldConn) of
		[Pool, OtherPools, _Conn, OtherConns] ->
			State1 = State#state{pools = [Pool#pool{connections=[NewConn|OtherConns]}|OtherPools]},
			{reply, ok, State1};
		Other ->
			{reply, Other, State}	
	end;
	
handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initialize_pools() ->
	case application:get_env(emysql, pools) of
		undefined ->
			[];
		{ok, Pools} ->
			[begin
				#pool{
					pool_id = PoolId, 
					size = proplists:get_value(size, Props, 1),
					user = proplists:get_value(user, Props),
					password = proplists:get_value(password, Props), 
					host = proplists:get_value(host, Props), 
					port = proplists:get_value(port, Props), 
					database = proplists:get_value(database, Props), 
					encoding = proplists:get_value(encoding, Props), 
					connections=[]
				}
			 end || {PoolId, Props} <- Pools]
	end.

open_connections(#pool{connections=Conns}=Pool) when Pool#pool.size > 0, length(Conns) < Pool#pool.size ->
	Conn = open_connection(Pool),
	open_connections(Pool#pool{
		connections = [Conn|Conns]
	});	
open_connections(Pool) ->
	Pool.
	
open_connection(#pool{pool_id=PoolId, host=Host, port=Port, user=User, password=Password, database=Database, encoding=Encoding}) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
		{ok, Sock} ->
			Greeting = mysql_auth:do_handshake(Sock, User, Password),
			Connection = #connection{
				id = erlang:port_to_list(Sock),
				pool_id = PoolId,
				socket = Sock,
				version = Greeting#greeting.server_version,
				thread_id = Greeting#greeting.thread_id,
				caps = Greeting#greeting.caps,
				language = Greeting#greeting.language
			},
			mysql_conn:set_database(Connection, Database),
			mysql_conn:set_encoding(Connection, Encoding),
			Connection;
		{error, Reason} ->
			exit({failed_to_connect_to_database, Reason})
	end.
	
find_pool(_, [], _) -> undefined;

find_pool(PoolId, [#pool{pool_id = PoolId} = Pool|Tail], OtherPools) ->
	{Pool, lists:append(OtherPools, Tail)};
	
find_pool(PoolId, [Pool|Tail], OtherPools) ->
	find_pool(PoolId, Tail, [Pool|OtherPools]).
	
find_connection(_, [], _) -> undefined;

find_connection(ConnID, [#connection{id=ConnID}=Conn|Tail], OtherConns) ->
	{Conn, lists:append(OtherConns, Tail)};

find_connection(ConnID, [Conn|Tail], OtherConns) ->
	find_connection(ConnID, Tail, [Conn|OtherConns]).

find_next_available(List) ->
	find_next_available(List, []).
	
find_next_available([#connection{state=available}=Conn|Tail], Rest) ->
	{Conn, lists:append(Rest, Tail)};
	
find_next_available([Conn|Tail], Rest) ->
	find_next_available(Tail, [Conn|Rest]);
	
find_next_available([], _) ->
	undefined.

find_connection_in_pool(Pools, Connection) ->
	case find_pool(Connection#connection.pool_id, Pools, []) of
		{Pool, OtherPools} ->
			if
				length(Pool#pool.connections) > 0 ->
					case find_connection(Connection#connection.id, Pool#pool.connections, []) of
						{Conn, OtherConns} ->
							[Pool, OtherPools, Conn, OtherConns];
						undefined ->
							{error, connection_not_found}
					end;
				true ->
					{error, connection_pool_is_empty}
			end;
		undefined ->
			{error, pool_not_found}
	end.

find_next_connection_in_pool(Pools, PoolId) ->
	case find_pool(PoolId, Pools, []) of
		{Pool, OtherPools} ->
			if
				length(Pool#pool.connections) > 0 ->
					case find_next_available(Pool#pool.connections) of
						undefined ->
							unavailable;
						{Conn, OtherConns} ->
							[Pool, OtherPools, Conn, OtherConns]
					end;
				true ->
					{error, connection_pool_is_empty}
			end;
		undefined ->
			{error, pool_not_found}
	end.
	