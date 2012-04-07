%% -*- mode: erlang;erlang-indent-level: 8;indent-tabs-mode:t -*-
%% Copyright (c) 2009-2012
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

-module(emysql_conn_mgr).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([pools/0, add_pool/1, remove_pool/1,
		add_connections/2, remove_connections/2,
		lock_connection/1, wait_for_connection/1,
		pass_connection/1, 
		replace_connection_as_available/2, replace_connection_as_locked/2,
		find_pool/2]).

-include("emysql.hrl").

-record(state, {pools}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

pools() ->
	gen_server:call(?MODULE, pools, infinity).

add_pool(Pool) ->
	do_gen_call({add_pool, Pool}).

remove_pool(PoolId) ->
	do_gen_call({remove_pool, PoolId}).

add_connections(PoolId, Conns) when is_list(Conns) ->
	do_gen_call({add_connections, PoolId, Conns}).

remove_connections(PoolId, Num) when is_integer(Num) ->
	do_gen_call({remove_connections, PoolId, Num}).

lock_connection(PoolId)->
	do_gen_call({lock_connection, PoolId}).

wait_for_connection(PoolId)->
	%% try to lock a connection. if no connections are available then
	%% wait to be notified of the next available connection
    %-% io:format("~p waits for connection to pool ~p~n", [self(), PoolId]),
        case do_gen_call({lock_connection_or_wait, PoolId}) of
		unavailable ->
            %-% io:format("~p is queued~n", [self()]),
			receive
				{connection, Connection} -> 
                    %-% io:format("~p gets a connection after waiting in queue~n", [self()]),
    				Connection
			after lock_timeout() ->
                %-% io:format("~p gets no connection and times out -> EXIT~n~n", [self()]),
				case do_gen_call({end_wait, PoolId}) of
					ok ->
						exit(connection_lock_timeout);
					not_waiting ->
						%% If we aren't waiting, then someone must
						%% have sent us a connection mssage at the
						%% same time that we timed out.
						receive_connection_not_waiting()
				end
			end;
		Connection ->
            %-% io:format("~p gets connection~n", [self()]),
			Connection
	end.

pass_connection(Connection) ->
	do_gen_call({pass_connection, Connection}).

replace_connection_as_available(OldConn, NewConn) ->
	do_gen_call({replace_connection_as_available, OldConn, NewConn}).

replace_connection_as_locked(OldConn, NewConn) ->
	do_gen_call({replace_connection_as_locked, OldConn, NewConn}).

%% the stateful loop functions of the gen_server never
%% want to call exit/1 because it would crash the gen_server.
%% instead we want to return error tuples and then throw
%% the error once outside of the gen_server process
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
	Pools1 = [emysql_conn:open_connections(Pool) || Pool <- Pools],
	{ok, #state{pools=Pools1}}.

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

handle_call({add_pool, Pool}, _From, State) ->
	case find_pool(Pool#pool.pool_id, State#state.pools) of
		{_, _} ->
			{reply, {error, pool_already_exists}, State};
		undefined ->
			{reply, ok, State#state{pools = [Pool|State#state.pools]}}
	end;

handle_call({remove_pool, PoolId}, _From, State) ->
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			{reply, Pool, State#state{pools=OtherPools}};
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({add_connections, PoolId, Conns}, _From, State) ->
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			OtherConns = Pool#pool.available,
			State1 = State#state{
				pools = [Pool#pool{available = queue:join(queue:from_list(Conns), OtherConns)}|OtherPools]
			},
			{reply, ok, State1};
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({remove_connections, PoolId, Num}, _From, State) ->
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			case Num > queue:len(Pool#pool.available) of
				true ->
					State1 = State#state{pools = [Pool#pool{available = queue:new()}]},
					{reply, queue:to_list(Pool#pool.available), State1};
				false ->
					{Conns, OtherConns} = queue:split(Num, Pool#pool.available),
					State1 = State#state{pools = [Pool#pool{available = OtherConns}|OtherPools]},
					{reply, queue:to_list(Conns), State1}
			end;
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({lock_connection_or_wait, PoolId}, {From, _Mref}, State) ->
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			case lock_next_connection(State, Pool, OtherPools) of
				{ok, NewConn, State1} ->
					{reply, NewConn, State1};
				unavailable ->
					%% place the calling pid at the end of the waiting queue of its pool
					PoolNow = Pool#pool{ waiting = queue:in(From, Pool#pool.waiting) },
					{reply, unavailable, State#state{pools=[PoolNow|OtherPools]}}
			end;
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({end_wait, PoolId}, {From, _Mref}, State) ->
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			%% Remove From from the wait queue
			QueueNow = queue:filter(
				     fun(Pid) -> Pid =/= From end,
				     Pool#pool.waiting),
			PoolNow = Pool#pool{ waiting = QueueNow },
			%% See if the length changed to know if From was removed.
			OldLen = queue:len(Pool#pool.waiting),
			NewLen = queue:len(QueueNow),
			if
				OldLen =:= NewLen ->
					Reply = not_waiting;
				true ->
					Reply = ok
			end,
			{reply, Reply, State#state{pools=[PoolNow|OtherPools]}};
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({lock_connection, PoolId}, _From, State) ->
	%% find the next available connection in the pool identified by PoolId
    %-% io:format("gen srv: lock connection for pool ~p~n", [PoolId]),
	case find_pool(PoolId, State#state.pools) of
		{Pool, OtherPools} ->
			case lock_next_connection(State, Pool, OtherPools) of
				{ok, NewConn, State1} ->
					{reply, NewConn, State1};
				Other ->
					{reply, Other, State}
			end;
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({pass_connection, Connection}, _From, State) ->
	{Result, State1} = pass_on_or_queue_as_available(State, Connection),
	{reply, Result, State1};

handle_call({replace_connection_as_available, OldConn, NewConn}, _From, State) ->
	%% if an error occurs while doing work over a connection then
	%% the connection must be closed and a new one created in its
	%% place. The calling process is responsible for creating the
	%% new connection, closing the old one and replacing it in state.
	%% This function expects a new, available connection to be
	%% passed in to serve as the replacement for the old one.
	%% But i.e. if the sql server is down, it can be fed a dead
	%% old connection as new connection, to preserve the pool size.
	case find_pool(OldConn#emysql_connection.pool_id, State#state.pools) of
		{Pool, OtherPools} ->
			Pool1 = Pool#pool{
				available = queue:in(NewConn, Pool#pool.available),
				locked = gb_trees:delete_any(OldConn#emysql_connection.id, Pool#pool.locked)
			},
			{reply, ok, State#state{pools=[Pool1|OtherPools]}};
		undefined ->
			{reply, {error, pool_not_found}, State}
	end;

handle_call({replace_connection_as_locked, OldConn, NewConn}, _From, State) ->
	%% replace an existing, locked condition with the newly supplied one
	%% and keep it in the locked list so that the caller can continue to use it
	%% without having to lock another connection.
	case find_pool(OldConn#emysql_connection.pool_id, State#state.pools) of
		{Pool, OtherPools} ->
            LockedStripped = gb_trees:delete_any(OldConn#emysql_connection.id, Pool#pool.locked),
            LockedAdded = gb_trees:enter(NewConn#emysql_connection.id, NewConn, LockedStripped),
		    Pool1 = Pool#pool{locked = LockedAdded},
			{reply, ok, State#state{pools=[Pool1|OtherPools]}};
		undefined ->
			{reply, {error, pool_not_found}, State}
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
	%% if the emysql application values are not present in the config
	%% file we will initialize and empty set of pools. Otherwise, the
	%% values defined in the config are used to initialize the state.
	[
		#pool{
			pool_id = PoolId,
			size = proplists:get_value(size, Props, 1),
			user = proplists:get_value(user, Props),
			password = proplists:get_value(password, Props),
			host = proplists:get_value(host, Props),
			port = proplists:get_value(port, Props),
			database = proplists:get_value(database, Props),
			encoding = proplists:get_value(encoding, Props)
		} || {PoolId, Props} <- emysql_app:pools()
	].

find_pool(PoolId, Pools) ->
    find_pool(PoolId, Pools, []).

find_pool(_, [], _) -> undefined;

find_pool(PoolId, [#pool{pool_id = PoolId} = Pool|Tail], OtherPools) ->
	{Pool, lists:append(OtherPools, Tail)};

find_pool(PoolId, [Pool|Tail], OtherPools) ->
	find_pool(PoolId, Tail, [Pool|OtherPools]).

lock_next_connection(State, Pool, OtherPools) ->
	% check no of connection in Pool
	%-% io:format("~p Pool ~p Connections available: ~p~n", [self(), PoolId, queue:len(Pool#pool.available)]),
	%-% io:format("~p Pool ~p Connections locked: ~p~n", [self(), PoolId, gb_trees:size(Pool#pool.locked)]),
	case queue:out(Pool#pool.available) of
		{{value, Conn}, OtherConns} ->
			%-% io:format("gen srv: lock connection ... found a good next connection~n", []),
			NewConn = Conn#emysql_connection{locked_at=lists:nth(2, tuple_to_list(now()))},
			Locked = gb_trees:enter(NewConn#emysql_connection.id, NewConn, Pool#pool.locked),
			State1 = State#state{pools = [Pool#pool{available=OtherConns, locked=Locked}|OtherPools]},
			{ok, Conn, State1};
		{empty, _} ->
			unavailable
	end.

%% This function does not wait, but may loop over the queue.
pass_on_or_queue_as_available(State, Connection) ->

    % get the pool that this connection belongs to
	case find_pool(Connection#emysql_connection.pool_id, State#state.pools) of

		{Pool, OtherPools} ->

        	%% check if any processes are waiting for a connection
		    Waiting = Pool#pool.waiting,
        	case queue:is_empty(Waiting) of

        		%% if no processes are waiting then unlock the connection
	        	true ->

		        	%% find connection in locked tree
			        case gb_trees:lookup(Connection#emysql_connection.id, Pool#pool.locked) of

				        {value, Conn} ->

    	    				%% add the connection to the 'available' queue and remove from 'locked' tree
	    	    			Pool1 = Pool#pool{
		    	    			available = queue:in(Conn#emysql_connection{locked_at=undefined}, Pool#pool.available),
			    	    		locked = gb_trees:delete_any(Connection#emysql_connection.id, Pool#pool.locked)
				    	    },
    				    	{ok, State#state{pools = [Pool1|OtherPools]}};

	    	    		none ->
		    	    		{{error, connection_not_found}, State}
    		    	end;

        		%% if the waiting queue is not empty then remove the head of
	        	%% the queue and send it the connection.
			%% Update the pool & queue in state once the head has been removed.
			false ->

				{{value, Pid}, OtherWaiting} = queue:out(Waiting),
   	    			PoolNow = Pool#pool{ waiting = OtherWaiting },
			    	StateNow = State#state{ pools = [PoolNow|OtherPools] },
				erlang:send(Pid, {connection, Connection}),
				{ok, StateNow}
		end;

		%% pool not found    
		undefined ->
			{{error, pool_not_found}, State}
	end.

lock_timeout() ->
	emysql_app:lock_timeout().


%% This is called after we timed out, but discovered that we weren't waiting for a
%% connection.
receive_connection_not_waiting() ->
	receive
		{connection, Connection} -> 
			%%-% io:format("~p gets a connection after timeout in queue~n", [self()]),
			Connection
	after 
		%% This should never happen, as we should only be here if we had been sent a connection
		lock_timeout() ->
			%%-% io:format("~p gets no connection and times out again -> EXIT~n~n", [self()]),
			exit(connection_lock_second_timeout)
	end.
