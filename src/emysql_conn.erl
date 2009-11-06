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
-module(emysql_conn).
-export([set_database/2, set_encoding/2,
		 execute/3, prepare/3, unprepare/2,
		 open_connections/1, open_connection/1, 
		 reset_connection/2, close_connection/1,
		 open_n_connections/2
]).

-include("emysql.hrl").

set_database(_, undefined) -> ok;
set_database(Connection, Database) ->
	Packet = <<?COM_QUERY, "use ", (iolist_to_binary(Database))/binary>>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).
	
set_encoding(Connection, Encoding) ->
	Packet = <<?COM_QUERY, "set names '", (erlang:atom_to_binary(Encoding, utf8))/binary, "'">>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).

execute(Connection, Query, []) when is_list(Query); is_binary(Query) ->
	Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary>>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
	
execute(Connection, StmtName, []) when is_atom(StmtName) ->
	prepare_statement(Connection, StmtName),
	StmtNameBin = atom_to_binary(StmtName, utf8),
	Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary>>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
	
execute(Connection, Query, Args) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
	case set_params(Connection, 1, Args, undefined) of
		OK when is_record(OK, ok_packet) ->
			ParamNamesBin = list_to_binary(string:join([[$@, I+48] || I <- lists:seq(1, length(Args))], ", ")),
			Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary, " USING ", ParamNamesBin/binary>>,
			emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
		Error ->
			Error
	end;

execute(Connection, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
	prepare_statement(Connection, StmtName),
	case set_params(Connection, 1, Args, undefined) of
		OK when is_record(OK, ok_packet) ->
			ParamNamesBin = list_to_binary(string:join([[$@ | integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", ")),
			StmtNameBin = atom_to_binary(StmtName, utf8),
			Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary, " USING ", ParamNamesBin/binary>>,
			emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
		Error ->
			Error
	end.
	
prepare(Connection, Name, Statement) ->
	Packet = <<?COM_QUERY, "PREPARE ", (atom_to_binary(Name, utf8))/binary, " FROM '", (iolist_to_binary(Statement))/binary, "'">>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).
	
unprepare(Connection, Name) ->
	Packet = <<?COM_QUERY, "DEALLOCATE PREPARE ", (atom_to_binary(Name, utf8))/binary>>,
	emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).

open_n_connections(PoolId, N) ->
	case emysql_conn_mgr:find_pool(PoolId, emysql_conn_mgr:pools(), []) of
		{Pool, _} ->
			[open_connection(Pool) || _ <- lists:seq(1, N)];
		_ ->
			exit(pool_not_found)
	end.
		
open_connections(Pool) ->
	case (queue:len(Pool#pool.available) + gb_trees:size(Pool#pool.locked)) < Pool#pool.size of
		true ->
			Conn = emysql_conn:open_connection(Pool),
			open_connections(Pool#pool{available = queue:in(Conn, Pool#pool.available)});
		false ->
			Pool
	end.
	
open_connection(#pool{pool_id=PoolId, host=Host, port=Port, user=User, password=Password, database=Database, encoding=Encoding}) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
		{ok, Sock} ->
			gen_tcp:controlling_process(Sock, whereis(emysql_conn_mgr)),
			Greeting = emysql_auth:do_handshake(Sock, User, Password),
			Connection = #connection{
				id = erlang:port_to_list(Sock),
				pool_id = PoolId,
				socket = Sock,
				version = Greeting#greeting.server_version,
				thread_id = Greeting#greeting.thread_id,
				caps = Greeting#greeting.caps,
				language = Greeting#greeting.language
			},
			case emysql_conn:set_database(Connection, Database) of
				OK1 when is_record(OK1, ok_packet) ->
					ok;
				Err1 when is_record(Err1, error_packet) ->
					exit({failed_to_set_database, Err1#error_packet.msg})
			end,
			case emysql_conn:set_encoding(Connection, Encoding) of
				OK2 when is_record(OK2, ok_packet) ->
					ok;
				Err2 when is_record(Err2, error_packet) ->
					exit({failed_to_set_encoding, Err2#error_packet.msg})
			end,
			Connection;
		{error, Reason} ->
			exit({failed_to_connect_to_database, Reason})
	end.
	
reset_connection(Pools, Conn) ->
	%% if a process dies or times out while doing work
	%% the socket must be closed and the connection reset
	%% in the conn_mgr state. Also a new connection needs
	%% to be opened to replace the old one.
	spawn(fun() -> close_connection(Conn) end),
	%% OPEN NEW SOCKET
	case emysql_conn_mgr:find_pool(Conn#connection.pool_id, Pools, []) of
		{Pool, _} ->
			NewConn = open_connection(Pool),
			emysql_conn_mgr:replace_connection(Conn, NewConn);
		undefined ->
			exit(pool_not_found)
	end.

close_connection(Conn) ->
	%% DEALLOCATE PREPARED STATEMENTS
	[(catch unprepare(Conn, Name)) || Name <- emysql_statements:remove(Conn#connection.id)],
	%% CLOSE SOCKET
	gen_tcp:close(Conn#connection.socket),
	ok.
		
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------			
set_params(_, _, [], Result) -> Result;
set_params(_, _, _, Error) when is_record(Error, error_packet) -> Error;
set_params(Connection, Num, [Val|Tail], _) ->
	NumBin = emysql_util:encode(Num, true),
    ValBin = emysql_util:encode(Val, true),
	Packet = <<?COM_QUERY, "SET @", NumBin/binary, "=", ValBin/binary>>,
	Result = emysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0),
	set_params(Connection, Num+1, Tail, Result).

prepare_statement(Connection, StmtName) ->
	case emysql_statements:fetch(StmtName) of
		undefined ->
			exit(statement_has_not_been_prepared);
		{Version, Statement} ->
			case emysql_statements:version(Connection#connection.id, StmtName) of
				Version ->
					ok;
				_ ->
					case prepare(Connection, StmtName, Statement) of
						OK when is_record(OK, ok_packet) ->
							emysql_statements:prepare(Connection#connection.id, StmtName, Version);
						Err when is_record(Err, error_packet) ->
							exit({failed_to_prepare_statement, Err#error_packet.msg})
					end
			end
	end.