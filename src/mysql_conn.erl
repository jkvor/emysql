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
-module(mysql_conn).
-export([set_database/2, set_encoding/2]).
-export([execute/3, prepare/3, unprepare/2]).
-export([open_connection/1, reset_connection/2, close_connection/1]).
-export([open_n_connections/2]).

-include("emysql.hrl").

set_database(_, undefined) -> ok;
set_database(Connection, Database) ->
	Packet = <<?COM_QUERY, "use ", (iolist_to_binary(Database))/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).
	
set_encoding(Connection, Encoding) ->
	Packet = <<?COM_QUERY, "set names '", (erlang:atom_to_binary(Encoding, utf8))/binary, "'">>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).

execute(Connection, Query, []) when is_list(Query); is_binary(Query) ->
	Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
	
execute(Connection, StmtName, []) when is_atom(StmtName) ->
	prepare_statement(Connection, StmtName),
	StmtNameBin = atom_to_binary(StmtName, utf8),
	Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
	
execute(Connection, Query, Args) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
	case set_params(Connection, 1, Args, undefined) of
		OK when is_record(OK, mysql_ok_packet) ->
			ParamNamesBin = list_to_binary(string:join([[$@, I+48] || I <- lists:seq(1, length(Args))], ", ")),
			Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary, " USING ", ParamNamesBin/binary>>,
			mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
		Error ->
			Error
	end;

execute(Connection, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
	prepare_statement(Connection, StmtName),
	case set_params(Connection, 1, Args, undefined) of
		OK when is_record(OK, mysql_ok_packet) ->
			ParamNamesBin = list_to_binary(string:join([[$@ | integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", ")),
			StmtNameBin = atom_to_binary(StmtName, utf8),
			Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary, " USING ", ParamNamesBin/binary>>,
			mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0);
		Error ->
			Error
	end.
	
prepare(Connection, Name, Statement) ->
	Packet = <<?COM_QUERY, "PREPARE ", (atom_to_binary(Name, utf8))/binary, " FROM '", (iolist_to_binary(Statement))/binary, "'">>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).
	
unprepare(Connection, Name) ->
	Packet = <<?COM_QUERY, "DEALLOCATE PREPARE ", (atom_to_binary(Name, utf8))/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).

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
	
reset_connection(Pools, Conn) ->
	%% if a process dies or times out while doing work
	%% the socket must be closed and the connection reset
	%% in the conn_mgr state. Also a new connection needs
	%% to be opened to replace the old one.
	close_connection(Conn),
	%% OPEN NEW SOCKET
	case mysql_conn_mgr:find_pool(Conn#connection.pool_id, Pools, []) of
		{Pool, _} ->
			NewConn = open_connection(Pool),
			mysql_conn_mgr:replace_connection(Conn, NewConn);
		undefined ->
			exit(pool_not_found)
	end.

close_connection(Conn) ->
	%% DEALLOCATE PREPARED STATEMENTS
	[(catch unprepare(Conn, Name)) || Name <- mysql_statements:remove(Conn#connection.id)],
	%% CLOSE SOCKET
	gen_tcp:close(Conn#connection.socket),
	ok.
	
open_n_connections(PoolId, N) ->
	case mysql_conn_mgr:find_pool(PoolId, mysql_conn_mgr:pools(), []) of
		{Pool, _} ->
			[open_connection(Pool) || _ <- lists:seq(1, N)];
		_ ->
			exit(pool_not_found)
	end.
	
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------			
set_params(_, _, [], Result) -> Result;
set_params(_, _, _, Error) when is_record(Error, mysql_error_packet) -> Error;
set_params(Connection, Num, [Val|Tail], _) ->
	NumBin = mysql_util:encode(Num, true),
    ValBin = mysql_util:encode(Val, true),
	Packet = <<?COM_QUERY, "SET @", NumBin/binary, "=", ValBin/binary>>,
	Result = mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0),
	set_params(Connection, Num+1, Tail, Result).

prepare_statement(Connection, StmtName) ->
	case mysql_statements:fetch(StmtName) of
		undefined ->
			exit(statement_has_not_been_prepared);
		{Version, Statement} ->
			case mysql_statements:version(Connection#connection.id, StmtName) of
				Version ->
					ok;
				_ ->
					prepare(Connection, StmtName, Statement),
					mysql_statements:prepare(Connection#connection.id, StmtName, Version)
			end
	end.