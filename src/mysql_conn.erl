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