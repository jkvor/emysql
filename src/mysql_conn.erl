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
-export([execute/2]).

-include("emysql.hrl").

set_database(_, undefined) -> ok;
set_database(Connection, Database) ->
	Packet = <<?COM_QUERY, "use ", (iolist_to_binary(Database))/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).
	
set_encoding(Connection, Encoding) ->
	Packet = <<?COM_QUERY, "set names '", (erlang:atom_to_binary(Encoding, utf8))/binary, "'">>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).

execute(Connection, Query) ->
	Packet = <<?COM_QUERY, (iolist_to_binary(Query))/binary>>,
	mysql_tcp:send_and_recv_packet(Connection#connection.socket, Packet, 0).