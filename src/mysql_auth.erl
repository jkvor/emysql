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
-module(mysql_auth).
-export([do_handshake/3]).
-compile(export_all).

-include("emysql.hrl").

do_handshake(Sock, User, Password) ->
	Greeting = recv_greeting(Sock),
	case auth(Sock, Greeting#greeting.seq_num+1, User, Password, Greeting#greeting.salt1, Greeting#greeting.salt2) of
		OK when is_record(OK, mysql_ok_packet) ->
			ok;
		Err when is_record(Err, mysql_error_packet) ->
			exit({failed_to_authenticate, Err})
	end,
	Greeting.

recv_greeting(Sock) ->
	GreetingPacket = mysql_tcp:recv_packet(Sock),
	case GreetingPacket#packet.data of
		<<ProtocolVersion:8/integer, Rest1/binary>> ->
			{ServerVersion, Rest2} = mysql_util:asciz(Rest1),
		    <<TreadID:32/little, Rest3/binary>> = Rest2,
		    {Salt, Rest4} = mysql_util:asciz(Rest3),
		    <<ServerCaps:16/little, Rest5/binary>> = Rest4,
		    <<ServerLanguage:8/little, ServerStatus:16/little, _:13/binary-unit:8, Rest6/binary>> = Rest5,
		    {Salt2, <<>>} = mysql_util:asciz(Rest6),
			#greeting{
				protocol_version = ProtocolVersion,
				server_version = parse_server_version(ServerVersion),
				thread_id = TreadID,
				salt1 = Salt,
				salt2 = Salt2,
				caps = ServerCaps,
				language = ServerLanguage,
				status = ServerStatus,
				seq_num = GreetingPacket#packet.seq_num
			};
		_ ->
			exit(poorly_formatted_handshake_packet_protocol_version)
	end.

parse_server_version(Version) ->
	[A,B,C] = string:tokens(Version, "."),
	{list_to_integer(A), list_to_integer(B), list_to_integer(C)}.

auth(Sock, SeqNum, User, Password, Salt1, Salt2) ->
	ScrambleBuff = if
		is_list(Password) orelse is_binary(Password) ->
			password(Password, Salt1 ++ Salt2);
		true ->
			<<>>
	end,
	DBCaps = 0,
	DatabaseB = <<>>,
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
        ?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS bor 
        ?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps,
    Maxsize = mysql_tcp:packet_size(),
    UserB = list_to_binary(User),
    PasswordL = size(ScrambleBuff),
    Packet = <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8, UserB/binary, 0:8, PasswordL:8, ScrambleBuff/binary, DatabaseB/binary>>,
	mysql_tcp:send_and_recv_packet(Sock, Packet, SeqNum).
	
password(Password, Salt) ->
    Stage1 = crypto:sha(Password),
    Stage2 = crypto:sha(Stage1),
    Res = crypto:sha_final(
        crypto:sha_update(
            crypto:sha_update(crypto:sha_init(), Salt),
            Stage2
        )
    ),
    mysql_util:bxor_binary(Res, Stage1).