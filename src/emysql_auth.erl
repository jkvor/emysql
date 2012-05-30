%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Henning Diedrich <hd2010@eonblast.com>
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
-module(emysql_auth).
-export([do_handshake/3]).
-compile(export_all).

-include("emysql.hrl").

do_handshake(Sock, User, Password) ->
	%-% io:format("~p handshake: recv_greeting~n", [self()]),
	Greeting = recv_greeting(Sock),
	%-% io:format("~p handshake: auth~n", [self()]),
	case auth(Sock, Greeting#greeting.seq_num+1, User, Password,
		Greeting#greeting.salt1, Greeting#greeting.salt2, Greeting#greeting.plugin) of
		OK when is_record(OK, ok_packet) ->
			%-% io:format("~p handshake: ok~n", [self()]),
			ok;
		Err when is_record(Err, error_packet) ->
			%-% io:format("~p handshake: FAIL ~p -> EXIT ~n~n", [self(), Err]),
			exit({failed_to_authenticate, Err});
		Other ->
			%-% io:format("~p handshake: UNEXPECTED ~p -> EXIT ~n~n", [self(), Other]),
			exit({unexpected_packet, Other})
	end,
	Greeting.

recv_greeting(Sock) ->
	%-% io:format("~p recv_greeting~n", [self()]),
	GreetingPacket = emysql_tcp:recv_packet(Sock),
	%-% io:format("~p recv_greeting ... received ...~n", [self()]),
	case GreetingPacket#packet.data of
		<<255, _/binary>> ->
			% io:format("error: ", []), 
			{#error_packet{
				code = Code,
				msg = Msg
			},_} = emysql_tcp:response(Sock, GreetingPacket),
			% io:format("exit: ~p~n-------------~p~n", [Code, Msg]), 
			exit({Code, Msg});
		<<ProtocolVersion:8/integer, Rest1/binary>> ->
			% io:format("prl v: ~p~n-------------~p~n", [ProtocolVersion, Rest1]), 
			{ServerVersion, Rest2} = emysql_util:asciz(Rest1),
			% io:format("srv v: ~p~n-------------~p~n", [ServerVersion, Rest2]),
			<<ThreadID:32/little, Rest3/binary>> = Rest2,
			% io:format("tread id: ~p~n-------------~p~n", [ThreadID, Rest3]),
			{Salt, Rest4} = emysql_util:asciz(Rest3),
			% io:format("salt: ~p~n-------------~p~n", [Salt, Rest4]), 
			<<ServerCaps:16/little, Rest5/binary>> = Rest4,
			% io:format("caps: ~p~n-------------~p~n", [ServerCaps, Rest5]),
			<<ServerLanguage:8/little,
				ServerStatus:16/little,
				ServerCapsHigh:16/little,
				ScrambleLength:8/little,
				_:10/binary-unit:8,
				Rest6/binary>> = Rest5,
			% io:format("lang: ~p, status: ~p, caps hi: ~p, salt len: ~p~n-------------~p ~n", [ServerLanguage, ServerStatus, ServerCapsHigh, ScrambleLength, Rest6]),
			Salt2Length = case ScrambleLength of 0 -> 13; _-> ScrambleLength - 8 end,
			<<Salt2Bin:Salt2Length/binary-unit:8, Plugin/binary>> = Rest6,
			{Salt2, <<>>} = emysql_util:asciz(Salt2Bin),
			% io:format("salt 2: ~p~n", [Salt2]),
			% io:format("plugin: ~p~n", [Plugin]),
			#greeting{
				protocol_version = ProtocolVersion,
				server_version = ServerVersion,
				thread_id = ThreadID,
				salt1 = Salt,
				salt2 = Salt2,
				caps = ServerCaps,
				caps_high = ServerCapsHigh,
				language = ServerLanguage,
				status = ServerStatus,
				seq_num = GreetingPacket#packet.seq_num,
				plugin = Plugin
			};
		What ->
			%-% io:format("~p recv_greeting FAILED: ~p~n", [self(), What]),
			exit({greeting_failed, What})
	end.

parse_server_version(Version) ->
	[A,B,C] = string:tokens(Version, "."),
	{list_to_integer(A), list_to_integer(B), list_to_integer(C)}.

auth(Sock, SeqNum, User, Password, Salt1, Salt2, Plugin) ->
	ScrambleBuff = if
		is_list(Password) orelse is_binary(Password) ->
			case Plugin of
				?MYSQL_OLD_PASSWORD ->
					password_old(Password, Salt1 ++ Salt2); % untested
				_ ->
					password_new(Password, Salt1 ++ Salt2)
			end;
		true ->
			<<>>
	end,
	DBCaps = 0,
	DatabaseB = <<>>,
	Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
		?CLIENT_MULTI_STATEMENTS bor ?CLIENT_MULTI_RESULTS bor
		?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps,
	Maxsize = ?MAXPACKETBYTES,
	UserB = unicode:characters_to_binary(User),
	PasswordL = size(ScrambleBuff),
	Packet = <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8, UserB/binary, 0:8, PasswordL:8, ScrambleBuff/binary, DatabaseB/binary>>,
	case emysql_tcp:send_and_recv_packet(Sock, Packet, SeqNum) of
		#eof_packet{seq_num = SeqNum1} ->
			AuthOld = password_old(Password, Salt1),
			emysql_tcp:send_and_recv_packet(Sock, <<AuthOld/binary, 0:8>>, SeqNum1+1);
		Result ->
			Result
	end.

password_new([], _Salt) ->
	<<>>;
password_new(Password, Salt) ->
	Stage1 = crypto:sha(Password),
	Stage2 = crypto:sha(Stage1),
	Res = crypto:sha_final(
		crypto:sha_update(
			crypto:sha_update(crypto:sha_init(), Salt),
			Stage2
		)
	),
	emysql_util:bxor_binary(Res, Stage1).

password_old(Password, Salt) ->
	{P1, P2} = emysql_util:hash(Password),
	{S1, S2} = emysql_util:hash(Salt),
	Seed1 = P1 bxor S1,
	Seed2 = P2 bxor S2,
	List = emysql_util:rnd(9, Seed1, Seed2),
	{L, [Extra]} = lists:split(8, List),
	list_to_binary(lists:map(fun (E) -> E bxor (Extra - 64) end, L)).
	% note, this operates on byte integer lists, never strings, much less unicode
