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
-module(emysql_tcp).
-export([send_and_recv_packet/3, recv_packet/1, timeout/0, packet_size/0, package_server_response/2]).

-include("emysql.hrl").
		
send_and_recv_packet(Sock, Packet, SeqNum) ->
	case gen_tcp:send(Sock, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>) of
		ok -> ok;
		{error, Reason} ->
			exit({failed_to_send_packet_to_server, Reason})
	end,
	package_server_response(Sock, recv_packet(Sock)).
	
recv_packet(Sock) ->
	{PacketLength, SeqNum} = recv_packet_header(Sock),
	Data = recv_packet_body(Sock, PacketLength, <<>>),
	#packet{size=PacketLength, seq_num=SeqNum, data=Data}.
	
package_server_response(_Sock, #packet{seq_num = SeqNum, data = <<0:8, Rest/binary>>}) ->
	%error_logger:debug_msg("recv'd ok packet: ~p~n", [Rest]),
	{AffectedRows, Rest1} = emysql_util:length_coded_binary(Rest),
	{InsertId, Rest2} = emysql_util:length_coded_binary(Rest1),
	<<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
	ok_packet:new(SeqNum, AffectedRows, InsertId, ServerStatus, WarningCount, binary_to_list(Msg));
	
package_server_response(_Sock, #packet{seq_num = SeqNum, data = <<255:8, Rest/binary>>}) ->
	%error_logger:debug_msg("recv'd error packet: ~p~n", [Rest]),
	<<Code:16/little, Msg/binary>> = Rest,
	error_packet:new(SeqNum, Code, binary_to_list(Msg));
	
package_server_response(Sock, #packet{seq_num=SeqNum, data=Data}) ->
	%error_logger:debug_msg("recv'd result packet: ~p~n", [Data]),
	{FieldCount, Rest1} = emysql_util:length_coded_binary(Data),
	{Extra, _} = emysql_util:length_coded_binary(Rest1),
	{SeqNum1, FieldList} = recv_field_list(Sock, SeqNum+1),
	if
		length(FieldList) =/= FieldCount ->
			exit(query_returned_incorrect_field_count);
		true ->
			ok
	end,
	{SeqNum2, Rows} = recv_row_data(Sock, FieldList, SeqNum1+1),
	result_packet:new(SeqNum2, FieldList, Rows, Extra).
	
recv_field_list(Sock, SeqNum) ->
	recv_field_list(Sock, SeqNum, []).
	
recv_field_list(Sock, _SeqNum, Acc) ->
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<254, _/binary>>} -> 
			{SeqNum1, lists:reverse(Acc)};
		#packet{seq_num = SeqNum1, data = Data} ->
			{Catalog, Rest2} = emysql_util:length_coded_string(Data),
			{Db, Rest3} = emysql_util:length_coded_string(Rest2),
			{Table, Rest4} = emysql_util:length_coded_string(Rest3),
			{OrgTable, Rest5} = emysql_util:length_coded_string(Rest4),
			{Name, Rest6} = emysql_util:length_coded_string(Rest5),
			{OrgName, Rest7} = emysql_util:length_coded_string(Rest6),
			<<_:1/binary, CharSetNr:16/little, Length:32/little, Rest8/binary>> = Rest7,
			<<Type:8/little, Flags:16/little, Decimals:8/little, _:2/binary, Rest9/binary>> = Rest8,
			{Default, _} = emysql_util:length_coded_binary(Rest9),
			Field = #field{
				seq_num = SeqNum1,
				catalog = Catalog,
				db = Db,
				table = Table,
				org_table = OrgTable,
				name = Name,
				org_name = OrgName,
				type = Type,
				default = Default,
				charset_nr = CharSetNr,
				length = Length,
				flags = Flags,
				decimals = Decimals
			},
			recv_field_list(Sock, SeqNum1, [Field|Acc])
	end.
	
recv_row_data(Sock, FieldList, SeqNum) ->
	recv_row_data(Sock, FieldList, SeqNum, []).
	
recv_row_data(Sock, FieldList, _SeqNum, Acc) ->
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<254, _/binary>>} -> 
			{SeqNum1, lists:reverse(Acc)};
		#packet{seq_num = SeqNum1, data = RowData} ->
			Row = decode_row_data(RowData, FieldList, []),
			recv_row_data(Sock, FieldList, SeqNum1, [Row|Acc])
	end.

decode_row_data(<<>>, [], Acc) -> 
	lists:reverse(Acc);	
decode_row_data(Bin, [Field|Rest], Acc) ->
	{Data, Tail} = emysql_util:length_coded_string(Bin),
	decode_row_data(Tail, Rest, [type_cast_row_data(Data, Field)|Acc]).

type_cast_row_data(undefined, _) ->
	undefined;
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_VARCHAR;
		 Type == ?FIELD_TYPE_TINY_BLOB;
		 Type == ?FIELD_TYPE_MEDIUM_BLOB;
		 Type == ?FIELD_TYPE_LONG_BLOB;
		 Type == ?FIELD_TYPE_BLOB;
		 Type == ?FIELD_TYPE_VAR_STRING;
		 Type == ?FIELD_TYPE_STRING ->
	Data;
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_TINY;
		 Type == ?FIELD_TYPE_SHORT;
		 Type == ?FIELD_TYPE_LONG;
		 Type == ?FIELD_TYPE_LONGLONG;
		 Type == ?FIELD_TYPE_INT24;
		 Type == ?FIELD_TYPE_YEAR ->
	List = binary_to_list(Data),
	case string:to_integer(List) of
		{error,no_integer} -> List;
		{Int, _} -> Int
	end;
	
type_cast_row_data(Data, #field{type=Type, decimals=Decimals}) 
	when Type == ?FIELD_TYPE_DECIMAL;
		 Type == ?FIELD_TYPE_NEWDECIMAL ->
	try_formats([lists:concat([[$~], Decimals, [$d]]), "~d"], binary_to_list(Data));

type_cast_row_data(Data, #field{type=Type, decimals=Decimals}) 
	when Type == ?FIELD_TYPE_FLOAT;
		 Type == ?FIELD_TYPE_DOUBLE ->
	try_formats([lists:concat([[$~], Decimals, [$f]]), "~f", "~d"], binary_to_list(Data));
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_DATE ->
	case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of
		{ok, [Year, Month, Day], _} ->
			{date, {Year, Month, Day}};
		{error, _} ->
			binary_to_list(Data);
		Other ->
			io:format("[~p:~b] unexpected ~p~n", [?MODULE, ?LINE, Other])
	end;
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_TIME ->
	case io_lib:fread("~d:~d:~d", binary_to_list(Data)) of
		{ok, [Hour, Minute, Second], _} ->
			{time, {Hour, Minute, Second}};
		{error, _} ->
			binary_to_list(Data);
		Other ->
			io:format("[~p:~b] unexpected ~p~n", [?MODULE, ?LINE, Other])
	end;
		
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_TIMESTAMP;
		 Type == ?FIELD_TYPE_DATETIME ->
	case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Data)) of
		{ok, [Year, Month, Day, Hour, Minute, Second], _} ->
			{datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
		{error, _} ->
			binary_to_list(Data);
		Other ->
			io:format("[~p:~b] unexpected ~p~n", [?MODULE, ?LINE, Other])
	end;
	
type_cast_row_data(Data, #field{type=Type})
	when Type == ?FIELD_TYPE_BIT ->
	case Data of
		<<1>> -> 1;
		<<0>> -> 0
	end;
			
% ?FIELD_TYPE_NEWDATE
% ?FIELD_TYPE_ENUM
% ?FIELD_TYPE_SET
% ?FIELD_TYPE_GEOMETRY
type_cast_row_data(Data, _) -> Data.

try_formats([], Data) -> Data;
try_formats([F|Tail], Data) ->
	case io_lib:fread(F, Data) of
		{error, _} -> try_formats(Tail, Data);
		{ok, [Val], _} -> Val
	end.

recv_packet_header(Sock) ->
	case gen_tcp:recv(Sock, 4, timeout()) of
		{ok, <<PacketLength:24/little-integer, SeqNum:8/integer>>} ->
			{PacketLength, SeqNum};
		{ok, Bin} when is_binary(Bin) ->
			exit({bad_packet_header_data, Bin});
		{error, Reason} ->
			exit({failed_to_recv_packet_header, Reason})
	end.
	
recv_packet_body(Sock, PacketLength, Acc) ->
	case packet_size() of
		%% the packet is too large to get in one request
		MaxSize when PacketLength > MaxSize ->
			case gen_tcp:recv(Sock, MaxSize, timeout()) of
				{ok, Bin} ->
					recv_packet_body(Sock, PacketLength - MaxSize, <<Acc/binary, Bin/binary>>);
				{error, Reason1} ->
					exit({failed_to_recv_packet_body, Reason1})
			end;
		%% fetch the remainder of the packet
		_ ->
			case gen_tcp:recv(Sock, PacketLength, timeout()) of
				{ok, Bin} ->
					<<Acc/binary, Bin/binary>>;
				{error, Reason2} ->
					exit({failed_to_recv_packet_body, Reason2})
			end
	end.

timeout() ->
	case application:get_env(emysql, default_timeout) of
		undefined -> ?TIMEOUT;
		{ok, Timeout} -> Timeout
	end.

packet_size() ->
	case application:get_env(emysql, max_packet_bytes) of
		undefined -> ?MAXPACKETBYTES;
		{ok, Size} -> Size
	end.
	
	