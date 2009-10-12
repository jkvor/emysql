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
-export([send_and_recv_packet/3, recv_packet/1, package_server_response/2]).

-include("emysql.hrl").

-define(PACKETSIZE, 1460).
-define(ETS_SELECT(TableID), ets:select(TableID,[{{'_','$2'},[],['$2']}])).
		
send_and_recv_packet(Sock, Packet, SeqNum) ->
	case gen_tcp:send(Sock, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>) of
		ok -> ok;
		{error, Reason} ->
			exit({failed_to_send_packet_to_server, Reason})
	end,	
	package_server_response(Sock, recv_packet(Sock)).

recv_packet(Sock) ->
	{PacketLength, SeqNum} = recv_packet_header(Sock),
	Data = recv_packet_body(Sock, PacketLength),
	#packet{size=PacketLength, seq_num=SeqNum, data=Data}.

package_server_response(_Sock, #packet{seq_num = SeqNum, data = <<0:8, Rest/binary>>}) ->
	{AffectedRows, Rest1} = emysql_util:length_coded_binary(Rest),
	{InsertId, Rest2} = emysql_util:length_coded_binary(Rest1),
	<<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2,
	#ok_packet{
		seq_num = SeqNum, 
		affected_rows = AffectedRows, 
		insert_id = InsertId, 
		status = ServerStatus, 
		warning_count = WarningCount, 
		msg = binary_to_list(Msg)
	};

package_server_response(_Sock, #packet{seq_num = SeqNum, data = <<254:8>>}) ->
	#eof_packet{
		seq_num = SeqNum
	};
	
package_server_response(_Sock, #packet{seq_num = SeqNum, data = <<255:8, Rest/binary>>}) ->
	<<Code:16/little, Msg/binary>> = Rest,
	#error_packet{
		seq_num = SeqNum, 
		code = Code, 
		msg = binary_to_list(Msg)
	};

package_server_response(Sock, #packet{seq_num=SeqNum, data=Data}) ->
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
	#result_packet{
		seq_num = SeqNum2, 
		field_list = FieldList, 
		rows = Rows, 
		extra = Extra
	}.

recv_packet_header(Sock) ->
	case gen_tcp:recv(Sock, 4, ?TIMEOUT) of
		{ok, <<PacketLength:24/little-integer, SeqNum:8/integer>>} ->
			{PacketLength, SeqNum};
		{ok, Bin} when is_binary(Bin) ->
			exit({bad_packet_header_data, Bin});
		{error, Reason} ->
			exit({failed_to_recv_packet_header, Reason})
	end.
	
recv_packet_body(Sock, PacketLength) ->
	Tid = ets:new(emysql_buffer, [ordered_set, private]),
	Bin = recv_packet_body(Sock, PacketLength, Tid, 0),
	ets:delete(Tid),
	Bin.

recv_packet_body(Sock, PacketLength, Tid, Key) ->
	if
		PacketLength > ?PACKETSIZE ->
			case gen_tcp:recv(Sock, ?PACKETSIZE, ?TIMEOUT) of
				{ok, Bin} ->
					ets:insert(Tid, {Key, Bin}),
					recv_packet_body(Sock, PacketLength - ?PACKETSIZE, Tid, Key+1);
				{error, Reason1} ->
					exit({failed_to_recv_packet_body, Reason1})
			end;
		true ->
			case gen_tcp:recv(Sock, PacketLength, ?TIMEOUT) of
				{ok, Bin} ->
					if
						Key == 0 -> Bin;
						true -> iolist_to_binary(?ETS_SELECT(Tid) ++ Bin)
					end;
				{error, Reason1} ->
					exit({failed_to_recv_packet_body, Reason1})
			end			
	end.
		
recv_field_list(Sock, SeqNum) ->
	Tid = ets:new(emysql_field_list, [ordered_set, private]),
	Res = recv_field_list(Sock, SeqNum, Tid, 0),
	ets:delete(Tid),
	Res.
	
recv_field_list(Sock, _SeqNum, Tid, Key) ->
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<254, _/binary>>} -> 
			{SeqNum1, ?ETS_SELECT(Tid)};
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
			ets:insert(Tid, {Key, Field}),
			recv_field_list(Sock, SeqNum1, Tid, Key+1)
	end.
	
recv_row_data(Sock, FieldList, SeqNum) ->
	Tid = ets:new(emysql_row_data, [ordered_set, private]),
	Res = recv_row_data(Sock, FieldList, SeqNum, Tid, 0),
	ets:delete(Tid),
	Res.
	
recv_row_data(Sock, FieldList, _SeqNum, Tid, Key) ->
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<254, _/binary>>} -> 
			{SeqNum1, ?ETS_SELECT(Tid)};
		#packet{seq_num = SeqNum1, data = RowData} ->
			Row = decode_row_data(RowData, FieldList, []),
			ets:insert(Tid, {Key, Row}),
			recv_row_data(Sock, FieldList, SeqNum1, Tid, Key+1)
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
	list_to_integer(binary_to_list(Data));
	
type_cast_row_data(Data, #field{type=Type, decimals=_Decimals}) 
	when Type == ?FIELD_TYPE_DECIMAL;
		 Type == ?FIELD_TYPE_NEWDECIMAL;
		 Type == ?FIELD_TYPE_FLOAT;
		 Type == ?FIELD_TYPE_DOUBLE ->
	{ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Data)) of
		{error, _} -> io_lib:fread("~d", binary_to_list(Data));
		Res -> Res
	end,
	Num;
	%try_formats(["~f", "~d"], binary_to_list(Data));
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_DATE ->
	case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of
		{ok, [Year, Month, Day], _} ->
			{date, {Year, Month, Day}};
		{error, _} ->
			binary_to_list(Data);
		_ ->
			exit({error, bad_date})
	end;
	
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_TIME ->
	case io_lib:fread("~d:~d:~d", binary_to_list(Data)) of
		{ok, [Hour, Minute, Second], _} ->
			{time, {Hour, Minute, Second}};
		{error, _} ->
			binary_to_list(Data);
		_ ->
			exit({error, bad_time})
	end;
		
type_cast_row_data(Data, #field{type=Type}) 
	when Type == ?FIELD_TYPE_TIMESTAMP;
		 Type == ?FIELD_TYPE_DATETIME ->
	case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Data)) of
		{ok, [Year, Month, Day, Hour, Minute, Second], _} ->
			{datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
		{error, _} ->
			binary_to_list(Data);
		_ ->
			exit({error, datetime})
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