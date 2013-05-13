%% Copyright (c) 2009-2011
%% Bill Warnecke <bill@rupture.com>,
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>,
%% Henning Diedrich <hd2010@eonblast.com>,
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

-module(emysql_tcp).
-export([send_and_recv_packet/3, recv_packet/1, response/2]).

-include("emysql.hrl").

-define(PACKETSIZE, 1460).

send_and_recv_packet(Sock, Packet, SeqNum) ->
    %-% io:format("~nsend_and_receive_packet: SEND SeqNum: ~p, Binary: ~p~n", [SeqNum, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>]),
    %-% io:format("~p send_and_recv_packet: send~n", [self()]),
    case gen_tcp:send(Sock, <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>) of
        ok ->
            %-% io:format("~p send_and_recv_packet: send ok~n", [self()]),
            ok;
        {error, Reason} ->
            %-% io:format("~p send_and_recv_packet: ERROR ~p -> EXIT~n", [self(), Reason]),
            exit({failed_to_send_packet_to_server, Reason})
    end,
    %-% io:format("~p send_and_recv_packet: resonse_list~n", [self()]),
    case response_list(Sock, ?SERVER_MORE_RESULTS_EXIST) of
        % This is a bit murky. It's compatible with former Emysql versions
        % but sometimes returns a list, e.g. for stored procedures,
        % since an extra OK package is sent at the end of their results.
        [Record | []] ->
            %-% io:format("~p send_and_recv_packet: record~n", [self()]),
            Record;
        List ->
            %-% io:format("~p send_and_recv_packet: list~n", [self()]),
            List
    end.

response_list(_, 0) -> [];

response_list(Sock, ?SERVER_MORE_RESULTS_EXIST) ->

    {Response, ServerStatus} = response(Sock, recv_packet(Sock)),
    [ Response | response_list(Sock, ServerStatus band ?SERVER_MORE_RESULTS_EXIST)].

recv_packet(Sock) ->
    %-% io:format("~p recv_packet~n", [self()]),
    %-% io:format("~p recv_packet: recv_packet_header~n", [self()]),
    {PacketLength, SeqNum} = recv_packet_header(Sock),
    %-% io:format("~p recv_packet: recv_packet_body~n", [self()]),
    Data = recv_packet_body(Sock, PacketLength),
    %-% io:format("~nrecv_packet: len: ~p, data: ~p~n", [PacketLength, Data]),
    #packet{size=PacketLength, seq_num=SeqNum, data=Data}.

% OK response: first byte 0. See -1-
response(_Sock, #packet{seq_num = SeqNum, data = <<0:8, Rest/binary>>}=_Packet) ->
    %-% io:format("~nresponse (OK): ~p~n", [_Packet]),
    {AffectedRows, Rest1} = emysql_util:length_coded_binary(Rest),
    {InsertId, Rest2} = emysql_util:length_coded_binary(Rest1),
    <<ServerStatus:16/little, WarningCount:16/little, Msg/binary>> = Rest2, % (*)!
    %-% io:format("- warnings: ~p~n", [WarningCount]),
    %-% io:format("- server status: ~p~n", [emysql_conn:hstate(ServerStatus)]),
    { #ok_packet{
        seq_num = SeqNum,
        affected_rows = AffectedRows,
        insert_id = InsertId,
        status = ServerStatus,
        warning_count = WarningCount,
        msg = unicode:characters_to_list(Msg) },
      ServerStatus };

% EOF: MySQL format <= 4.0, single byte. See -2-
response(_Sock, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8>>}=_Packet) ->
    %-% io:format("~nresponse (EOF v 4.0): ~p~n", [_Packet]),
    { #eof_packet{
        seq_num = SeqNum },
      ?SERVER_NO_STATUS };

% EOF: MySQL format >= 4.1, with warnings and status. See -2-
response(_Sock, #packet{seq_num = SeqNum, data = <<?RESP_EOF:8, WarningCount:16/little, ServerStatus:16/little>>}=_Packet) -> % (*)!
    %-% io:format("~nresponse (EOF v 4.1), Warn Count: ~p, Status ~p, Raw: ~p~n", [WarningCount, ServerStatus, _Packet]),
    %-% io:format("- warnings: ~p~n", [WarningCount]),
    %-% io:format("- server status: ~p~n", [emysql_conn:hstate(ServerStatus)]),
    { #eof_packet{
        seq_num = SeqNum,
        status = ServerStatus,
        warning_count = WarningCount },
      ServerStatus };

% ERROR response: MySQL format >= 4.1. See -3-
response(_Sock, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, "#", SQLState:5/binary-unit:8, Msg/binary>>}=_Packet) ->
    %-% io:format("~nresponse (Response is ERROR): SeqNum: ~p, Packet: ~p~n", [SeqNum, _Packet]),
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = SQLState,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS };

% ERROR response: MySQL format <= 4.0. See -3-
response(_Sock, #packet{seq_num = SeqNum, data = <<255:8, ErrNo:16/little, Msg/binary>>}=_Packet) ->
    %-% io:format("~nresponse (Response is ERROR): SeqNum: ~p, Packet: ~p~n", [SeqNum, _Packet]),
    { #error_packet{
        seq_num = SeqNum,
        code = ErrNo,
        status = 0,
        msg = binary_to_list(Msg) }, % todo: test and possibly conversion to UTF-8
     ?SERVER_NO_STATUS };

% DATA response.
response(Sock, #packet{seq_num = SeqNum, data = Data}=_Packet) ->
    %-% io:format("~nresponse (DATA): ~p~n", [_Packet]),
    {FieldCount, Rest1} = emysql_util:length_coded_binary(Data),
    {Extra, _} = emysql_util:length_coded_binary(Rest1),
    {SeqNum1, FieldList} = recv_field_list(Sock, SeqNum+1),
    if
        length(FieldList) =/= FieldCount ->
            exit(query_returned_incorrect_field_count);
        true ->
            ok
    end,
    {SeqNum2, Rows, ServerStatus} = recv_row_data(Sock, FieldList, SeqNum1+1),
    { #result_packet{
        seq_num = SeqNum2,
        field_list = FieldList,
        rows = Rows,
        extra = Extra },
      ServerStatus }.

recv_packet_header(Sock) ->
    Timeout = emysql_app:default_timeout(),
    %-% io:format("~p recv_packet_header~n", [self()]),
    %-% io:format("~p recv_packet_header: recv~n", [self()]),
    case gen_tcp:recv(Sock, 4, Timeout) of
        {ok, <<PacketLength:24/little-integer, SeqNum:8/integer>>} ->
            %-% io:format("~p recv_packet_header: ok~n", [self()]),
            {PacketLength, SeqNum};
        {ok, Bin} when is_binary(Bin) ->
            %-% io:format("~p recv_packet_header: ERROR: exit w/bad_packet_header_data~n", [self()]),
            exit({bad_packet_header_data, Bin});
        {error, Reason} ->
            %-% io:format("~p recv_packet_header: ERROR: exit w/~p~n", [self(), Reason]),
            exit({failed_to_recv_packet_header, Reason})
    end.

% This was used to approach a solution for proper handling of SERVER_MORE_RESULTS_EXIST
%
% recv_packet_header_if_present(Sock) ->
%   case gen_tcp:recv(Sock, 4, 0) of
%       {ok, <<PacketLength:24/little-integer, SeqNum:8/integer>>} ->
%           {PacketLength, SeqNum};
%       {ok, Bin} when is_binary(Bin) ->
%           exit({bad_packet_header_data, Bin});
%       {error, timeout} ->
%           none;
%       {error, Reason} ->
%           exit({failed_to_recv_packet_header, Reason})
%   end.

recv_packet_body(Sock, PacketLength) ->
    recv_packet_body(Sock, PacketLength, []).

recv_packet_body(Sock, PacketLength, Acc) ->
    Timeout = emysql_app:default_timeout(),
    if
        PacketLength > ?PACKETSIZE->
            case gen_tcp:recv(Sock, ?PACKETSIZE, Timeout) of
                {ok, Bin} ->
                    recv_packet_body(Sock, PacketLength - ?PACKETSIZE, [Bin|Acc]);
                {error, Reason1} ->
                    exit({failed_to_recv_packet_body, Reason1})
            end;
        true ->
            case gen_tcp:recv(Sock, PacketLength, Timeout) of
                {ok, Bin} ->
                    iolist_to_binary(lists:reverse([Bin|Acc]));
                {error, Reason1} ->
                    exit({failed_to_recv_packet_body, Reason1})
            end
    end.

recv_field_list(Sock, SeqNum) ->
    recv_field_list(Sock, SeqNum, []).

recv_field_list(Sock, _SeqNum, Acc) ->
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _WarningCount:16/little, _ServerStatus:16/little>>} -> % (*)!
			%-% io:format("- eof: ~p~n", [emysql_conn:hstate(_ServerStatus)]),
                        {SeqNum1, lists:reverse(Acc)};
		#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _/binary>>} ->
			%-% io:format("- eof~n", []),
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
	%-% io:format("~nreceive row ~p: ", [Key]),
	case recv_packet(Sock) of
		#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _WarningCount:16/little, ServerStatus:16/little>>} ->
			%-% io:format("- eof: ~p~n", [emysql_conn:hstate(ServerStatus)]),
                        {SeqNum1, lists:reverse(Acc), ServerStatus};
		#packet{seq_num = SeqNum1, data = <<?RESP_EOF, _/binary>>} ->
			%-% io:format("- eof.~n", []),
                        {SeqNum1, lists:reverse(Acc), ?SERVER_NO_STATUS};
		#packet{seq_num = SeqNum1, data = RowData} ->
			%-% io:format("Seq: ~p raw: ~p~n", [SeqNum1, RowData]),
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
    list_to_integer(binary_to_list(Data));  % note: should not need conversion

type_cast_row_data(Data, #field{type=Type, decimals=_Decimals})
    when Type == ?FIELD_TYPE_DECIMAL;
        Type == ?FIELD_TYPE_NEWDECIMAL;
        Type == ?FIELD_TYPE_FLOAT;
        Type == ?FIELD_TYPE_DOUBLE ->
    {ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Data)) of
                                           % note: does not need conversion
        {error, _} ->
          case io_lib:fread("~d", binary_to_list(Data)) of  % note: does not need conversion
            {ok, [_], []} = Res ->
              Res;
            {ok, [X], E} ->
              io_lib:fread("~f", lists:flatten(io_lib:format("~w~s~s" ,[X,".0",E])))
          end
        ;
        Res ->
          Res
    end,
    Num;
    %try_formats(["~f", "~d"], binary_to_list(Data));

type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_DATE ->
    case io_lib:fread("~d-~d-~d", binary_to_list(Data)) of  % note: does not need conversion
        {ok, [Year, Month, Day], _} ->
            {date, {Year, Month, Day}};
        {error, _} ->
            binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, bad_date})
    end;

type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_TIME ->
    case io_lib:fread("~d:~d:~d", binary_to_list(Data)) of  % note: does not need conversion
        {ok, [Hour, Minute, Second], _} ->
            {time, {Hour, Minute, Second}};
        {error, _} ->
            binary_to_list(Data);  % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, bad_time})
    end;

type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_TIMESTAMP;
        Type == ?FIELD_TYPE_DATETIME ->
    case io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Data)) of % note: does not need conversion
        {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
            {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
        {error, _} ->
            binary_to_list(Data);   % todo: test and possibly conversion to UTF-8
        _ ->
            exit({error, datetime})
    end;

type_cast_row_data(Data, #field{type=Type})
    when Type == ?FIELD_TYPE_BIT ->
    case Data of
        <<1>> -> 1;
        <<0>> -> 0
    end;

% TODO:
% ?FIELD_TYPE_NEWDATE
% ?FIELD_TYPE_ENUM
% ?FIELD_TYPE_SET
% ?FIELD_TYPE_GEOMETRY

type_cast_row_data(Data, _) -> Data.

% TODO: [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol#COM_QUERY]
% field_count:          The value is always 0xfe (decimal ?RESP_EOF).
%                       However ... recall (from the
%                       section "Elements", above) that the value ?RESP_EOF can begin
%                       a Length-Encoded-Binary value which contains an 8-byte
%                       integer. So, to ensure that a packet is really an EOF
%                       Packet: (a) check that first byte in packet = 0xfe, (b)
%                       check that size of packet smaller than 9.


% This was used to approach a solution for proper handling of SERVER_MORE_RESULTS_EXIST
%
% recv_rest(Sock) ->
%   %-% io:format("~nrecv_rest: ", []),
%   case recv_packet_header_if_present(Sock) of
%       {PacketLength, SeqNum} ->
%           %-% io:format("recv_packet ('rest'): len: ~p, seq#: ~p ", [PacketLength, SeqNum]),
%           Data = recv_packet_body(Sock, PacketLength),
%           %-% io:format("data: ~p~n", [Data]),
%           Packet = #packet{size=PacketLength, seq_num=SeqNum, data=Data},
%           response(Sock, Packet);
%       none ->
%           %-% io:format("nothing~n", []),
%           nothing
%   end.


% -------------------------------------------------------------------------------
% Note: (*) The order of status and warnings count reversed for eof vs. ok packet.
% -------------------------------------------------------------------------------

% -----------------------------------------------------------------------------1-
% OK packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                       Name
%  -----                       ----
%  1   (Length Coded Binary)   field_count, always = 0
%  1-9 (Length Coded Binary)   affected_rows
%  1-9 (Length Coded Binary)   insert_id
%  2                           server_status
%  n   (until end of packet)   message
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1   (Length Coded Binary)   field_count, always = 0
%  1-9 (Length Coded Binary)   affected_rows
%  1-9 (Length Coded Binary)   insert_id
%  2                           server_status
%  2                           warning_count
%  n   (until end of packet)   message
%
%  field_count:     always = 0
%
%  affected_rows:   = number of rows affected by INSERT/UPDATE/DELETE
%
%  insert_id:       If the statement generated any AUTO_INCREMENT number,
%                   it is returned here. Otherwise this field contains 0.
%                   Note: when using for example a multiple row INSERT the
%                   insert_id will be from the first row inserted, not from
%                   last.
%
%  server_status:   = The client can use this to check if the
%                   command was inside a transaction.
%
%  warning_count:   number of warnings
%
%  message:         For example, after a multi-line INSERT, message might be
%                   "Records: 3 Duplicates: 0 Warnings: 0"
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol

% -----------------------------------------------------------------------------2-
% EOF packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                 Name
%  -----                 ----
%  1                     field_count, always = 0xfe
%
%  VERSION 4.1
%  Bytes                 Name
%  -----                 ----
%  1                     field_count, always = 0xfe
%  2                     warning_count
%  2                     Status Flags
%
%  field_count:          The value is always 0xfe (decimal 254).
%                        However ... recall (from the
%                        section "Elements", above) that the value 254 can begin
%                        a Length-Encoded-Binary value which contains an 8-byte
%                        integer. So, to ensure that a packet is really an EOF
%                        Packet: (a) check that first byte in packet = 0xfe, (b)
%                        check that size of packet smaller than 9.
%
%  warning_count:        Number of warnings. Sent after all data has been sent
%                        to the client.
%
%  server_status:        Contains flags like SERVER_MORE_RESULTS_EXISTS
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol

% -----------------------------------------------------------------------------3-
% Error packet format
% -------------------------------------------------------------------------------
%
%  VERSION 4.0
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xff
%  2                           errno (little endian)
%  n                           message
%
%  VERSION 4.1
%  Bytes                       Name
%  -----                       ----
%  1                           field_count, always = 0xff
%  2                           errno
%  1                           (sqlstate marker), always '#'
%  5                           sqlstate (5 characters)
%  n                           message
%
%  field_count:       Always 0xff (255 decimal).
%
%  errno:             The possible values are listed in the manual, and in
%                     the MySQL source code file /include/mysqld_error.h.
%
%  sqlstate marker:   This is always '#'. It is necessary for distinguishing
%                     version-4.1 messages.
%
%  sqlstate:          The server translates errno values to sqlstate values
%                     with a function named mysql_errno_to_sqlstate(). The
%                     possible values are listed in the manual, and in the
%                     MySQL source code file /include/sql_state.h.
%
%  message:           The error message is a string which ends at the end of
%                     the packet, that is, its length can be determined from
%                     the packet header. The MySQL client (in the my_net_read()
%                     function) always adds '\0' to a packet, so the message
%                     may appear to be a Null-Terminated String.
%                     Expect the message to be between 0 and 512 bytes long.
%
% Source: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
