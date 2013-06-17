%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Mike Oxford <moxford@gmail.com>
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
-module(emysql_util).
-export([
         asciz/1,
         as_dict/1,
         as_json/1,
         as_proplist/1,
         as_record/3,
         as_record/4,
         bxor_binary/2,
         dualmap/3,
         encode/1,
         encode/2,
         field_names/1,
         hash/1,
         length_coded_binary/1,
         length_coded_string/1,
         null_terminated_string/2,
         quote/1,
         rnd/3,
         to_binary/1
        ]).

-include("emysql.hrl").

field_names(Result) when is_record(Result, result_packet) ->
    [Field#field.name || Field <- Result#result_packet.field_list].

%% @spec as_dict(Result) -> dict
%%      Result = #result_packet{}
%%
%% @doc package row data as a dict
%%
%% -module(fetch_example).
%%
%% fetch_foo() ->
%%  Res = emysql:execute(pool1, "select * from foo"),
%%  Res:as_dict(Res).
as_dict(Res = #result_packet{}) ->
    dict:from_list(as_proplist(Res)).
 
%% @spec as_proplist(Result) -> proplist
%%      Result = #result_packet{}
%%
%% @doc package row data as a proplist
%%
%% -module(fetch_example).
%%
%% fetch_foo() ->
%%  Res = emysql:execute(pool1, "select * from foo"),
%%  Res:as_proplist(Res).
as_proplist(#result_packet{field_list=_Cols,rows=_Vals}) when _Cols =:= undefined, 
							      _Vals =:= undefined ->
    [];
as_proplist(#result_packet{field_list=_Cols,rows=_Vals}) when is_list(_Cols), 
								  _Vals =:= undefined ->
    [];
as_proplist(#result_packet{field_list=_Cols,rows=_Vals}) when is_list(_Cols), 
								  _Vals =:= [] ->
    [];
as_proplist(Res = #result_packet{field_list=Cols,rows=Vals}) when is_list(Cols), 
								  is_list(Vals) ->
    FieldData = emysql_util:field_names(Res),
    RowData = case lists:flatten(Vals) of
		  [] ->
		      array:to_list(array:new([erlang:length(FieldData)]));
		  Data ->
		      Data
	      end,
    emysql_util:dualmap(fun(A,B)->{A,B} end, FieldData, RowData).

%% @spec as_record(Result, RecordName, Fields, Fun) -> Result
%%      Result = #result_packet{}
%%      RecordName = atom()
%%      Fields = [atom()]
%%      Fun = fun()
%%      Result = [Row]
%%      Row = [record()]
%%
%% @doc package row data as records
%%
%% RecordName is the name of the record to generate.
%% Fields are the field names to generate for each record.
%%
%% -module(fetch_example).
%%
%% fetch_foo() ->
%%  Res = emysql:execute(pool1, "select * from foo"),
%%  Res:as_record(foo, record_info(fields, foo)).
as_record(Result, RecordName, Fields, Fun) when is_record(Result, result_packet), is_atom(RecordName), is_list(Fields), is_function(Fun) ->
	Columns = Result#result_packet.field_list,

	S = lists:seq(1, length(Columns)),
	P = lists:zip([ binary_to_atom(C1#field.name, utf8) || C1 <- Columns ], S),
	F = fun(FieldName) ->
		case proplists:lookup(FieldName, P) of
			none ->
					fun(_) -> undefined end;
			{FieldName, Pos} ->
					fun(Row) -> lists:nth(Pos, Row) end
		end
	end,
	Fs = [ F(FieldName) || FieldName <- Fields ],
	F1 = fun(Row) ->
		RecordData = [ Fx(Row) || Fx <- Fs ],
		Fun(list_to_tuple([RecordName|RecordData]))
	end,
	[ F1(Row) || Row <- Result#result_packet.rows ].

as_record(Result, RecordName, Fields) when is_record(Result, result_packet), is_atom(RecordName), is_list(Fields) ->
    as_record(Result, RecordName, Fields, fun(A) -> A end).

%% @spec as_json(Result) -> Result
%% @doc package row data as erlang json (jsx/jiffy compatible)
as_json(Result) when is_record(Result, result_packet) ->
    Fields = emysql_util:field_names(Result),
    Rows = Result#result_packet.rows,
    [begin
        {JSRow, _} = lists:mapfoldl( fun(K, [V | T]) -> {{K, json_val(V)}, T} end, Row, Fields),
        JSRow
    end || Row <- Rows].

json_val(undefined) ->
    null;
json_val({date,{Year,Month,Day}}) ->
    iolist_to_binary( io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
json_val({datetime,{ {Year,Month,Day}, {Hour,Min,Sec} }}) ->
    iolist_to_binary( io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ", [Year, Month, Day, Hour, Min, Sec]));
json_val(Value) ->
    Value.

length_coded_binary(<<>>) -> {<<>>, <<>>};
length_coded_binary(<<FirstByte:8, Tail/binary>>) ->
    if
        FirstByte =< 250 -> {FirstByte, Tail};
        FirstByte == 251 -> {undefined, Tail};
        FirstByte == 252 ->
            <<Word:16/little, Tail1/binary>> = Tail,
            {Word, Tail1};
        FirstByte == 253 ->
            <<Word:24/little, Tail1/binary>> = Tail,
            {Word, Tail1};
        FirstByte == 254 ->
            <<Word:64/little, Tail1/binary>> = Tail,
            {Word, Tail1};
        true ->
            exit(poorly_formatted_length_encoded_binary)
    end.

length_coded_string(<<>>) -> {<<>>, <<>>};
length_coded_string(Bin) ->
    case length_coded_binary(Bin) of
        {undefined, Rest} ->
            {undefined, Rest};
        {Length, Rest} ->
            case Rest of
                <<String:Length/binary, Rest1/binary>> ->
                    {String, Rest1};
                _ ->
                    exit(poorly_formatted_length_coded_string)
            end
    end.

null_terminated_string(<<0, Tail/binary>>, Acc) ->
    {Acc, Tail};
null_terminated_string(<<_:8>>, _) ->
    exit(poorly_formatted_null_terminated_string);
null_terminated_string(<<H:1/binary, Tail/binary>>, Acc) ->
    null_terminated_string(Tail, <<Acc/binary, H/binary>>).

asciz(Data) when is_binary(Data) ->
    asciz_binary(Data, []);
asciz(Data) when is_list(Data) ->
    {String, [0 | Rest]} = lists:splitwith(fun (C) -> C /= 0 end, Data),
    {String, Rest}.

asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) -> E1 bxor E2 end, binary_to_list(B1), binary_to_list(B2))).
    % note: only call from auth, password hashing, using int list returned from sha.

dualmap(_F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

hash(S) -> hash(S, 1345345333, 305419889, 7).
hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, _Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).
rnd(0, List, _, _) ->
    lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N - 1, [Val | List], NSeed1, NSeed2).

%% @doc Encode a value so that it can be included safely in a MySQL query.
%% @spec encode(term()) -> binary() | {error, Error}
encode(Val) ->
    encode(Val, binary).

%% @doc Encode a value so that it can be included safely in a MySQL query.
%% @spec encode(term(), list | binary) -> string() | binary() | {error, Error}
encode(null, list) ->
    "null";
encode(undefined, list) ->
    "null";
encode(null, binary)  ->
    <<"null">>;
encode(undefined, binary)  ->
    <<"null">>;
encode(Val, list) when is_binary(Val) ->
    quote(binary_to_list(Val));
encode(Val, binary) when is_atom(Val) ->
    encode(atom_to_list(Val), binary);
encode(Val, binary) when is_list(Val) -> 
    list_to_binary(quote(Val));
encode(Val, binary) when is_binary(Val) ->
    list_to_binary(quote(binary_to_list(Val)));
encode(Val, list) when is_list(Val) ->
    quote(Val);
encode(Val, list) when is_integer(Val) ->
    integer_to_list(Val);
encode(Val, binary) when is_integer(Val) ->
    list_to_binary(integer_to_list(Val));
encode(Val, list) when is_float(Val) ->
    [Res] = io_lib:format("~w", [Val]),
    Res;
encode(Val, binary) when is_float(Val) ->
    iolist_to_binary(io_lib:format("~w", [Val]));
encode({datetime, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({date, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({time, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({{Year, Month, Day}, {Hour, Minute, Second}}, list) ->
    Res = io_lib:format("'~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w'",
                        [Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res);
encode({{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}=Val, binary) ->
    list_to_binary(encode(Val, list));
encode({Time1, Time2, Time3}, list) ->
    Res = two_digits([Time1, Time2, Time3]),
    lists:flatten(Res);
encode({_Time1, _Time2, _Time3}=Val, binary) ->
    list_to_binary(encode(Val, list));
encode(Val, _) ->
    {error, {unrecognized_value, Val}}.

%% @private
two_digits(Nums) when is_list(Nums) ->
    [two_digits(Num) || Num <- Nums];
two_digits(Num) ->
    [Str] = io_lib:format("~b", [Num]),
    case length(Str) of
        1 -> [$0 | Str];
        _ -> Str
    end.

%% @doc Quote a string or binary value so that it can be included safely in a
%% MySQL query. For the quoting, a binary is converted to a list and back.
%% For this, it's necessary to know the encoding of the binary.
%% @spec quote(x()) -> x()
%%       x() = list() | binary()
%% @end
%% hd/11,12
quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote_loop(String)])]. %% 39 is $'

%% @doc  Make MySQL-safe backslash escapes before 10, 13, \, 26, 34, 39.
%% @spec quote_loop(list()) -> list()
%% @private
%% @end
%% hd/11,12
quote_loop(List) ->
    quote_loop(List, []).

quote_loop([], Acc) ->
    Acc;

quote_loop([0 | Rest], Acc) ->
    quote_loop(Rest, [$0, $\\ | Acc]);

quote_loop([10 | Rest], Acc) ->
    quote_loop(Rest, [$n, $\\ | Acc]);

quote_loop([13 | Rest], Acc) ->
    quote_loop(Rest, [$r, $\\ | Acc]);

quote_loop([$\\ | Rest], Acc) ->
    quote_loop(Rest, [$\\ , $\\ | Acc]);

quote_loop([39 | Rest], Acc) -> %% 39 is $'
    quote_loop(Rest, [39, $\\ | Acc]); %% 39 is $'

quote_loop([34 | Rest], Acc) -> %% 34 is $"
    quote_loop(Rest, [34, $\\ | Acc]); %% 34 is $"

quote_loop([26 | Rest], Acc) ->
    quote_loop(Rest, [$Z, $\\ | Acc]);

quote_loop([C | Rest], Acc) ->
    quote_loop(Rest, [C | Acc]).

%% UTF-8 is designed in such a way that ISO-latin-1 characters with
%% numbers beyond the 7-bit ASCII range are seldom considered valid
%% when decoded as UTF-8. Therefore one can usually use heuristics
%% to determine if a file is in UTF-8 or if it is encoded in
%% ISO-latin-1 (one byte per character) encoding. The unicode module
%% can be used to determine if data can be interpreted as UTF-8.
%% Source: http://www.erlang.org/doc/apps/stdlib/unicode_usage.html

to_binary(L) when is_binary(L) -> L;
to_binary(L) when is_list(L)   -> list_to_binary(L).
