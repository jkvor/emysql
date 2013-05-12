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
-module(emysql_util).
-export([field_names/1, as_record/4, as_record/3, length_coded_binary/1, length_coded_string/1,
    null_terminated_string/2, asciz/1, bxor_binary/2, dualmap/3, hash/1,
    rnd/3, encode/1, encode/2, quote/1]).
-compile(export_all).

-include("emysql.hrl").

field_names(Result) when is_record(Result, result_packet) ->
    [Field#field.name || Field <- Result#result_packet.field_list].

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
    {Lookup, _} = lists:mapfoldl(
        fun(#field{name=Name}, Acc) ->
            {{binary_to_atom(Name, utf8), Acc}, Acc+1}
        end, 1, Result#result_packet.field_list),
    [begin
        RecordData = [case proplists:get_value(Field, Lookup) of
                undefined ->
                    undefined;
                Index ->
                    lists:nth(Index, Row)
        end || Field <- Fields],
        Fun(list_to_tuple([RecordName | RecordData]))
    end || Row <- Result#result_packet.rows].

as_record(Result, RecordName, Fields) when is_record(Result, result_packet), is_atom(RecordName), is_list(Fields) ->
    as_record(Result, RecordName, Fields, fun(A) -> A end).

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
encode(Val, ReturnType) when is_atom(Val) ->
    encode(atom_to_list(Val), ReturnType, latin1); % todo: latin1?

encode(Val, ReturnType) ->
    encode(Val, ReturnType, latin1).

encode(null, list, _) ->
    "null";

encode(undefined, list, _) ->
    "null";

encode(null, binary, _)  ->
    <<"null">>;

encode(undefined, binary, _)  ->
    <<"null">>;

encode(Val, list, latin1) when is_binary(Val) ->
    quote(binary_to_list(Val));

encode(Val, list, Encoding) when is_binary(Val) ->
    quote(unicode:characters_to_list(Val, Encoding));

encode(Val, binary, Encoding) when is_atom(Val) ->
	encode(atom_to_list(Val), binary, Encoding);

encode(Val, binary, latin1) when is_list(Val) -> 
	list_to_binary(quote(Val));

encode(Val, binary, Encoding) when is_list(Val) ->
    unicode:characters_to_binary(quote(Val), Encoding, Encoding);


encode(Val, binary, latin1) when is_binary(Val) ->
    %-% io:format("encode latin-1 in : ~s = ~w ~n", [Val, Val]),
    X = list_to_binary(quote(binary_to_list(Val))),
    %-% io:format("encode latin-1 out: ~s = ~w ~n", [X, X]),
    X;

encode(Val, binary, Encoding) when is_binary(Val) ->
    case unicode:characters_to_list(Val,Encoding) of
        {error,E1,E2} -> exit({invalid_utf8_binary, E1, E2});
        {incomplete,E1,E2} -> exit({invalid_utf8_binary, E1, E2});
        List ->
            unicode:characters_to_binary(quote(List),Encoding,Encoding)
    end;

encode(Val, list, _) when is_list(Val) ->
    %-% io:format("encode list in : ~s = ~w ~n", [Val, Val]),
    %-% io:format("encode list out: ~s = ~w ~n", [quote(Val), quote(Val)]),
    quote(Val);

encode(Val, list, _) when is_integer(Val) ->
    integer_to_list(Val);

encode(Val, binary, _) when is_integer(Val) ->
    list_to_binary(integer_to_list(Val));

encode(Val, list, _) when is_float(Val) ->
    [Res] = io_lib:format("~w", [Val]),
    Res;

encode(Val, binary, _) when is_float(Val) ->
    iolist_to_binary(io_lib:format("~w", [Val]));

encode({datetime, Val}, ReturnType, Encoding) ->
    encode(Val, ReturnType, Encoding);

encode({date, Val}, ReturnType, Encoding) ->
    encode(Val, ReturnType, Encoding);

encode({time, Val}, ReturnType, Encoding) ->
    encode(Val, ReturnType, Encoding);

encode({{Year, Month, Day}, {Hour, Minute, Second}}, list, _) ->
    Res = io_lib:format("'~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w'",
                        [Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res);

encode({{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}=Val, binary, E) ->
    list_to_binary(encode(Val, list, E));

encode({Time1, Time2, Time3}, list, _) ->
    Res = two_digits([Time1, Time2, Time3]),
    lists:flatten(Res);

encode({_Time1, _Time2, _Time3}=Val, binary, E) ->
    list_to_binary(encode(Val, list, E));

encode(Val, _, _) ->
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

quote(String, _) when is_list(String) ->
    quote(String);

quote(Any, Pool) when is_record(Any,pool) ->
    quote(Any, Pool#pool.encoding);

quote(Bin, latin1) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin)));

quote(Bin, Encoding) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin,Encoding) of
        {error,E1,E2} -> exit({invalid_utf8_binary, E1, E2});
        List ->
            unicode:characters_to_binary(quote(List),Encoding,Encoding)
    end.
    % note:quote is a codepoint-wise inspection (each is a number) that also works for Unicode.

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

anybin_to_list(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
        Bin -> unicode:characters_to_list(Bin);
        _ -> binary_to_list(Bin)
    end.

any_to_binary(L) when is_binary(L) ->
    L;
any_to_binary(L) when is_list(L) ->
    case unicode:characters_to_binary(L) of
        {error,_,_} -> list_to_binary(L);
        B -> case unicode:characters_to_list(B,utf8) of
            L -> B;
            _ -> list_to_binary(L)
        end
    end.

to_binary(L,_) when is_binary(L) ->
    L;

to_binary(L,latin1) when is_list(L) ->
    list_to_binary(L);

to_binary(L,Encoding) when is_list(L) ->
    unicode:characters_to_binary(L,Encoding,Encoding).
