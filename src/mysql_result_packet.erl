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
-module(mysql_result_packet, [SeqNum, FieldList, Rows, Extra]).
-export([field_list/0, rows/0, as_record/2]).
-include("emysql.hrl").

%% @spec field_list() -> Result
%%		 Result = [field()]
field_list() -> FieldList.

%% @spec rows() -> Result
%%		 Result = [Row]
%%		 Row = [any()]
rows() -> Rows.

%% @spec as_record(RecordName, Fields) -> Result
%%		 RecordName = atom() (the name of the record to generate)
%%		 Fields = [atom()] (the field names to generate for each record)
%%		 Result = [Row]
%%		 Row = [record()]
%% @doc package row data as records
%%
%% -module(fetch_example).
%%
%% fetch_foo() ->
%%	  Res = mysql:execute(pool1, "select * from foo"),
%%	  Res:as_record(foo, record_info(fields, foo)).
as_record(RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->
	{Lookup, _} = lists:mapfoldl(
		fun(#field{name=Name}, Acc) ->
			{{list_to_atom(Name), Acc}, Acc+1}
		end, 1, FieldList),
	[begin
		RecordData = [case proplists:get_value(Field, Lookup) of
				undefined ->
					undefined;
				Index ->
					lists:nth(Index, Row)
		 end || Field <- Fields],
		list_to_tuple([RecordName | RecordData])
	 end || Row <- Rows].
