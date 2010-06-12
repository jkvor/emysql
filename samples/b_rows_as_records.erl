% ------------------------------------------------------------------------
% Emysql Accessing Rows as Records: A minimal sample
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 12 Jun 2010
% ------------------------------------------------------------------------
%
% This sample does the same as the hello world examples but uses a record
% definition to access the result row.
%
% Instructions: 
%
% Create local mysql database (same as for previous samples):
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
%
% On *nix build and run using batch b_rows_as_records in folder samples:
% $ ./b_rows_as_records
%
% - or - 
%
% Make emysql and start this sample manually along these lines:
% $ cd ..
% $ make
% $ cd samples
% $ erlc b_rows_as_records.erl
% $ erl -pa ../ebin -s b_rows_as_records run -s init stop -noshell
%
% As output you should see a lot of PROGRESS-REPORTS from sasl and finally:
%
% ------------------------------------------------------------------------
% record: <<"Hello World!">>
%
% ------------------------------------------------------------------------
% Questions, mail Henning <hd2010@eonblast.com>, welcome.
% ------------------------------------------------------------------------


-module(b_rows_as_records).
-export([run/0]).

%% -----------------------------------------------------------------------
%% Record Definition:                                                    1
%% .......................................................................

-record(hello_record, {hello_text}).

%% -----------------------------------------------------------------------


run() ->

	application:start(sasl),
	crypto:start(),
	application:start(emysql),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	Result = emysql:execute(hello_pool, <<"SELECT * from hello_table">>),

	%% ------------------------------------------------------------------- 
	%% Records Fetch:                                                    2
	%% ................................................................... 

	Recs = emysql_util:as_record(
		Result, hello_record, record_info(fields, hello_record)),

	%% -------------------------------------------------------------------

	io:format("~n~s~n", [string:chars($-,72)]),

	%% -------------------------------------------------------------------
	%% Records Use:                                                      3
	%% ...................................................................

	[begin
      io:format("record: ~p~n", [Rec#hello_record.hello_text])
    end || Rec <- Recs],
    
	%% -------------------------------------------------------------------

    ok.

