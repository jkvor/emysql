% ------------------------------------------------------------------------
% Emysql Stored Procedures: A minimal sample 
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 12 Jun 2010
% ------------------------------------------------------------------------
%
% This sample does the same as the hello world examples but creating and
% using a stored procedure for the the select.
%
% Instructions: 
%
% Create local mysql database (same as for previous samples):
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
%
% On *nix build and run using batch c_stored_procedure in folder samples:
% $ ./c_stored_procedure
%
% - or - 
%
% Make emysql and start this sample manually along these lines:
% $ cd ..
% $ make
% $ cd samples
% $ erlc c_stored_procedure.erl
% $ erl -pa ../ebin -s c_stored_procedure run -s init stop -noshell
%
% As output you should see a lot of PROGRESS-REPORTS from sasl and finally:
%
% ------------------------------------------------------------------------
% Result: {result_packet,32,
%                      [{field,2,<<"def">>,<<"hello_database">>,
%                               <<"hello_table">>,<<"hello_table">>,
%                               <<"hello_text">>,<<"hello_text">>,254,<<>>,33,
%                               60,0,0}],
%                       [[<<"Hello World!">>]],
%                       <<>>}
% ------------------------------------------------------------------------
% Questions, mail Henning <hd2010@eonblast.com>, welcome.
% ------------------------------------------------------------------------


-module(c_stored_procedure).
-export([run/0]).

run() ->

	application:start(sasl),
	crypto:start(),
	application:start(emysql),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

	%% ------------------------------------------------------------------- 
	%% Stored procedure:
	%% ...................................................................

	emysql:prepare(hello_stmt, 
		<<"SELECT * from hello_table WHERE hello_text like ?">>),

	Result = emysql:execute(hello_pool, hello_stmt, ["Hello%"]),

	%% ------------------------------------------------------------------- 

	io:format("~n~s~n", [string:chars($-,72)]),
	io:format("Result: ~p~n", [Result]),

    ok.
    

