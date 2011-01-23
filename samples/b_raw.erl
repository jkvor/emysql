% ------------------------------------------------------------------------
% Hello World 2: Another minimal sample usage of emysql
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 11 Jun 2010
% ------------------------------------------------------------------------
%
% This sample shows rawer output. It is more robust.
%
% Instructions:
%
% Create local mysql database (same as for previous sample):
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
%
% On *nix build and run using batch a_hello2 in folder samples:
% $ ./a_hello2
%
% - or - 
%
% Make emysql and start this sample manually along these lines:
% $ cd ..
% $ make
% $ cd samples
% $ erlc a_hello2.erl
% $ erl -pa ../ebin -s a_hello2 run -s init stop -noshell
%
% As output you should see a lot of PROGRESS-REPORTS from sasl and finally:
%
% ------------------------------------------------------------------------
% Result: {result_packet,5,
%                       [{field,2,<<"def">>,<<"hello_database">>,
%                               <<"hello_table">>,<<"hello_table">>,
%                               <<"hello_text">>,<<"hello_text">>,254,<<>>,33,
%                               60,0,0}],
%                       [[<<"Hello World!">>]],
%                       <<>>}
% ------------------------------------------------------------------------
% Questions, mail Henning <hd2010@eonblast.com>, welcome.
% ------------------------------------------------------------------------


-module(a_hello2).
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
	%% Vs a_hello.erl: get complete Result, not merely {_,_,_,Result,_}
	%% ...................................................................
    Result = emysql:execute(hello_pool,
    	<<"select hello_text from hello_table">>),
	%% ------------------------------------------------------------------- 

	io:format("~n~s~n", [string:chars($-,72)]),
	io:format("Result: ~p~n", [Result]),

    ok.
    
