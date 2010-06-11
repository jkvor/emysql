% ------------------------------------------------------------------------
% Hello World: Minimal sample usage of emysql
% H. Diedrich <hd2010@eonblast.com> - Eonblast http://www.eonblast.com
% 11 Jun 2010
% ------------------------------------------------------------------------
%
% Instructions:
%
% Create local mysql database:
% mysql> create database hello_database;
% mysql> use hello_database;
% mysql> create table hello_table (hello_text char(20));
% mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';
%
% On *nix build and run using batch a_hello in folder samples:
% $ ./a_hello
%
% - or - 
%
% Make emysql and start this sample manually along these lines:
% $ cd ..
% $ make
% $ cd samples
% $ erlc a_hello.erl
% $ erl -pa ../ebin -s a_hello run -s init stop -noshell
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
%                       <<>>}
% ------------------------------------------------------------------------
% Questions, mail Henning <hd2010@eonblast.com>, welcome.
% ------------------------------------------------------------------------


-module(a_hello).
-export([run/0]).

run() ->

	crypto:start(),
	application:start(emysql),

	emysql:add_pool(hello_pool, 1,
		"hello_username", "hello_password", "localhost", 3306,
		"hello_database", utf8),

	emysql:execute(hello_pool,
		<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    { _, _, _, Result, _ } = emysql:execute(hello_pool,
    	<<"select hello_text from hello_table">>),

	io:format("~n~p~n", [Result]),

    ok.
    
