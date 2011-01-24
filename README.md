## Emysql 

Erlang mysql driver. Fork with fixes, more docs and samples. Supports prepared statements and stored procedures.

While you can use mysql via ODBC, using Emysql, or another native solution, should perform better. This is one of the multiple maintained mysql drivers on github.


## Contents

* History
* Hints on Usage
* Samples
* Links
* Todo
* License


## History

There are three native solutions out there  
* [erlang-mysql / Yxa](https://support.process-one.net/doc/display/CONTRIBS/Yxa) of 2005-08 by Process One  
* [erlang-mysql-driver](http://code.google.com/p/erlang-mysql-driver/) a 2006 derivate by Yariv Sadan, Dave Smith et al  
* [emysql](http://github.com/JacobVorreuter/emysql) a 2009 rewrite by Jacob Vorreuter, Bill Warnecke  

This is a fork of the newest one, [JacobVorreuter/emysql](http://github.com/JacobVorreuter/emysql) with fixes, more docu and samples.

[Jacob Vorreuter](http://github.com/JacobVorreuter) in 2009 rewrote
[Yariv Sadan's](http://yarivsblog.com/) 2006-07 
[erlang-mysql-driver](http://code.google.com/p/erlang-mysql-driver/),
which was a derivate and extension of
[Process One's Yxa erlang-mysql](https://support.process-one.net/doc/display/CONTRIBS/Yxa) of 2006. 
Yxa meanwhile had an extension patch in 2008 that never made it into Yariv's driver, 
which has not been updated since Oct '07. 
In Feb '10, [Dave Smith](http://github.com/dizzyd) started making some
[updates](http://github.com/dizzyd/erlang-mysql-driver) and put them on github.

Jacob rewrote the erlang-mysql-driver code because he felt it had been touched by so many
people that it had become more complicated than necessary. According to Jacob, Emysql
is pretty stable and ran without issue in an production environment at Electronic Arts.

[Vitaliy Batichko](https://github.com/bva) and
[Chris Rempel](https://github.com/csrl) have contributed updates to this branch.

Thank you!


## Hints on Usage 

#### Start the application

	crypto:start(),
	application:start(emysql).
	
#### Add a pool
	% emysql:add_pool(PoolName, PoolSize, Username, Password, Host, Port, Database, Encoding) ->
	%	ok | {error, pool_already_exists}  
	% PoolName = atom()  
	% PoolSize = integer()  
	% Username = string()  
	% Password = string()  
	% Host = string()  
	% Port = integer()  
	% Database = string()  
	% Encoding = atom()  

	emysql:add_pool(mypoolname, 1, "username", "mypassword", "localhost", 3306, "mydatabase", utf8).
	
#### Record Types
	-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
	-record(error_packet, {seq_num, code, msg}).
	-record(result_packet, {seq_num, field_list, rows, extra}).

#### Executing SQL statements
	% emysql:execute(PoolName, Statement) -> result_packet() | ok_packet() | error_packet()  
	% PoolName = atom()  
	% Statement = string() | binary()  	

	emysql:execute(mypoolname, <<"SELECT * from mytable">>).
	#result_packet{field_list=[...], rows=[...]}
	
	emysql:execute(mypoolname, <<"UPDATE mytable SET bar = 'baz' WHERE id = 1">>).
	#ok_packet{affected_rows=1}
	
#### Executing prepared statements
	% emysql:prepare(StmtName, Statement) -> ok  
	% StmtName = atom()  
	% Statement = binary() | string()  

	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	ok
	
	% emysql:execute(PoolName, StmtName, Args) -> result_packet() | ok_packet() | error_packet()  
	% StmtName = atom()  
	% Args = [term()]  

	emysql:execute(mypoolname, my_stmt, [1]).
	#result_packet{field_list=[...], rows=[...]}

#### Executing stored procedures

	% emysql:execute(PoolName, StmtName, Args) -> result_packet() | ok_packet() | error_packet()  
	% StmtName = atom()  
	% Args = [term()]  

	emysql:execute(hello_pool,
		<<"create procedure sp_hello() begin select * from hello_table; end">>).
	{ok_packet,1,0,0,2,0,[]}

	emysql:execute(hello_pool, <<"call sp_hello();">>).
	[{result_packet,6,
	                [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
	                        <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
	                        254,<<>>,33,60,0,0}],
	                [[<<"Hello World!">>],[<<"Hello World!">>]],
	                <<>>},
	{ok_packet,7,0,0,34,0,[]}]
 
#### Converting Row Data To Records
	% emysql_util:as_record(ResultPacket, RecordName, Fields) -> Result  
	% ResultPacket = result_packet()  
	% RecordName = atom() (the name of the record to generate)  
	% Fields = [atom()] (the field names to generate for each record)  
	% Result = [record()]  

	-module(fetch_example).
	-record(foo, {bar, baz, bat}).
	
	fetch_foo() ->
	   Result = emysql:execute(pool1, <<"select bar, baz, bat from foo">>),
	   Recs = emysql_util:as_record(Result, foo, record_info(fields, foo)),
	   [begin
		  io:format("foo: ~p, ~p, ~p~n", [Foo#foo.bar, Foo#foo.baz, Foo#foo.bat])
	    end || Foo <- Recs].

## Getting Emysql

	$ git clone git://github.com/Eonblast/Emysql.git Emysql
	
## Samples

#### Hello World

This is a hello world program. Follow the three steps below to try it out.
	
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

	    Result = emysql:execute(hello_pool,
			<<"select hello_text from hello_table">>),

		io:format("~n~p~n", [Result]).


We come back to that source, but first:

#### Building Emysql

Build emysql.app, using make:

	$ cd Emysql
	$ make


#### Sample database

For the above sample, create a local mysql database. You should have a mysql server installed and running:
	
	$ mysql [-u<user> -p]
	mysql> create database hello_database;
	mysql> use hello_database;
	mysql> create table hello_table (hello_text char(20));
	mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';

#### Run Hello

Be sure to have ./ebin in your Erlang path. Now copy the Hello World source above into a file hello.erl and run it (in the Emysql root directory):

	$ erlc hello.erl
	$ erl -pa ../ebin -s hello run -s init stop -noshell

See more sample programms, below.

#### Running the Samples
Sample programs are in ./samples. 

* [a_hello](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello.erl) - Hello World
* [a_hello2](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello2.erl) - Hello World somewhat rawer
* [b_rows_as_records](http://github.com/Eonblast/Emysql/blob/master/samples/b_rows_as_records.erl) - Using Erlang records to access result rows
* [c_stored_procedure](http://github.com/Eonblast/Emysql/blob/master/samples/c_stored_procedure.erl) - Using Stored procedures

To run the samples, create the database as listed above at localhost, and:

	$ cd samples
	$ ./a_hello
	$ ./a_hello2
	$ ./b_rows_as_records
	$ ./c_stored_procedure
	
or make emysql.app and start a_hello etc. manually along these lines (but
first create the database as listed above):

	$ make
	$ cd samples
	$ erlc a_hello.erl
	$ erl -pa ../ebin -s a_hello run -s init stop -noshell

## Links

* [Emysql on Github](http://github.com/Eonblast/Emysql)
* [MySQL Client Server Protocol](http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol)
* [MySQL 5.5 Source](ftp://ftp.fu-berlin.de/unix/databases/mysql/Downloads/MySQL-5.5/mysql-5.5.8.tar.gz)

## TODO
* decrementing pool size could close sockets that are in use
* spawn individual conn_mgr gen_server processes for each pool
* allow row results to be returned as binary

## License

Copyright (c) 2009-2011
Bill Warnecke <bill@rupture.com>,
Jacob Vorreuter <jacob.vorreuter@gmail.com>,
Henning Diedrich <hd2010@eonblast.com>,
Eonblast Corporation [http://www.eonblast.com].

Permission is  hereby  granted,  free of charge,  to any person
obtaining  a copy of this software and associated documentation
files (the "Software"),to deal in the Software without restric-
tion,  including  without  limitation the rights to use,  copy, 
modify, merge,  publish,  distribute,  sublicense,  and/or sell
copies  of the  Software,  and to  permit  persons to  whom the
Software  is  furnished  to do  so,  subject  to the  following 
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.