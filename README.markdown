## emysql

Erlang mysql driver. Fork with a tad more doku and samples.


#### Situation

While you can use mysql via ODBC, using emysql, or another native solution, should perform better.

There are three native solutions out there it seems  
* [erlang-mysql](https://support.process-one.net/doc/display/CONTRIBS/Yxa) by Process One  
* [erlang-mysql-driver](http://code.google.com/p/erlang-mysql-driver/) a 2006 derivate by Yariv Sadan, Dave Smith et al  
* [emysql](http://github.com/JacobVorreuter/emysql) a 2009 rewrite by Jacob Vorreuter, Bill Warnecke  

This here is a fork of [JacobVorreuter/emysql](http://github.com/JacobVorreuter/emysql) with a tad more docu & samples.

Jacob rewrote [Yariv Sadan's](http://yarivsblog.com/) 2006-07 derivate [erlang-mysql-driver](http://code.google.com/p/erlang-mysql-driver/)
of [Process One's Yxa erlang-mysql](https://support.process-one.net/doc/display/CONTRIBS/Yxa) from 2006. 
Yxa meanwhile had an extension patch in 2008 that never made it into Yariv's driver, which has not
been updated since Oct '07. In Feb '10, Dave Smith started making some [updates](http://github.com/dizzyd/erlang-mysql-driver) and put them on github.

Jake rewrote the erlang-mysql-driver code because he felt it had been touched by so many people that it had
become more complicated than necessary. Emysql is pretty stable and ran without issue in an production environment at Electronic Arts.


#### Contents

* Situation
* Hints on Usage
* Samples
* Todo
* License

## Hints on Usage 

#### Start the application

	crypto:start(),
	application:start(emysql).
	
#### Add a pool
emysql:add\_pool(PoolName, PoolSize, Username, Password, Host, Port, Database, Encoding) -> ok | {error, pool\_already\_exists}  
PoolName = atom()  
PoolSize = integer()  
Username = string()  
Password = string()  
Host = string()  
Port = integer()  
Database = string()  
Encoding = atom()  

	emysql:add_pool(mypoolname, 1, "username", "mypassword", "localhost", 3306, "mydatabase", utf8).
	
#### Record Types
	-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
	-record(error_packet, {seq_num, code, msg}).
	-record(result_packet, {seq_num, field_list, rows, extra}).

#### Executing SQL statements
emysql:execute(PoolName, Statement) -> result\_packet() | ok\_packet() | error\_packet()  
PoolName = atom()  
Statement = string() | binary()  	

	emysql:execute(mypoolname, <<"SELECT * from mytable">>).
	#result_packet{field_list=[...], rows=[...]}
	
	emysql:execute(mypoolname, <<"UPDATE mytable SET bar = 'baz' WHERE id = 1">>).
	#ok_packet{affected_rows=1}
	
#### Executing prepared statements
emysql:prepare(StmtName, Statement) -> ok  
StmtName = atom()  
Statement = binary() | string()  

	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	ok
	
emysql:execute(StmtName, Args) -> result\_packet() | ok\_packet() | error\_packet()  
StmtName = atom()  
Args = [term()]  

	emysql:execute(my_stmt, [1]).
	#result_packet{field_list=[...], rows=[...]}

#### Converting Row Data To Records
emysql\_util:as\_record(ResultPacket, RecordName, Fields) -> Result  
ResultPacket = result\_packet()  
RecordName = atom() (the name of the record to generate)  
Fields = [atom()] (the field names to generate for each record)  
Result = [record()]  

	-module(fetch_example).
	-record(foo, {bar, baz, bat}).
	
	fetch_foo() ->
	   Result = emysql:execute(pool1, <<"select bar, baz, bat from foo">>),
	   Recs = emysql_util:as_record(Result, foo, record_info(fields, foo)),
	   [begin
		  io:format("foo: ~p, ~p, ~p~n", [Foo#foo.bar, Foo#foo.baz, Foo#foo.bat])
	    end || Foo <- Recs].

## Samples

#### Hello World
To run the following, first build emysql.app, using make, 
and be sure to have ./ebin in your Erlang path. See sample programms, below.
	
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

#### Run Samples
Sample programs are in ./samples. To run a hello world sample:

	$ cd samples
	$ ./a_hello
	
or make emysql.app and start a_hello manually along these lines:

	$ make
	$ cd samples
	$ erlc a_hello.erl
	$ erl -pa ../ebin -s a_hello run -s init stop -noshell


#### Sample database
For the above sample, create a local mysql database:
	
	mysql> create database hello_database;
	mysql> use hello_database;
	mysql> create table hello_table (hello_text char(20));
	mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';


## TODO
* decrementing pool size could close sockets that are in use
* spawn individual conn\_mgr gen\_server processes for each pool
* allow row results to be returned as binary

## License

Copyright (c) 2009-2010   
[Bill Warnecke](http://github.com/wwarneck) <bill@rupture.com>   
[Jacob Vorreuter](http://github.com/JacobVorreuter) <jacob.vorreuter@gmail.com>  
[Henning Diedrich](http://www.eonblast.com) <hd2010@eonblast.com>   
 
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.
 
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.