# Emysql 

Erlang MySQL driver, based on a rewrite for Electronic Arts. Connection pooling, prepared statements & stored procedures.

While you can use mysql via ODBC, using a driver, like Emysql, should perform better. For [samples][] and [docs][] see below. Read the brief on [choosing a package][choosing] and about the [history][] of the various MySQL drivers.

This package is a direct continuation of the original [emysql][1] with [fixes][], [updates][], more [documentation][docs] and [samples][]. [Emysql][1] is a cleaner rewrite of [erlang-mysql-driver][2], see [History][].

<hr/>

 **Which fork/package should I use?** See [Choosing][].  
 **Why are there so many?** See [History][].  
 **Who used *this* fork?** Electronic Arts.  
 **How do I ...?** See [Samples][].  

 **Download:** <https://github.com/Eonblast/Emysql/archives/master>  
 **Docs:** <http://eonblast.github.com/Emysql/>  
 **Issues:** <https://github.com/Eonblast/Emysql/issues>  
 **Repository:** <https://github.com/Eonblast/Emysql>  

<hr/>

## Contents

* [Choosing][]
* [Samples][]
* [Usage][]
* [History][]
* [Links][]
* [Todo][]
* [License][]

<hr/>

## Choosing                                             <a name="choosing"></a>

If you are looking for the plain necessities, you should use the [ejabberd][7] mysql driver. It is battle tested and stable. If you want stored procedures, use this package, [Emysql][emysql]. For mnesia-style transactions, one of the multiple [erlang-mysql-driver][16]s may suite you best.  

To learn more about out the differences between the drivers, see the [mysql driver history][history].

## Getting Emysql

	$ git clone git://github.com/Eonblast/Emysql.git Emysql


## Samples                                             <a name=samples></a>

### Hello World

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


We'll be coming back to this source in a minute.

### Executing an SQL Statements

	emysql:execute(my_pool, <<"SELECT * from mytable">>).

For the exact spec, see below, [Usage][]. Regarding the 'pool', also see below.

### Executing a Prepared Statements

	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	
	emysql:execute(my_pool, my_stmt, [1]).

### Executing a Stored Procedures

	emysql:execute(my_pool, <<"create procedure my_sp() begin select * from mytable; end">>).
	
	emysql:execute(my_pool, <<"call my_sp();">>).

### Result Record

	-record(result_packet, {seq_num, field_list, rows, extra}).
	
### Converting Row Data To Records

	-record(foo, {bar, baz}).

	Result = emysql:execute(pool1, <<"select bar, baz from foo">>).
	Recs = emysql_util:as_record(Result, foo, record_info(fields, foo)).
	Bars = [Foo#foo.bar || Foo <- Recs].

### Adding a Connection to the Connection Pool

Emysql uses a sophisticated connection pooling mechanism.

	emysql:add_pool(my_pool, 1, "myuser", "mypass", "myhost", 3306, "mydatabase", utf8).

### Running Hello World

Let's run the hello world sample from above:

#### 1. Build Emysql

Build emysql.app, using make:

	$ cd Emysql
	$ make


#### 2. Make a Sample Database

For use in the above sample (and all of those below, too), create a local mysql database. You should have a mysql server installed and running:
	
	$ mysql [-u<user> -p]
	mysql> create database hello_database;
	mysql> use hello_database;
	mysql> create table hello_table (hello_text char(20));
	mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';

#### 3. Paste & Run Hello

Be sure to have ./ebin in your Erlang path. The hello-world source as shown above already waits in the Emysql directory, as hello.erl. Just compile and run it:

	$ erlc hello.erl
	$ erl -pa ./ebin -s hello run -s init stop -noshell

That was easy!

There are more sample programs:

## More Samples
Sample programs are in ./samples. 

* [a_hello](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello.erl) - Hello World
* [b_raw](http://github.com/Eonblast/Emysql/blob/master/samples/b_raw.erl) - Hello World, raw output
* [c\_rows\_as\_records](http://github.com/Eonblast/Emysql/blob/master/samples/c_rows_as_records.erl) - Using Erlang records to access result rows
* [d\_prepared\_statement](http://github.com/Eonblast/Emysql/blob/master/samples/d_prepared_statement.erl) - Using prepared statements
* [e\_stored\_procedure](http://github.com/Eonblast/Emysql/blob/master/samples/e_stored_procedure.erl) - Using stored procedures

To run the samples, create the database as listed above at localhost, and simply run the compile & run batches:

	$ cd samples
	$ ./a_hello
	$ ./b_raw
	$ ./c_rows_as_records
	$ ./d_prepared_statement
	$ ./e_stored_procedure
	
or (after building emysql.app and the database, as explained above), start a_hello etc. manually along these lines:

	$ make
	$ cd samples
	$ erlc a_hello.erl
	$ erl -pa ../ebin -s a_hello run -s init stop -noshell

## Usage 

General Notes on using Emysql, including the actual specs:

#### Starting an Application

The Emysql driver is an Erlang gen-server, and, application.

	crypto:start(),
	application:start(emysql).

#### Adding a Pool

	% emysql:add_pool(PoolName, PoolSize, Username, Password, Host, Port, Database, Encoding) ->
	%	 ok | {error, pool_already_exists}  
	% PoolName = atom()  
	% PoolSize = integer()  
	% Username = string()  
	% Password = string()  
	% Host = string()  
	% Port = integer()  
	% Database = string()  
	% Encoding = atom()  
	
	emysql:add_pool(mypoolname, 1, "username", "mypassword", "localhost", 3306, "mydatabase", utf8).

#### More Record Types

	-record(result_packet, {seq_num, field_list, rows, extra}).
	
	-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
	
	-record(error_packet, {seq_num, code, msg}).

For other record types, see include/emysql.hrl.

#### Executing SQL Statements

	% emysql:execute(PoolName, Statement) -> result_packet() | ok_packet() | error_packet()  
	% PoolName = atom()  
	% Statement = string() | binary()  
	
	emysql:execute(mypoolname, <<"SELECT * from mytable">>).
	# result_packet{field_list=[...], rows=[...]}
	
	emysql:execute(mypoolname, <<"UPDATE mytable SET bar = 'baz' WHERE id = 1">>).
	# ok_packet{affected_rows=1}

#### Executing Prepared Statements

	% emysql:prepare(StmtName, Statement) -> ok  
	% StmtName = atom()  
	% Statement = binary() | string()  
	
	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	# ok

	% emysql:execute(PoolName, StmtName, Args) -> result_packet() | ok_packet() | error_packet()  
	% StmtName = atom()  
	% Args = [term()]  
	
	emysql:execute(mypoolname, my_stmt, [1]).
	#result_packet{field_list=[...], rows=[...]}

#### Executing Stored Procedures

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
 
 Note that you are getting back a list of results here.
 
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


## History                                               <a name="history"></a>

Open Source Erlang MySQL driver efforts are currently a fractured matter. There are four main choices.

* **Yxa**: The [first][17] Erlang MySQL driver, in ~270 lines of code, seems to have been written between 2001 and 2004 by [Magnus Ahltorp][ma] at the Swedish [Royal Institute of Technology][3]. It is low level and on its own, blocking. In 2005 [Fredrik Thulin][fr] brought it into it's current modular form of three process layers: a high level, connection pooling, async module `mysql'; the underlying single-connection, blocking `mysql_conn.erl' that does the bit-level talk in the [MySQL protocol][18] and can also be used stand-alone. And which uses the protocol agnostic receiver processes `mysql_recv' that handle the low level socket communication with the server, no matter the content. To use the driver in the [Yxa][5] project, Fredrik gave it gen-server behavior, added logging, error messages, support for the new MySQL authentication protocol, and commented the source. This [mysql driver source][4] is complete and stable since at least 2007, it is available as part of the SIP proxy [Yxa 1.0][5] (hosted [on github][6]). It has no support for transactions or stored procedures. It is the basis for the following two packages. It's basic modular division and general functionality where not changed but only enhanced in later incarnations. It had originally been agreed that this package should receive and merge the upstream of the following two forks but unfortunately, that did not come to pass.

* **ejabberd**: In 2006, a [fork][7] of the Yxa driver was created by [Mickael Remond][mr] at [Process One][8] to become part of the successful instant messaging server [ejabberd][9] (also hosted [at github][10]). It can be assumed to be as stable as the Yxa branch, didn't change a byte in the lowest level, and only enhanced the higher levels. The difference to the original Yxa branch mainly consists of added inspection functions that help using the query results, and of an [independent adoption][11] to the MySQL 4.1 client-server protocol. The original Yxa branch has meanwhile adopted edoc comment format. Find a diff between Yxa and the ejabberd version [here][12], and one ignoring comments [here][13]. These two branches could, be merged quiet easily, probably without any change in functionality.

* **erlang-mysql-driver**: The storied life of this package begain in 2006 when [Yariv Sadan][ys] created a fork from the ejabberd branch, made it a standalone project, gave it a maximally generic name that stuck, and hosted it at [Google Code][15]. Before he moved on to work at Facebook, he added high-level handling of prepared statements and transactions, and at long last completed some loose ends with the connection pooling that had been known to be lagging since the Yxa version. But there were also changes both in the original Yxa and the ejabberd branch after the forking off in 2006 that seem to never have made their way into this fork. Its main repository now lies dormant since Oct '07. It is not quite obvious if the original changes in erlang-mysql-driver had reached their intended final form. The third layer module, mysql.erl, has started to become entangled with the second, mysql_conn.erl. Docs beyond the source comments remained lacking. Then in Feb '10, Dave Smith started making some [updates][15] and put them on github, were the driver is now enjoying a couple of [active forks][16] that make for a convincing case in favor of the github Network graph.

* **Emysql** was started from scratch in 2009 by [Jacob Vorreuter][jv] and [Bill Warnecke][bw] at Electronic Arts. They decided to rewrite the erlang-mysql-driver code because they felt that, in Jacob's words, it had been touched by so many people that it had become more complicated than necessary. They abandoned the separation into three process layers and pulled the socket handling and bit-parsing into one module (`emysql_recv'), coupling the functionality by direct function calls. They borrowed some lines from Magnus but generally created a new, more straight forward, architecture based on pooled connections. According to Jacob, Emysql is pretty stable and ran without issue in a production environment at Electronic Arts. This fork is a continuation of [their work][1], including all their commits and adding [documentation][docs], [samples], [fixes][] and extensions. [Vitaliy Batichko][vb] and [Chris Rempel][cr] have contributed updates to this branch. Thank you!

Jacob and Fredrik helped sheding light on the matter, thanks for taking the time! The archeology work was conducted by [Henning Diedrich][hd].


### Links and References

[1]: http://github.com/JacobVorreuter/emysql "emysql"  
[2]: http://github.com/dizzyd/erlang-mysql-driver "erlang-mysql-driver"   
[3]: http://www.kth.se/ "Royal Institure of Technology"   
[4]: https://github.com/fredrikt/yxa/tree/master/src/mysql "Yxa mysql driver"   
[5]: http://www.stacken.kth.se/project/yxa/index.html "Yxa Home"   
[6]: https://github.com/fredrikt/yxa "Yxa repository at github"   
[7]: http://svn.process-one.net/ejabberd-modules/mysql/trunk/   
    "ejabberd mysql driver"  
[8]: https://support.process-one.net "Process One Home"  
[9]: http://www.process-one.net/en/ejabberd/ "ejabberd Home"  
[10]: https://github.com/processone/ejabberd/ "ejabberd repository at github"  
[11]: https://support.process-one.net/doc/display/CONTRIBS/Yxa   
     "ejabberd MySQL 4.1. patch"  
[12]: https://github.com/Eonblast/Emysql/tree/master/doc/diff-ejabberd-yxa.txt  
     "Diff of Yxa and ejabberd mysql drivers"  
[13]: https://github.com/Eonblast/Emysql/tree/master/doc/diff-ejabberd-yxa-2.txt  
     "Diff of Yxa and ejabberd mysql drivers ignoring comment changes"  
[14]: http://code.google.com/p/erlang-mysql-driver/   
     "original erlang-mysql-driver"  
[15]: http://github.com/dizzyd/erlang-mysql-driver   
     "Dave Smith's erlang-mysql-driver at github, currently not maintained"  
[16]: https://github.com/dizzyd/erlang-mysql-driver/network   
     "Fork graph of erlang-mysql-driver at github"  
[17]: http://www.stacken.kth.se/projekt/yxa/mysql-0.1.tar.gz
      "Earliest Yxa mysql driver"
[18]: http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
      "MySQL protocol"
      
[ma]: ahltorp@nada.kth.se               "Magnus Ahltorp"  
[fr]: ft@it.su.se                       "Fredrik Thulin"
[mr]: mickael.remond@process-one.net    "Mickael Remond"  
[ys]: http://yarivsblog.blogspot.com    "Yariv Sadan"  
[ds]: dizzyd@dizzyd.com                 "Dave Smith"
[bw]: bill@rupture.com                  "Bill Warnecke"  
[jv]: https://github.com/JacobVorreuter "Jacob Vorreuter"  
[hd]: hd2010@eonblast.com               "Henning Diedrich"  
[vb]: https://github.com/bva            "Vitaliy Batichko"  
[cr]: https://github.com/csrl           "Chris Rempel"  

[emysql]:   https://github.com/Eonblast/Emysql  
           "Emysql Repository"  
[fixes]:   https://github.com/Eonblast/Emysql/issues/closed  
           "Emysql fixes"  
[updates]: https://github.com/Eonblast/Emysql/commits/master
           "Emysql updates"
[docs]:    http://eonblast.github.com/Emysql/  
           "Emysql online docs"  

## Links                                                    <a name=links></a>

* [Emysql on Github](http://github.com/Eonblast/Emysql)
* [Original Yxa mysql driver](https://github.com/fredrikt/yxa/tree/master/src/mysql)
* [ejabberd fork](http://svn.process-one.net/ejabberd-modules/mysql/trunk/)
* ['erlang-mysql-driver'](http://code.google.com/p/erlang-mysql-driver/)
* [Dave Smith's erlang-mysql-driver fork](http://github.com/dizzyd/erlang-mysql-driver)
* [A maintained(+) erlang-mysql-driver](https://github.com/JoelPM/erlang-mysql-driver)  fork
* [Another maintained(+) erlang-mysql-driver](https://github.com/chernomor/erlang-mysql-driver)  fork
* [MySQL Client Server Protocol](http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol)
* [MySQL 5.5 Source](ftp://ftp.fu-berlin.de/unix/databases/mysql/Downloads/MySQL-5.5/mysql-5.5.8.tar.gz)

 (+)maintained at the time of writing, Feb 2011.



## TODO                                                     <a name=todo></a>
* decrementing pool size could close sockets that are in use
* spawn individual conn_mgr gen_server processes for each pool
* allow row results to be returned as binary

## License                                                  <a name=license></a>

Copyright (c) 2009-2011
Bill Warnecke <bill@rupture.com>,
Jacob Vorreuter <jacob.vorreuter@gmail.com>,
Henning Diedrich <hd2010@eonblast.com>,
Eonblast Corporation [http://www.eonblast.com].

Permission is  hereby  granted,  free of charge,  to any person
obtaining  a copy of this software and associated documentation
files  (the  "Software"),  to  deal  in  the  Software  without 
restriction,  including  without limitation  the rights to use,
copy, modify,  merge,  publish, distribute,  sublicense, and/or 
sell  copies of the  Software,  and to permit  persons  to whom
the  Software  is furnished to do so,  subject to the following 
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

[choosing]: #choosing
[history]: #history
[samples]: #samples
[usage]:   #usage
[links]:   #links
[todo]:    #todo
[license]: #license