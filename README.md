## Emysql 0.2.14

<hr/>
**Please note: this commit of Apr '12 has incompatible changes for automatic UTF-8 conversion vs the Dec '11 commits.**

There are cases where the automatic conversion of parameters to prepared
statements has been changed and now behaves different than before. The
conversion now respects the encoding setting of the connection pool that it
is for, which you assigned when opening the pool.

There are extensive new tests checking as many sensible cases as possible, among
them 40+ 'human readable' cases in the files test/utf8_SUITE, test/latin_SUITE,
test/utf8_to_latindb_SUITE, test/latin_to_utf8db_SUITE. Please refer to these
first in cases where you have doubt about expected behavior in fringe cases.

<hr/>

This is an Erlang MySQL driver, based on a rewrite at Electronic Arts. [Easy][Samples] to use, strong [connection pooling][Adding_a_Pool], [prepared statements][Executing_Prepared_Statements] & [stored procedures][Executing_Stored_Procedures]. Optimized for a central node architecture and OLTP.

While you can use mysql via ODBC, you should see better performance when using a *driver* like Emysql. For [samples][Samples] and [docs][] see below. Read the brief on [choosing][Choosing] a package and about the [history][History] of the various MySQL drivers.

[Emysql][1] is a cleaner rewrite of [erlang-mysql-driver][2], see [History][]. This fork is a direct continuation of the original [emysql][1] with [fixes][], [updates][], more [documentation][docs] and [samples][Samples]. 

**This is the master branch. Should you run into problems, please report them by opening an issue at github and try if they go away by checking out the 'stable' branch. Thank you.**

<hr/>

 **Which fork/package should I use?** Likely this one, but see [Choosing][].  
 **Why are there so many?** See [History][].  
 **Who used *this* fork?** [Electronic Arts][History].  
 **How do I ...?** See [Samples][].  
 **Hello ...?** See [Samples][].  

 **Download:** <https://github.com/Eonblast/Emysql/archives/master>  
 **Docs:** <http://eonblast.github.com/Emysql/>  
 **Issues:** <https://github.com/Eonblast/Emysql/issues>  
 **Repository:** <https://github.com/Eonblast/Emysql>  

<hr/>

## Contents

* [Choosing][]
* [Samples][]
* [Usage][]
* [Tests][]
* [History][]
* [Links][]
* [Todo][]
* [License][]

<hr/>

## Choosing                                             <a name="Choosing"></a>

#### Best
In most cases, especially for high performance and stability, this package, [Emysql][emysql], will be the best choice. It was rewritten from the ground up to overcome fundamental issues of 'erlang-mysql-driver'. It also has some usable docs meanwhile. 

#### Simple
If you are looking for the **plain necessities**, you should use the [ejabberd][7] mysql driver. It is simple, battle tested and stable. There are comprehensive instructions in the source comments.

#### Transaction
For **mnesia-style transactions**, one of the multiple '[erlang-mysql-driver][22]s' may suite you best. There are [quite many][16] branches of it out there, and they are based on the same project as the ejabberd driver. To learn more about out the differences between the drivers, see the [mysql driver history][History].

## Getting Emysql

	$ git clone git://github.com/Eonblast/Emysql.git Emysql


## Samples                                             <a name=Samples></a>

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


We'll be coming back to this source to make it run on your machine in a minute. But let's look at the basic building blocks first:

### Executing an SQL Statement

	emysql:execute(my_pool, <<"SELECT * from mytable">>).

For the exact spec, see below, [Usage][]. Regarding the 'pool', also see below.

### Executing a Prepared Statement

	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	
	emysql:execute(my_pool, my_stmt, [1]).

### Executing Stored Procedures

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

Or use rebar: 

	$ cd Emysql
	$ ./rebar compile

Both yield an option to install but this is not needed for the samples.

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

That's it. If you need to blindly repeat that more often some time, you can also use

	$ make hello

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

## Usage                                                    <a name="Usage"></a>

General Notes on using Emysql, including the actual specs:

#### Starting an Application

The Emysql driver is an Erlang gen-server, and, application.

	crypto:start(),
	application:start(emysql).

#### Adding a Pool                                  <a name="Adding_a_Pool"></a>

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

#### Executing Prepared Statements                          <a name="Executing_Prepared_Statements"></a>

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

#### Executing Stored Procedures                          <a name="Executing_Stored_Procedures"></a>

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


## Tests                                             <a name="Tests"></a>

**Please add a Common Test suite if you are proposing a pull request!**

### Basic Tests

Some Common Tests (Unit Tests) have been added in the `test` folder. They have
no significant coverage yet but can help to test the basics. They might also 
help you find trip ups in your system set up (environment and basics suites). 

For the basic tests you only need the test database set up and a mysql server running, the same as described above for the samples:

	$ mysql [-u<user> -p]
	mysql> create database hello_database;
	mysql> use hello_database;
	mysql> create table hello_table (hello_text char(20));
	mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';

To run the basic tests, at the command line type:

	make test

Some tests can take up to half a minute to finish on a slow machine.

These tests currently check access to the database (environment suite) the same functionality as the samples (basics suite) and for race conditions as described in issue #9. Thank you, Ransom!

### Encoding Tests

Currently the main focus is on Unicode test cases and encoding conversions,
in the suites utf8_SUITE, latin_SUITE, utf8_to_latindb_SUITE,
latin_to_utf8db_SUITE. Especially the silent conversions of list strings
to the appropriate binary format were a bit of a challenge. 

For the encoding tests, please create these databases:

	  create database hello_utf8_database character set utf8;
	  use hello_utf8_database;
	  create table hello_table (hello_text char(20));
	  grant all privileges on hello_utf8_database.* to hello_username@localhost identified by 'hello_password';
	  
	  create database hello_latin1_database character set latin1;
	  use hello_latin1_database;
	  create table hello_table (hello_text char(20));
	  grant all privileges on hello_latin1_database.* to hello_username@localhost identified by 'hello_password';

To run the encoding tests do:

	make encoding-test
	
To run all tests (this includes issue tests, see below):

	make all-test
	

You see the test results when opening test/index.html with a browser. It should look like this:

<div style="border: 2px solid black; margin: 10px; font-size: 0.6em;">
<CENTER>
<H3>Test Results</H3>
</CENTER>
<BR />
<CENTER>
<A HREF="#">All test runs in "test"</A>
<br /><br />
<TABLE border="3" cellpadding="5" BGCOLOR="#E4F0FE" style="font-size: 0.7em;">
<tr>
<th>Test Name</th>
<th>Label</th>
<th>Test Run Started</th>
<th><font color="#E4F0FE">_</font>Ok<font color="#E4F0FE">_</font></th>
<th>Failed</th>
<th>Skipped<br />(User/Auto)</th>

<th>Missing<br />Suites</th>
<th>Node</th>
</tr>

<TR valign="top">
<TD><FONT SIZE="-1"><A HREF="">me.Emysql. basics_SUITE</A></FONT></TD>
<TD ALIGN="center"><FONT SIZE="-1"><B>-</B></FONT></TD>
<TD><FONT SIZE="-1">Tue Dec 13 2011 04:17:29</FONT></TD>

<TD ALIGN="right">7</TD>
<TD ALIGN="right">0</TD>
<TD ALIGN="right">0 (0/0)</TD>
<TD ALIGN="right">0</TD>
<TD ALIGN="right"><FONT SIZE="-1">ct@machine</FONT></TD>

</TR>
<TR valign="top">
<TD><FONT SIZE="-1"><A HREF="">me.Emysql. environment_SUITE</A></FONT></TD>

<TD ALIGN="center"><FONT SIZE="-1"><B>-</B></FONT></TD>
<TD><FONT SIZE="-1">Tue Dec 13 2011 04:17:29</FONT></TD>
<TD ALIGN="right">6</TD>
<TD ALIGN="right">0</TD>
<TD ALIGN="right">0 (0/0)</TD>
<TD ALIGN="right">0</TD>
<TD ALIGN="right"><FONT SIZE="-1">ct@machine</FONT></TD>

</TR>
<TR valign="top">
<TD><B>Total</B></TD><TD> </TD>
<TD> </TD>
<TD ALIGN="right"><B>13</B></TD>
<TD ALIGN="right"><B>0</B></TD>
<TD ALIGN="right">0 (0/0)</TD>
<TD ALIGN="right"><B>0</B></TD>
<TD ></TD>
</TR>
</TABLE>

</CENTER>
<P /><CENTER>
<BR /><BR />
<HR />
<P /><FONT SIZE="-1">
Copyright (C) 2011 <A HREF="">Open Telecom Platform</A><BR />
Updated: Tue Dec 13 2011 04:17:36<BR/>
</FONT>
</CENTER>
</div>

### Issue Tests
#### Issue 20
There is a test to check on issue #20. For this test you need 
two databases like this:

	$ mysql [-u<user> -p]
	mysql> create database test1;
	mysql> create database test2;
	mysql> grant all privileges on test1.* to test@localhost identified by 'test';
	mysql> grant all privileges on test2.* to test@localhost identified by 'test';
	mysql> use test1;
	mysql> CREATE TABLE `test` ( `a` int(11) NOT NULL );
	mysql> use test2;
	mysql> CREATE TABLE `test` ( `b` int(11) NOT NULL );

The test suite is test/pool_SUITE.erl. To run the test, use make:

	make test20
	
Check the test results by opening test/index.html with a browser. 

#### Issue 9
The probably rare but still bad races of issue #9 have been solved at long last, courtesy of Ransom Richardson. They are tested for as part of the standard test. You can also use:

	make test9
	
Check the test results by opening test/index.html with a browser. 


## History                                               <a name="History"></a>

Open Source Erlang MySQL driver efforts are a fractured matter. You may find yourself digging in the sources to find out about their relationships with each other - and which one to pick. Here is a brief history.

**Yxa:** The first Erlang MySQL driver, in [~270 lines][17] of code, seems to have been written between 2001 and 2004 by [Magnus Ahltorp][ma] at the Swedish [Royal Institute of Technology][3]. It exposes low level, blocking functions to talk 4.0 protocol with a MySQL server. In 2005 [Fredrik Thulin][fr] brought the driver into its current modular form to use it for the the SIP proxy [Yxa][5] while working at the [Stockholm University][19]. It has three process layers: a high level, round-robin connection pooling module; then, middle-man, single-connection, blocking processes that do the bit-level wrangling with the [MySQL protocol][18]. This module, mysql_conn, can also be used as a single-connection, stand-alone driver. And below this, there are small, protocol-agnostic receiver processes that handle the socket communication with the server, no matter the contents. Fredrik implemented gen-server behavior, added logging, error messages, upgraded authentication, and thoroughly commented the source. This [mysql driver][4] is working, complete and stable since at least 2007, it is available as part of [Yxa 1.0][5] (hosted on [github][6]). It has no support for transactions or stored procedures. It is the basis for the following two packages. Its basic modular division and general functionality were not changed but only enhanced and it had originally been agreed upon that the Yxa branch should receive and merge the contributions of the later forks upstream. Unfortunately, that did not come to pass.


**ejabberd:** In 2006, a [fork][7] of the Yxa driver was created by [Mickael Remond][mr] at [Process One][8] to become part of the successful instant messaging server [ejabberd][9] (also hosted on [github][10]). It can be assumed to be as stable as the Yxa branch, didn't change a byte in the lowest level, but only slightly enhanced the higher level. The difference from the original Yxa branch consists mainly of added inspection functions that help using the query results, and of an [independent adoption][11] of the MySQL 4.1 client-server protocol. The original Yxa branch has meanwhile adopted EDoc comment format, which makes the sources look more different than they actually are. You can find a Jan 2011 diff between Yxa and the ejabberd version [here][12], and one ignoring comments [here][13]. These two branches could be merged quite easily, probably without any change in functionality at all.

**erlang-mysql-driver:** The storied life of this branch began in 2006 when [Yariv Sadan][ys] created a fork from the ejabberd branch, made it a standalone project, gave it the maximally generic name that stuck, and hosted it on [Google Code][15]. Before he moved on to work at Facebook, he added high-level handling of prepared statements and transactions, and at long last completed some loose ends with the connection pooling that had been known to be lagging since the Yxa version. There were changes both in the original Yxa and the ejabberd branch after the forking off that never made their way into this fork, but in all likelihood they must be minor. It is not always obvious if the changes in erlang-mysql-driver had reached their intended final form. The separation of the three process layers seems to have suffered and complicated enhancements as the highest layer module, mysql.erl, started to become entangled with the second, mysql_conn.erl. Maybe that had a part in why the upstream merge never happened. The main repository of this fork lay dormant since Oct '07 when in Feb '10, Dave Smith, the rebar guy, started making some [updates][15] and put them on github. The driver is now enjoying a couple of active [forks][16] that make a convincing case for the github Network graphs.

A [parallel][20] fork from Yariv's branch, not entangled with Dave's tree, is [the one][22] by [Nick Gerakines][ng]. I suspect it could technically be the strongest of the erlang-mysql-driver forks, with a lot of clean up work by smart guys put in, although it is generally less well known. And much less forked. In the end, the branch was abandoned for [Emysql][]. In all branches, documentation beyond the source comments remains lacking.

**Emysql** was created from scratch in 2009, specifically to achieve better stability and throughput. It was proposed and written by [Jacob Vorreuter][jv] at Electronic Arts and deployed at Shawn Fanning's Rupture.com, a social network site for gamers. Initially, [Nick Gerakines][ng], Jacob's boss at EA, rewrote large parts of erlang-mysql-server to [clean it up][21]. But some fundamental problems remained and when half a year in, they still could not overcome performance and stability issues, Nick gave Jacob the green light to rewrite it from the ground up because they felt that, in Jacob's words, the Yxa branch had been touched by so many people that it had become more complicated than necessary. According to Jacob, [Bill Warnecke][bw] helped in the early design and testing. They abandoned the separation into three process layers and pulled the socket handling and bit-parsing into one module, coupling the functionality into direct function calls. It looks like they borrowed some chore lines from Magnus but generally created a new, straightforward architecture focused on providing a high performance node. Not only can Emysql open multiple connections, but multiple pools of multiple connections each to multiple database servers, which makes for a strong central OLTP node. Jacob says that Emysql is pretty stable and ran without issues in production at EA. Nick remembers: "The primary way we used it was to have a single Erlang node be the MySQL communication point and allow a number of connected nodes to connect through it to MySQL. We wanted very high throughput with many pids across a grid connecting to it and needed the ability to have a number of MySQL connections open for connection pooling." Rupture was killed in the consolidations of 2009. But Shawn could probably keep the money and we the fond memory of Napster and now, the glistening Emysql.

**Eonblast Emysql** is a continuation fork of [Jacob's work][1], including all his commits and adding [docs][], [samples][], [fixes][] and [extensions][24]. [Henning Diedrich][hd], [Vitaliy Batichko][vb], [Chris Rempel][cr], [Patrick Atambo][pa], [Joel Meyer][jm], [Erik Seres][es], [Alexey Lebedeff][al], [Logan Owen][lo], [Seven Du][sd], [Brendon Hogger][bh], [Bart van Deenen][bd] and [Ransom Richardson][rr] have contributed to this branch. Support for stored procedures has been added, remaining issues are being addressed and there is work going on to bring Mnesia-style transactions. The fork is still close to the original, which currently lies dormant, but has started to add features and possibly increased stability.

Fredrik, Nick and Jacob helped shedding light on the matter. Thank you very much! Errors and omissions are [mine][hd]. Please let me know about any errors you may spot. Thanks.


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
[19]:  http://www.su.se/english/
      "Stockholm University"
[20]: https://github.com/ngerakines/erlang_mysql/network
      "Network graph of Nick's erlang-mysql-driver fork"
[21]: https://github.com/ngerakines/erlang_mysql/commits/master
      "Commits in Nick Gerakines' erlang-mysql-driver fork"
[22]: https://github.com/ngerakines/erlang_mysql
      "Nick Gerakines' erlang-mysql-driver fork"
[23]: http://method.com/detail/CaseStudy/65
      "About Rupture"
[24]: https://github.com/Eonblast/Emysql/commits/master
      "Commits in the current Eonblast Emysql branch"

      
[ma]: ahltorp@nada.kth.se               "Magnus Ahltorp"  
[fr]: ft@it.su.se                       "Fredrik Thulin"
[mr]: mickael.remond@process-one.net    "Mickael Remond"  
[ys]: http://yarivsblog.blogspot.com    "Yariv Sadan"  
[ds]: dizzyd@dizzyd.com                 "Dave Smith"
[ng]: https://github.com/ngerakines     "Nick Gerakines"
[jv]: https://github.com/JacobVorreuter "Jacob Vorreuter"  
[bw]: bill@rupture.com                  "Bill Warnecke"  
[hd]: hd2010@eonblast.com               "Henning Diedrich"  
[vb]: https://github.com/bva            "Vitaliy Batichko"  
[cr]: https://github.com/csrl           "Chris Rempel"  
[pa]: partoa@gmail.com                  "Patrick Atambo"
[jm]: joel.meyer@openx.org              "Joel Meyer"
[es]: https://github.com/eseres         "Erik Seres"
[al]: https://github.com/binarin        "Alexey Lebedeff"
[lo]: https://github.com/lsowen         "Logan Owen"
[sd]: https://github.com/seven1240      "Seven Du"
[bh]: brendonh@gmail.com                "Brendon Hogger"
[bd]: https://github.com/bvdeenen       "Bart van Deenen"
[rr]: https://github.com/ransomr        "Ransom Richardson"

[emysql]:   https://github.com/Eonblast/Emysql  
           "Eonblast Emysql Repository"  
[fixes]:   https://github.com/Eonblast/Emysql/issues/closed  
           "Emysql fixes"  
[updates]: https://github.com/Eonblast/Emysql/commits/master
           "Emysql updates"
[docs]:    http://eonblast.github.com/Emysql/  
           "Emysql online docs"  

## Links                                                    <a name=Links></a>

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



## TODO                                                     <a name=Todo></a>
* decrementing pool size could close sockets that are in use
* spawn individual conn_mgr gen_server processes for each pool
* allow row results to be returned as binary


## License                                                  <a name=License></a>

Copyright (c) 2009-2011
Bill Warnecke <bill@rupture.com>,
Jacob Vorreuter <jacob.vorreuter@gmail.com>,
Henning Diedrich <hd2010@eonblast.com>,
Eonblast Corporation <http://www.eonblast.com>.

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

[Choosing]: #Choosing
[History]:  #History
[Samples]:  #Samples
[Usage]:    #Usage
[Tests]:    #Tests
[Links]:    #Links
[Todo]:     #Todo
[License]:  #License
[Executing_Prepared_Statements]: #Executing_Prepared_Statements
[Executing_Stored_Procedures]:   #Executing_Stored_Procedures
[Adding_a_Pool]:                 #Adding_a_Pool