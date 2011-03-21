# This fork is no longer maintained. Active development has moved to:
<https://github.com/Eonblast/Emysql>

## Usage

### Start the application

	application:start(emysql).
	
### Add a pool
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
	
### Record Types
	-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
	-record(error_packet, {seq_num, code, msg}).
	-record(result_packet, {seq_num, field_list, rows, extra}).

### Executing SQL statements
emysql:execute(PoolName, Statement) -> result\_packet() | ok\_packet() | error\_packet()  
PoolName = atom()  
Statement = string() | binary()  	

	emysql:execute(mypoolname, <<"SELECT * from mytable">>).
	#result_packet{field_list=[...], rows=[...]}
	
	emysql:execute(mypoolname, <<"UPDATE mytable SET bar = 'baz' WHERE id = 1">>).
	#ok_packet{affected_rows=1}
	
### Executing prepared statements
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

### Converting Row Data To Records
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

### TODO:
* decrementing pool size could close sockets that are in use
* spawn individual conn\_mgr gen\_server processes for each pool
* allow row results to be returned as binary
