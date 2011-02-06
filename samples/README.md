# Emysql Samples

Sample programs are in ./samples. 

* [a_hello](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello.erl) - Hello World
* [b_raw](http://github.com/Eonblast/Emysql/blob/master/samples/b_raw.erl) - Hello World, raw output
* [c\_rows\_as\_records](http://github.com/Eonblast/Emysql/blob/master/samples/c_rows_as_records.erl) - Using Erlang records to access result rows
* [d\_prepared\_statement](http://github.com/Eonblast/Emysql/blob/master/samples/d_prepared_statement.erl) - Using prepared statements
* [e\_stored\_procedure](http://github.com/Eonblast/Emysql/blob/master/samples/e_stored_procedure.erl) - Using stored procedures

## Running Samples

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

#### 3. Running the Samples

	$ cd samples
	$ ./a_hello
	$ ./b_raw
	$ ./c_rows_as_records
	$ ./d_prepared_statement
	$ ./e_stored_procedure
	
or make emysql.app and the database, as explained above, and start a_hello etc. manually along these lines:

	$ make
	$ cd samples
	$ erlc a_hello.erl
	$ erl -pa ../ebin -s a_hello run -s init stop -noshell
