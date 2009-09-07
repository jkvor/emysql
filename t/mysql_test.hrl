-define(DROP_TABLES(PoolId), begin
	ShowTables = mysql:execute(PoolId, "SHOW TABLES"),
	[mysql:execute(PoolId, "DROP TABLE " ++ Table) || [Table] <- ShowTables:rows()]
end).