---
title: "Executing Commands"
slug: "executing-commands"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Stored Procedures
## Simple usage ##

Dapper fully supports stored procs:

    var user = conn.Query<User>("spGetUser", new { Id = 1 }, 
                                commandType: CommandType.StoredProcedure)
               .SingleOrDefault();

## Input, Output and Return parameters ##
If you want something more fancy, you can do:

    var p = new DynamicParameters();
    p.Add("@a", 11);
    p.Add("@b", 
          dbType: DbType.Int32, 
          direction: ParameterDirection.Output);
    p.Add("@c", 
          dbType: DbType.Int32, 
          direction: ParameterDirection.ReturnValue);
    
    conn.Execute("spMagicProc", p, 
                 commandType: CommandType.StoredProcedure); 
    
    var b = p.Get<int>("@b");
    var c = p.Get<int>("@c"); 

## Table Valued Parameters ##
If you have a stored procedure that accepts a Table Valued Parameter, you need to pass a DataTable which has the same structure as the table type in SQL Server has.
Here's a definition for a table type and procedure utilizing it:

    CREATE TYPE [dbo].[myUDTT] AS TABLE([i1] [int] NOT NULL);
    GO
    CREATE PROCEDURE myProc(@data dbo.myUDTT readonly) AS
    SELECT i1 FROM @data;
    GO
    /*
    -- optionally grant permissions as needed, depending on the user you execute this with.
    -- Especially the GRANT EXECUTE ON TYPE is often overlooked and can cause problems if omitted.
    GRANT EXECUTE ON TYPE::[dbo].[myUDTT] TO [user];
    GRANT EXECUTE ON dbo.myProc TO [user];
    GO
    */
  To call that procedure from within c#, you need to do the following:

    // Build a DataTable with one int column
    DataTable data = new DataTable();
    data.Columns.Add("i1", typeof(int));
    // Add two rows
    data.Rows.Add(1);
    data.Rows.Add(2);

    var q = conn.Query("myProc", new {data}, commandType: CommandType.StoredProcedure);


## Execute a command that returns no results
    IDBConnection db = /* ... */  
    var id = /* ... */

    db.Execute(@"update dbo.Dogs set Name = 'Beowoof' where Id = @id",
       new { id });

