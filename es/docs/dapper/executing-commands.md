---
title: "Ejecutando Comandos"
slug: "ejecutando-comandos"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Procedimientos almacenados
## Uso sencillo ##

Dapper es totalmente compatible con los procesos almacenados:

    var user = conn.Query<User>("spGetUser", new { Id = 1 }, 
                                commandType: CommandType.StoredProcedure)
               .SingleOrDefault();

## Parámetros de entrada, salida y retorno ##
Si quieres algo más elegante, puedes hacer:

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

## Parámetros con valores de tabla ##
Si tiene un procedimiento almacenado que acepta un parámetro de valor de tabla, debe pasar un DataTable que tenga la misma estructura que el tipo de tabla en SQL Server.
Aquí hay una definición para un tipo de tabla y el procedimiento que lo utiliza:

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
Para llamar a ese procedimiento desde C#, debe hacer lo siguiente:

    // Build a DataTable with one int column
    DataTable data = new DataTable();
    data.Columns.Add("i1", typeof(int));
    // Add two rows
    data.Rows.Add(1);
    data.Rows.Add(2);

    var q = conn.Query("myProc", new {data}, commandType: CommandType.StoredProcedure);


## Ejecuta un comando que no arroja resultados
    IDBConnection db = /* ... */  
    var id = /* ... */

    db.Execute(@"update dbo.Dogs set Name = 'Beowoof' where Id = @id",
       new { id });

