---
title: "Executando Comandos"
slug: "executando-comandos"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Procedimentos armazenados
## Uso simples ##

Dapper suporta totalmente procs armazenados:

    var user = conn.Query<User>("spGetUser", new { Id = 1 }, 
                                commandType: CommandType.StoredProcedure)
               .SingleOrDefault();

## Parâmetros de entrada, saída e retorno ##
Se você quiser algo mais sofisticado, você pode fazer:

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

## Parâmetros com valor de tabela ##
Se você tiver um procedimento armazenado que aceita um parâmetro com valor de tabela, precisará passar um DataTable que tenha a mesma estrutura que o tipo de tabela no SQL Server.
Aqui está uma definição para um tipo de tabela e procedimento que o utiliza:

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
Para chamar esse procedimento de dentro do c#, você precisa fazer o seguinte:

    // Build a DataTable with one int column
    DataTable data = new DataTable();
    data.Columns.Add("i1", typeof(int));
    // Add two rows
    data.Rows.Add(1);
    data.Rows.Add(2);

    var q = conn.Query("myProc", new {data}, commandType: CommandType.StoredProcedure);


## Executa um comando que não retorna nenhum resultado
    IDBConnection db = /* ... */  
    var id = /* ... */

    db.Execute(@"update dbo.Dogs set Name = 'Beowoof' where Id = @id",
       new { id });

