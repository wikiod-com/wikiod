---
title: "Exécution de commandes"
slug: "execution-de-commandes"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Procédures stockées
## Utilisation simple ##

Dapper prend entièrement en charge les procs stockés :

    var user = conn.Query<User>("spGetUser", new { Id = 1 }, 
                                commandType: CommandType.StoredProcedure)
               .SingleOrDefault();

## Paramètres d'entrée, de sortie et de retour ##
Si vous voulez quelque chose de plus chic, vous pouvez faire :

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

## Paramètres de valeur de table ##
Si vous avez une procédure stockée qui accepte un paramètre de table, vous devez transmettre un DataTable qui a la même structure que le type de table dans SQL Server.
Voici une définition d'un type de table et d'une procédure l'utilisant :

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
Pour appeler cette procédure depuis C#, vous devez procéder comme suit :

    // Build a DataTable with one int column
    DataTable data = new DataTable();
    data.Columns.Add("i1", typeof(int));
    // Add two rows
    data.Rows.Add(1);
    data.Rows.Add(2);

    var q = conn.Query("myProc", new {data}, commandType: CommandType.StoredProcedure);


## Exécute une commande qui ne renvoie aucun résultat
    IDBConnection db = /* ... */  
    var id = /* ... */

    db.Execute(@"update dbo.Dogs set Name = 'Beowoof' where Id = @id",
       new { id });

