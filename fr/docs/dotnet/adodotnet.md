---
title: "ADO.NET"
slug: "adonet"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

ADO(ActiveX Data Objects).Net est un outil fourni par Microsoft qui permet d'accéder à des sources de données telles que SQL Server, Oracle et XML via ses composants. Les applications frontales .Net peuvent récupérer, créer et manipuler des données une fois qu'elles sont connectées à une source de données via ADO.Net avec les privilèges appropriés.

ADO.Net fournit une architecture sans connexion. Il s'agit d'une approche sécurisée pour interagir avec une base de données, car la connexion n'a pas besoin d'être maintenue pendant toute la session.

** Une note sur le paramétrage des SQL avec `Parameters.AddWithValue` :** `AddWithValue` n'est jamais un bon point de départ. Cette méthode repose sur la déduction du type de données à partir de ce qui est transmis. Avec cela, vous pourriez vous retrouver dans une situation où la conversion empêche votre requête [d'utiliser un index] (http://stackoverflow.com/q/799584 /87698). Notez que certains types de données SQL Server, tels que `char`/`varchar` (sans "n" précédant) ou `date` n'ont pas de type de données .NET correspondant. Dans ces cas, [`Add` avec le type de données correct doit être utilisé à la place](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already /).

## Meilleures pratiques - Exécution d'instructions SQL
    public void SaveNewEmployee(Employee newEmployee)
    {
        // best practice - wrap all database connections in a using block so they are always closed & disposed even in the event of an Exception
        // best practice - retrieve the connection string by name from the app.config or web.config (depending on the application type) (note, this requires an assembly reference to System.configuration)
        using(SqlConnection con = new SqlConnection(System.Configuration.ConfigurationManager.ConnectionStrings["MyConnectionName"].ConnectionString))
        {
            // best practice - use column names in your INSERT statement so you are not dependent on the sql schema column order
            // best practice - always use parameters to avoid sql injection attacks and errors if malformed text is used like including a single quote which is the sql equivalent of escaping or starting a string (varchar/nvarchar)
            // best practice - give your parameters meaningful names just like you do variables in your code
            using(SqlCommand sc = new SqlCommand("INSERT INTO employee (FirstName, LastName, DateOfBirth /*etc*/) VALUES (@firstName, @lastName, @dateOfBirth /*etc*/)", con))
            {
                // best practice - always specify the database data type of the column you are using
                // best practice - check for valid values in your code and/or use a database constraint, if inserting NULL then use System.DbNull.Value
                sc.Parameters.Add(new SqlParameter("@firstName", SqlDbType.VarChar, 200){Value = newEmployee.FirstName ?? (object) System.DBNull.Value});
                sc.Parameters.Add(new SqlParameter("@lastName", SqlDbType.VarChar, 200){Value = newEmployee.LastName ?? (object) System.DBNull.Value});
    
                // best practice - always use the correct types when specifying your parameters, Value is assigned to a DateTime instance and not a string representation of a Date
                sc.Parameters.Add(new SqlParameter("@dateOfBirth", SqlDbType.Date){ Value = newEmployee.DateOfBirth });
    
                // best practice - open your connection as late as possible unless you need to verify that the database connection is valid and wont fail and the proceeding code execution takes a long time (not the case here)
                con.Open();
                sc.ExecuteNonQuery();
            }
    
            // the end of the using block will close and dispose the SqlConnection
            // best practice - end the using block as soon as possible to release the database connection
        }
    }

    // supporting class used as parameter for example
    public class Employee
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public DateTime DateOfBirth { get; set; }
    }



----------

## Meilleures pratiques pour travailler avec [ADO.NET][1]
* La règle d'or consiste à ouvrir la connexion pendant un minimum de temps. Fermez la connexion explicitement une fois l'exécution de votre procédure terminée, cela renverra l'objet de connexion au pool de connexions. Taille maximale du pool de connexions par défaut = 100. Comme le pool de connexions améliore les performances de la connexion physique à SQL Server. [Regroupement de connexions dans SQL Server] [2]
* Enveloppez toutes les connexions à la base de données dans un bloc using afin qu'elles soient toujours fermées et supprimées même en cas d'exception. Voir [using Statement (C# Reference)][3] pour plus d'informations sur l'utilisation des instructions
* Récupérer les chaînes de connexion par nom à partir de app.config ou web.config (selon le type d'application)
* Cela nécessite une référence d'assembly à `System.configuration`
* Voir [Chaînes de connexion et fichiers de configuration][4] pour plus d'informations sur la façon de structurer votre fichier de configuration
* Utilisez toujours des paramètres pour les valeurs entrantes
* Éviter les attaques [injection sql][5]
* Évitez les erreurs si du texte malformé est utilisé, comme l'inclusion d'un guillemet simple qui est l'équivalent sql de l'échappement ou du démarrage d'une chaîne (varchar/nvarchar)
* Laisser le fournisseur de base de données réutiliser les plans de requête (non pris en charge par tous les fournisseurs de base de données), ce qui augmente l'efficacité
* Lorsque vous travaillez avec des paramètres
* La non-concordance entre le type et la taille des paramètres SQL est une cause fréquente d'échec d'insertion/mise à jour/sélection
* Donnez à vos paramètres Sql des noms significatifs, tout comme vous faites des variables dans votre code
* Spécifiez le type de données de base de données de la colonne que vous utilisez, cela garantit que les mauvais types de paramètres ne sont pas utilisés, ce qui pourrait entraîner des résultats inattendus
* Validez vos paramètres entrants avant de les transmettre à la commande (comme le dit le dicton, "[garbage in, garbage out](https://en.wikipedia.org/wiki/Garbage_in,_garbage_out)"). Valider les valeurs entrantes le plus tôt possible dans la pile
* Utilisez les types corrects lors de l'attribution de vos valeurs de paramètre, exemple : n'attribuez pas la valeur de chaîne d'un DateTime, attribuez plutôt une instance DateTime réelle à la valeur du paramètre
* Spécifiez la [taille](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparameter.size(v=vs.110).aspx) des paramètres de type chaîne. En effet, SQL Server peut réutiliser les plans d'exécution si les paramètres correspondent en type *et* en taille. Utilisez -1 pour MAX
* N'utilisez pas la méthode [AddWithValue][6], la raison principale est qu'il est très facile d'oublier de spécifier le type de paramètre ou la précision/échelle si nécessaire. Pour plus d'informations, voir [Pouvons-nous déjà arrêter d'utiliser AddWithValue ?] [7]
* Lors de l'utilisation de connexions à la base de données
* Ouvrez la connexion le plus tard possible et fermez-la dès que possible. Il s'agit d'une directive générale lorsque vous travaillez avec une ressource externe
* Ne partagez jamais les instances de connexion à la base de données (exemple : avoir un singleton hébergeant une instance partagée de type `SqlConnection`). Demandez à votre code de toujours créer une nouvelle instance de connexion à la base de données lorsque cela est nécessaire, puis demandez au code appelant de s'en débarrasser et de le "jeter" une fois terminé. La raison en est
    * Most database providers have some sort of connection pooling so creating new managed connections is cheap
    * It eliminates any future errors if the code starts working with multiple threads


[1] : https://msdn.microsoft.com/en-us/library/h43ks021(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/8xx3tyca(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/yh598w02.aspx
[4] : https://msdn.microsoft.com/en-us/library/ms254494(v=vs.110).aspx
[5] : https://en.wikipedia.org/wiki/SQL_injection
[6] : https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparametercollection.addwithvalue(v=vs.110).aspx
[7] : http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already/

## Exécution d'instructions SQL en tant que commande
    // Uses Windows authentication. Replace the Trusted_Connection parameter with
    // User Id=...;Password=...; to use SQL Server authentication instead. You may
    // want to find the appropriate connection string for your server.
    string connectionString = @"Server=myServer\myInstance;Database=myDataBase;Trusted_Connection=True;"

    string sql = "INSERT INTO myTable (myDateTimeField, myIntField) " +
        "VALUES (@someDateTime, @someInt);";

    // Most ADO.NET objects are disposable and, thus, require the using keyword.
    using (var connection = new SqlConnection(connectionString))
    using (var command = new SqlCommand(sql, connection))
    {
        // Use parameters instead of string concatenation to add user-supplied
        // values to avoid SQL injection and formatting issues. Explicitly supply datatype.

        // System.Data.SqlDbType is an enumeration. See Note1
        command.Parameters.Add("@someDateTime", SqlDbType.DateTime).Value = myDateTimeVariable;
        command.Parameters.Add("@someInt", SqlDbType.Int).Value = myInt32Variable;

        // Execute the SQL statement. Use ExecuteScalar and ExecuteReader instead
        // for query that return results (or see the more specific examples, once
        // those have been added).

        connection.Open();
        command.ExecuteNonQuery();
    }

***Remarque 1 :*** Veuillez consulter [SqlDbType Enumeration][1] pour la variation spécifique à MSFT SQL Server.

***Remarque 2 :*** Veuillez consulter [MySqlDbType Enumeration][2] pour la variation spécifique à MySQL.


[1] : https://msdn.microsoft.com/en-us/library/system.data.sqldbtype(v=vs.110).aspx
[2] : https://dev.mysql.com/doc/dev/connector-net/html/T_MySql_Data_MySqlClient_MySqlDbType.htm

## Utilisation d'interfaces communes pour extraire les classes spécifiques aux fournisseurs
    var providerName = "System.Data.SqlClient";    //Oracle.ManagedDataAccess.Client, IBM.Data.DB2
    var connectionString = "{your-connection-string}";
    //you will probably get the above two values in the ConnectionStringSettings object from .config file

    var factory = DbProviderFactories.GetFactory(providerName);
    using(var connection = factory.CreateConnection()) {    //IDbConnection
        connection.ConnectionString = connectionString;
        connection.Open();
        
        using(var command = connection.CreateCommand()) {    //IDbCommand
            command.CommandText = "{query}";
            
            using(var reader = command.ExecuteReader()) {    //IDataReader
                while(reader.Read()) {
                    ...
                }
            }
        }
    }

