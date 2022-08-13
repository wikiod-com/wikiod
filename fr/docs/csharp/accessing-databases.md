---
title: "Accéder aux bases de données"
slug: "acceder-aux-bases-de-donnees"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Chaînes de connexion
Une chaîne de connexion est une chaîne qui spécifie des informations sur une source de données particulière et comment s'y connecter en stockant des informations d'identification, des emplacements et d'autres informations.

    Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;

**Stockage de votre chaîne de connexion**
---

En règle générale, une chaîne de connexion sera stockée dans un fichier de configuration (tel qu'un `app.config` ou `web.config` dans les applications ASP.NET). Voici un exemple de ce à quoi pourrait ressembler une connexion locale dans l'un de ces fichiers :

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=True;"/> 
    </connectionStrings>

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=SSPI;"/> 
    </connectionStrings>

Cela permettra à votre application d'accéder à la chaîne de connexion par programmation via `WidgetsContext`. Bien que `Integrated Security=SSPI` et `Integrated Security=True` exécutent la même fonction, `Integrated Security=SSPI` est préféré car il fonctionne avec le fournisseur SQLClient & OleDB où `Integrated Security=true` lève une exception lorsqu'il est utilisé avec le fournisseur OleDb.

** Différentes connexions pour différents fournisseurs **
----

Chaque fournisseur de données (SQL Server, MySQL, Azure, etc.) propose sa propre syntaxe pour ses chaînes de connexion et expose différentes propriétés disponibles. [ConnectionStrings.com](https://www.connectionstrings.com/) est une ressource extrêmement utile si vous ne savez pas à quoi devrait ressembler la vôtre.


## Connexions Entity Framework
Entity Framework expose des classes d'abstraction utilisées pour interagir avec les bases de données sous-jacentes sous la forme de classes telles que `DbContext`. Ces contextes sont généralement constitués de propriétés `DbSet<T>` qui exposent les collections disponibles pouvant être interrogées :

    public class ExampleContext: DbContext 
    { 
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

Le `DbContext` lui-même gérera les connexions avec les bases de données et lira généralement les données de chaîne de connexion appropriées à partir d'une configuration pour déterminer comment établir les connexions :

    public class ExampleContext: DbContext 
    { 
        // The parameter being passed in to the base constructor indicates the name of the 
        // connection string
        public ExampleContext() : base("ExampleContextEntities")
        {
        }
    
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

**Exécution de requêtes Entity Framework**
----

En fait, l'exécution d'une requête Entity Framework peut être assez simple et vous oblige simplement à créer une instance du contexte, puis à utiliser les propriétés disponibles pour extraire ou accéder à vos données.

    using(var context = new ExampleContext())
    {
          // Retrieve all of the Widgets in your database
          var data = context.Widgets.ToList();
    }

Entity Framework fournit également un système complet de suivi des modifications qui peut être utilisé pour gérer la mise à jour des entrées dans votre base de données en appelant simplement la méthode `SaveChanges()` pour envoyer les modifications à la base de données :

    using(var context = new ExampleContext())
    {
          // Grab the widget you wish to update
          var widget = context.Widgets.Find(w => w.Id == id);
          // If it exists, update it
          if(widget != null)
          {
               // Update your widget and save your changes
               widget.Updated = DateTime.UtcNow;
               context.SaveChanges();
          }
    }

## Connexions ADO.NET
Les connexions ADO.NET sont l'un des moyens les plus simples de se connecter à une base de données à partir d'une application C#. Ils s'appuient sur l'utilisation d'un fournisseur et d'une chaîne de connexion qui pointe vers votre base de données pour effectuer des requêtes.

**Classes de fournisseur de données communes**
----

Bon nombre des classes suivantes sont couramment utilisées pour interroger les bases de données et leurs espaces de noms associés :

- `SqlConnection`,`SqlCommand`,`SqlDataReader` de `System.Data.SqlClient`
- `OleDbConnection`,`OleDbCommand`,`OleDbDataReader` de `System.Data.OleDb`
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader` de [`MySql.Data`](http://dev.mysql.com/downloads/file/?id=13427)

Tous ces éléments sont couramment utilisés pour accéder aux données via C # et seront couramment rencontrés lors de la création d'applications centrées sur les données. De nombreuses autres classes non mentionnées qui implémentent les mêmes classes `FooConnection`,`FooCommand`,`FooDataReader` peuvent s'attendre à se comporter de la même manière.

** Modèle d'accès commun pour les connexions ADO.NET **
----

Un modèle commun pouvant être utilisé lors de l'accès à vos données via une connexion ADO.NET peut ressembler à ceci :

    // This scopes the connection (your specific class may vary)
    using(var connection = new SqlConnection("{your-connection-string}")
    {
        // Build your query
        var query = "SELECT * FROM YourTable WHERE Property = @property");
        // Scope your command to execute
        using(var command = new SqlCommand(query, connection))
        {
             // Open your connection
             connection.Open();

             // Add your parameters here if necessary

             // Execute your query as a reader (again scoped with a using statement)
             using(var reader = command.ExecuteReader())
             {
                   // Iterate through your results here
             }
        }
    }

Ou si vous ne faisiez qu'une simple mise à jour et que vous n'aviez pas besoin d'un lecteur, le même concept de base s'appliquerait :

    using(var connection = new SqlConnection("{your-connection-string}"))
    {
         var query = "UPDATE YourTable SET Property = Value WHERE Foo = @foo";
         using(var command = new SqlCommand(query,connection))
         {
              connection.Open();
              
              // Add parameters here
              
              // Perform your update
              command.ExecuteNonQuery();
         }
    }


Vous pouvez même programmer sur un ensemble d'interfaces communes sans avoir à vous soucier des classes spécifiques au fournisseur. Les principales interfaces fournies par ADO.NET sont :

- IDbConnection - pour gérer les connexions à la base de données
- IDbCommand - pour exécuter des commandes SQL
- IDbTransaction - pour la gestion des transactions
- IDataReader - pour lire les données renvoyées par une commande
- IDataAdapter - pour canaliser les données vers et depuis les ensembles de données


    var connectionString = "{your-connection-string}";
    var providerName = "{System.Data.SqlClient}"; //for Oracle use "Oracle.ManagedDataAccess.Client"
    //most likely you will get the above two from ConnectionStringSettings object

    var factory = DbProviderFactories.GetFactory(providerName);

    using(var connection = new factory.CreateConnection()) {
        connection.ConnectionString = connectionString;
        connection.Open();

        using(var command = new connection.CreateCommand()) {
            command.CommandText = "{sql-query}";    //this needs to be tailored for each database system
    
            using(var reader = command.ExecuteReader()) {
                while(reader.Read()) {
                    ...
                }
            }
        }
    }

