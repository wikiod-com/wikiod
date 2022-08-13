---
title: "Acessando Bancos de Dados"
slug: "acessando-bancos-de-dados"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Cadeias de conexão
Uma cadeia de conexão é uma cadeia de caracteres que especifica informações sobre uma determinada fonte de dados e como se conectar a ela armazenando credenciais, locais e outras informações.

    Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;

**Armazenando sua string de conexão**
---

Normalmente, uma string de conexão será armazenada em um arquivo de configuração (como um `app.config` ou `web.config` em aplicativos ASP.NET). Veja a seguir um exemplo de como uma conexão local pode se parecer em um desses arquivos:

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=True;"/> 
    </connectionStrings>

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=SSPI;"/> 
    </connectionStrings>

Isso permitirá que seu aplicativo acesse a string de conexão programaticamente por meio de `WidgetsContext`. Embora `Integrated Security=SSPI` e `Integrated Security=True` executem a mesma função; `Integrated Security=SSPI` é preferível, pois funciona com o provedor SQLClient e OleDB, onde `Integrated Security=true` lança uma exceção quando usado com o provedor OleDb.

**Conexões diferentes para provedores diferentes**
----

Cada provedor de dados (SQL Server, MySQL, Azure etc.) apresenta seu próprio sabor de sintaxe para suas cadeias de conexão e expõe diferentes propriedades disponíveis. [ConnectionStrings.com](https://www.connectionstrings.com/) é um recurso incrivelmente útil se você não tiver certeza sobre como deve ser o seu.


## Conexões do Entity Framework
O Entity Framework expõe classes de abstração que são usadas para interagir com bancos de dados subjacentes na forma de classes como `DbContext`. Esses contextos geralmente consistem em propriedades `DbSet<T>` que expõem as coleções disponíveis que podem ser consultadas:

    public class ExampleContext: DbContext 
    { 
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

O próprio `DbContext` tratará de fazer as conexões com os bancos de dados e geralmente lerá os dados apropriados da String de conexão de uma configuração para determinar como estabelecer as conexões:

    public class ExampleContext: DbContext 
    { 
        // The parameter being passed in to the base constructor indicates the name of the 
        // connection string
        public ExampleContext() : base("ExampleContextEntities")
        {
        }
    
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

**Executando consultas do Entity Framework**
----

Na verdade, executar uma consulta do Entity Framework pode ser bastante fácil e simplesmente requer que você crie uma instância do contexto e, em seguida, use as propriedades disponíveis para extrair ou acessar seus dados

    using(var context = new ExampleContext())
    {
          // Retrieve all of the Widgets in your database
          var data = context.Widgets.ToList();
    }

O Entity Framework também fornece um extenso sistema de rastreamento de alterações que pode ser usado para manipular entradas de atualização em seu banco de dados simplesmente chamando o método `SaveChanges()` para enviar alterações ao banco de dados:

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

## Conexões ADO.NET
As conexões ADO.NET são uma das maneiras mais simples de se conectar a um banco de dados a partir de um aplicativo C#. Eles contam com o uso de um provedor e uma cadeia de conexão que aponta para seu banco de dados para realizar consultas.

**Classes de provedores de dados comuns**
----

Muitas das seguintes classes são comumente usadas para consultar bancos de dados e seus namespaces relacionados:

- `SqlConnection`,`SqlCommand`,`SqlDataReader` de `System.Data.SqlClient`
- `OleDbConnection`,`OleDbCommand`,`OleDbDataReader` de `System.Data.OleDb`
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader` de [`MySql.Data`](http://dev.mysql.com/downloads/file/?id=13427)

Todos eles são comumente usados ​​para acessar dados por meio de C# e serão comumente encontrados ao longo da criação de aplicativos centrados em dados. Muitas outras classes que não são mencionadas que implementam as mesmas classes `FooConnection`,`FooCommand`,`FooDataReader` podem se comportar da mesma maneira.

**Padrão de acesso comum para conexões ADO.NET**
----

Um padrão comum que pode ser usado ao acessar seus dados por meio de uma conexão ADO.NET pode ter a seguinte aparência:

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

Ou se você estivesse apenas realizando uma atualização simples e não precisasse de um leitor, o mesmo conceito básico se aplicaria:

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


Você pode até programar em um conjunto de interfaces comuns e não precisa se preocupar com as classes específicas do provedor. As principais interfaces fornecidas pelo ADO.NET são:

- IDbConnection - para gerenciar conexões de banco de dados
- IDbCommand - para executar comandos SQL
- IDbTransaction - para gerenciar transações
- IDataReader - para leitura de dados retornados por um comando
- IDataAdapter - para canalizar dados de e para conjuntos de dados


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

