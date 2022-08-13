---
title: "Acceso a bases de datos"
slug: "acceso-a-bases-de-datos"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Cadenas de conexión
Una cadena de conexión es una cadena que especifica información sobre una fuente de datos en particular y cómo conectarse a ella mediante el almacenamiento de credenciales, ubicaciones y otra información.

    Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;

**Almacenamiento de su cadena de conexión**
---

Por lo general, una cadena de conexión se almacenará dentro de un archivo de configuración (como `app.config` o `web.config` dentro de las aplicaciones ASP.NET). El siguiente es un ejemplo de cómo se vería una conexión local dentro de uno de estos archivos:

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=True;"/> 
    </connectionStrings>

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=SSPI;"/> 
    </connectionStrings>

Esto permitirá que su aplicación acceda a la cadena de conexión programáticamente a través de `WidgetsContext`. Aunque tanto `Integrated Security=SSPI` como `Integrated Security=True` realizan la misma función, se prefiere `Integrated Security=SSPI` ya que funciona tanto con SQLClient como con el proveedor OleDB, mientras que `Integrated Security=true` arroja una excepción cuando se usa con el proveedor OleDb.

**Diferentes conexiones para diferentes proveedores**
----

Cada proveedor de datos (SQL Server, MySQL, Azure, etc.) presenta su propio tipo de sintaxis para sus cadenas de conexión y expone diferentes propiedades disponibles. [ConnectionStrings.com](https://www.connectionstrings.com/) es un recurso increíblemente útil si no está seguro de cómo debería ser el suyo.


## Conexiones de Entity Framework
Entity Framework expone clases de abstracción que se utilizan para interactuar con bases de datos subyacentes en forma de clases como `DbContext`. Estos contextos generalmente consisten en propiedades `DbSet<T>` que exponen las colecciones disponibles que se pueden consultar:

    public class ExampleContext: DbContext 
    { 
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

El propio `DbContext` se encargará de hacer las conexiones con las bases de datos y, en general, leerá los datos de la cadena de conexión apropiados de una configuración para determinar cómo establecer las conexiones:

    public class ExampleContext: DbContext 
    { 
        // The parameter being passed in to the base constructor indicates the name of the 
        // connection string
        public ExampleContext() : base("ExampleContextEntities")
        {
        }
    
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

**Ejecución de consultas de Entity Framework**
----

En realidad, ejecutar una consulta de Entity Framework puede ser bastante fácil y simplemente requiere que cree una instancia del contexto y luego use las propiedades disponibles para extraer o acceder a sus datos.

    using(var context = new ExampleContext())
    {
          // Retrieve all of the Widgets in your database
          var data = context.Widgets.ToList();
    }

Entity Framework también proporciona un extenso sistema de seguimiento de cambios que se puede usar para manejar la actualización de entradas dentro de su base de datos simplemente llamando al método `SaveChanges()` para enviar cambios a la base de datos:

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

## Conexiones ADO.NET
Las conexiones ADO.NET son una de las formas más sencillas de conectarse a una base de datos desde una aplicación C#. Se basan en el uso de un proveedor y una cadena de conexión que apunta a su base de datos para realizar consultas.

**Clases de proveedores de datos comunes**
----

Muchas de las siguientes son clases que se usan comúnmente para consultar bases de datos y sus espacios de nombres relacionados:

- `SqlConnection`,`SqlCommand`,`SqlDataReader` de `System.Data.SqlClient`
- `OleDbConnection`,`OleDbCommand`,`OleDbDataReader` de `System.Data.OleDb`
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader` de [`MySql.Data`](http://dev.mysql.com/downloads/file/?id=13427)

Todos estos se usan comúnmente para acceder a datos a través de C# y se encontrarán comúnmente en la creación de aplicaciones centradas en datos. Se puede esperar que muchas otras clases que no se mencionan que implementan las mismas clases `FooConnection`,`FooCommand`,`FooDataReader` se comporten de la misma manera.

**Patrón de acceso común para conexiones ADO.NET**
----

Un patrón común que se puede usar al acceder a sus datos a través de una conexión ADO.NET podría tener el siguiente aspecto:

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

O si solo estuviera realizando una actualización simple y no necesitara un lector, se aplicaría el mismo concepto básico:

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


Incluso puede programar contra un conjunto de interfaces comunes y no tener que preocuparse por las clases específicas del proveedor. Las interfaces principales proporcionadas por ADO.NET son:

- IDbConnection: para administrar las conexiones de la base de datos
- IDbCommand - para ejecutar comandos SQL
- IDbTransaction - para gestionar transacciones
- IDataReader - para leer los datos devueltos por un comando
- IDataAdapter: para canalizar datos hacia y desde conjuntos de datos


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

