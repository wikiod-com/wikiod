---
title: "ADO.NET"
slug: "adonet"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

ADO (ActiveX Data Objects).Net es una herramienta proporcionada por Microsoft que brinda acceso a fuentes de datos como SQL Server, Oracle y XML a través de sus componentes. Las aplicaciones front-end .Net pueden recuperar, crear y manipular datos, una vez que están conectadas a una fuente de datos a través de ADO.Net con los privilegios apropiados.

ADO.Net proporciona una arquitectura sin conexión. Es un método seguro para interactuar con una base de datos, ya que no es necesario mantener la conexión durante toda la sesión.

**Una nota sobre la parametrización de SQL con `Parameters.AddWithValue`:** `AddWithValue` nunca es un buen punto de partida. Ese método se basa en inferir el tipo de datos de lo que se pasa. Con esto, podría terminar en una situación en la que la conversión evita que su consulta [use un índice] (http://stackoverflow.com/q/799584 /87698). Tenga en cuenta que algunos tipos de datos de SQL Server, como `char`/`varchar` (sin la "n" anterior) o `date` no tienen un tipo de datos .NET correspondiente. En esos casos, se debe usar [`Add` con el tipo de datos correcto](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already /).

## Prácticas recomendadas - Ejecución de sentencias Sql
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

## Mejores prácticas para trabajar con [ADO.NET][1]
* La regla general es abrir la conexión por un tiempo mínimo. Cierre la conexión explícitamente una vez que finalice la ejecución de su procedimiento, esto devolverá el objeto de conexión al grupo de conexiones. Tamaño máximo del grupo de conexiones predeterminado = 100. Como el grupo de conexiones mejora el rendimiento de la conexión física a SQL Server. [Agrupación de conexiones en SQL Server][2]
* Envuelva todas las conexiones de la base de datos en un bloque de uso para que siempre estén cerradas y eliminadas incluso en el caso de una excepción. Consulte [using Statement (referencia de C#)][3] para obtener más información sobre el uso de declaraciones.
* Recuperar las cadenas de conexión por nombre de app.config o web.config (según el tipo de aplicación)
* Esto requiere una referencia de ensamblado a `System.configuration`
* Consulte [Cadenas de conexión y archivos de configuración][4] para obtener información adicional sobre cómo estructurar su archivo de configuración
* Utilice siempre parámetros para los valores entrantes para
* Evite los ataques de [inyección sql][5]
* Evite errores si se usa texto con formato incorrecto, como incluir una comilla simple, que es el equivalente en SQL de escapar o iniciar una cadena (varchar/nvarchar)
* Permitir que el proveedor de la base de datos reutilice los planes de consulta (no compatibles con todos los proveedores de bases de datos), lo que aumenta la eficiencia
* Cuando se trabaja con parámetros
* La discrepancia entre el tipo y el tamaño de los parámetros Sql es una causa común de errores de inserción/actualización/selección
* Dé a sus parámetros Sql nombres significativos al igual que hace con las variables en su código
* Especifique el tipo de datos de la base de datos de la columna que está utilizando, esto garantiza que no se utilicen tipos de parámetros incorrectos, lo que podría generar resultados inesperados
* Valide sus parámetros entrantes antes de pasarlos al comando (como dice el refrán, "[basura entra, basura sale] (https://en.wikipedia.org/wiki/Garbage_in,_garbage_out)"). Valide los valores entrantes lo antes posible en la pila
* Use los tipos correctos cuando asigne los valores de sus parámetros, ejemplo: no asigne el valor de cadena de un DateTime, en su lugar, asigne una instancia de DateTime real al valor del parámetro
* Especifique el [tamaño](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparameter.size(v=vs.110).aspx) de los parámetros de tipo cadena. Esto se debe a que SQL Server puede reutilizar los planes de ejecución si los parámetros coinciden en tipo *y* tamaño. Utilice -1 para MÁX.
* No use el método [AddWithValue][6], la razón principal es que es muy fácil olvidarse de especificar el tipo de parámetro o la precisión/escala cuando sea necesario. Para obtener información adicional, consulte [¿Podemos dejar de usar AddWithValue ya?][7]
* Al usar conexiones de base de datos
* Abra la conexión lo más tarde posible y ciérrela lo antes posible. Esta es una guía general cuando se trabaja con cualquier recurso externo
* Nunca comparta instancias de conexión de base de datos (ejemplo: tener un singleton que aloje una instancia compartida de tipo `SqlConnection`). Haga que su código siempre cree una nueva instancia de conexión de base de datos cuando sea necesario y luego haga que el código de llamada lo deseche y "tírelo" cuando termine. La razón de esto es
    * Most database providers have some sort of connection pooling so creating new managed connections is cheap
    * It eliminates any future errors if the code starts working with multiple threads


[1]:https://msdn.microsoft.com/en-us/library/h43ks021(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/8xx3tyca(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/yh598w02.aspx
[4]:https://msdn.microsoft.com/en-us/library/ms254494(v=vs.110).aspx
[5]: https://en.wikipedia.org/wiki/SQL_injection
[6]:https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparametercollection.addwithvalue(v=vs.110).aspx
[7]:http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-ya/

## Ejecutar sentencias SQL como un comando
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

***Nota 1:*** Consulte [SqlDbType Enumeration][1] para conocer la variación específica de MSFT SQL Server.

***Nota 2:*** Consulte [Enumeración MySqlDbType][2] para conocer la variación específica de MySQL.


[1]: https://msdn.microsoft.com/en-us/library/system.data.sqldbtype(v=vs.110).aspx
[2]: https://dev.mysql.com/doc/dev/connector-net/html/T_MySql_Data_MySqlClient_MySqlDbType.htm

## Uso de interfaces comunes para abstraer clases específicas de proveedores
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

