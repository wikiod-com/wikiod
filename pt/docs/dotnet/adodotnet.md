---
title: "ADO.NET"
slug: "adonet"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

ADO(ActiveX Data Objects).Net é uma ferramenta fornecida pela Microsoft que fornece acesso a fontes de dados como SQL Server, Oracle e XML através de seus componentes. Os aplicativos front-end .Net podem recuperar, criar e manipular dados, desde que estejam conectados a uma fonte de dados por meio do ADO.Net com os privilégios apropriados.

O ADO.Net fornece uma arquitetura sem conexão. É uma abordagem segura para interagir com um banco de dados, pois a conexão não precisa ser mantida durante toda a sessão.

**Uma nota sobre como parametrizar SQLs com `Parameters.AddWithValue`:** `AddWithValue` nunca é um bom ponto de partida. Esse método depende de inferir o tipo de dados do que é passado. Com isso, você pode acabar em uma situação em que a conversão impeça sua consulta de [usar um índice](http://stackoverflow.com/q/799584 /87698). Observe que alguns tipos de dados do SQL Server, como `char`/`varchar` (sem "n" precedente") ou `date` não possuem um tipo de dados .NET correspondente. Nesses casos, [`Add` com o tipo de dados correto deve ser usado](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already /).

## Melhores Práticas - Executando Declarações Sql
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

## Prática recomendada para trabalhar com [ADO.NET][1]
* A regra geral é abrir a conexão por um tempo mínimo. Feche a conexão explicitamente quando a execução do procedimento terminar, isso retornará o objeto de conexão de volta ao pool de conexões. Tamanho máximo do pool de conexões padrão = 100. Como o pool de conexões melhora o desempenho da conexão física com o SQL Server.[Conexão Pooling in SQL Server][2]
* Envolva todas as conexões de banco de dados em um bloco de uso para que sejam sempre fechadas e descartadas, mesmo no caso de uma exceção. Consulte [usando instrução (referência C#)][3] para obter mais informações sobre o uso de instruções
* Recupere as strings de conexão por nome do app.config ou web.config (dependendo do tipo de aplicativo)
* Isso requer uma referência de assembly para `System.configuration`
* Consulte [Strings de conexão e arquivos de configuração][4] para obter informações adicionais sobre como estruturar seu arquivo de configuração
* Sempre use parâmetros para valores de entrada para
* Evite ataques de [injeção de sql][5]
* Evite erros se o texto malformado for usado, como incluir uma aspa simples que é o equivalente sql de escapar ou iniciar uma string (varchar/nvarchar)
* Permitir que o provedor de banco de dados reutilize planos de consulta (não suportados por todos os provedores de banco de dados), o que aumenta a eficiência
* Ao trabalhar com parâmetros
* A incompatibilidade de tipo e tamanho de parâmetros SQL é uma causa comum de falha de inserção/atualização/seleção
* Dê nomes significativos aos seus parâmetros Sql, assim como você faz variáveis ​​em seu código
* Especifique o tipo de dados do banco de dados da coluna que você está usando, isso garante que os tipos de parâmetros errados não sejam usados, o que pode levar a resultados inesperados
* Valide seus parâmetros de entrada antes de passá-los para o comando (como diz o ditado, "[garbage in, garbage out](https://en.wikipedia.org/wiki/Garbage_in,_garbage_out)"). Valide os valores de entrada o mais cedo possível na pilha
* Use os tipos corretos ao atribuir seus valores de parâmetro, por exemplo: não atribua o valor de string de um DateTime, em vez disso, atribua uma instância de DateTime real ao valor do parâmetro
* Especifique o [tamanho](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparameter.size(v=vs.110).aspx) dos parâmetros do tipo string. Isso ocorre porque o SQL Server pode reutilizar planos de execução se os parâmetros corresponderem em tipo * e * tamanho. Use -1 para MAX
* Não use o método [AddWithValue][6], a principal razão é que é muito fácil esquecer de especificar o tipo de parâmetro ou a precisão/escala quando necessário. Para obter informações adicionais, consulte [Já podemos parar de usar AddWithValue?][7]
* Ao usar conexões de banco de dados
* Abra a conexão o mais tarde possível e feche-a o mais rápido possível. Esta é uma orientação geral ao trabalhar com qualquer recurso externo
* Nunca compartilhe instâncias de conexão de banco de dados (exemplo: ter um host singleton uma instância compartilhada do tipo `SqlConnection`). Faça com que seu código sempre crie uma nova instância de conexão de banco de dados quando necessário e, em seguida, faça com que o código de chamada a descarte e "jogue fora" quando terminar. A razão disso é
    * Most database providers have some sort of connection pooling so creating new managed connections is cheap
    * It eliminates any future errors if the code starts working with multiple threads


[1]:https://msdn.microsoft.com/en-us/library/h43ks021(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/8xx3tyca(v=vs.110).aspx
[3]:https://msdn.microsoft.com/en-us/library/yh598w02.aspx
[4]:https://msdn.microsoft.com/en-us/library/ms254494(v=vs.110).aspx
[5]:https://en.wikipedia.org/wiki/SQL_injection
[6]:https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparametercollection.addwithvalue(v=vs.110).aspx
[7]:http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already/

## Executando instruções SQL como um comando
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

***Observação 1:*** Consulte [SqlDbType Enumeration][1] para obter a variação específica do MSFT SQL Server.

***Nota 2:*** Consulte [MySqlDbType Enumeration][2] para obter a variação específica do MySQL.


[1]: https://msdn.microsoft.com/en-us/library/system.data.sqldbtype(v=vs.110).aspx
[2]: https://dev.mysql.com/doc/dev/connector-net/html/T_MySql_Data_MySqlClient_MySqlDbType.htm

## Usando interfaces comuns para abstrair classes específicas de fornecedores
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

