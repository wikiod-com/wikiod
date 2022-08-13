---
title: "Usando Declaração"
slug: "usando-declaracao"
draft: false
images: []
weight: 9015
type: docs
toc: true
---

Fornece uma sintaxe conveniente que garante o uso correto de objetos [IDisposable](https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netframework-4.7).



## Sintaxe
- usando (descartável) { }
- using (IDisposable descartável = new MyDisposable()) { }


O objeto na instrução `using` deve implementar a interface `IDisposable`.

    using(var obj = new MyObject())
    {
    }
    
    class MyObject : IDisposable
    {
        public void Dispose()
        {
            // Cleanup
        }
    }

Exemplos mais completos para a implementação do `IDisposable` podem ser encontrados nos [documentos do MSDN][1].


[1]: https://msdn.microsoft.com/en-us/library/fs2xkftw(v=vs.110).aspx
  

## Gotcha: retornando o recurso que você está descartando
O seguinte é uma má ideia porque descartaria a variável `db` antes de retorná-la.

    public IDBContext GetDBContext()
    {
        using (var db = new DBContext())
        {
            return db;
        }
    }

Isso também pode criar erros mais sutis:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age);
        }
    }

Isso parece bom, mas o problema é que a avaliação da expressão LINQ é preguiçosa e possivelmente só será executada mais tarde quando o `DBContext` subjacente já tiver sido descartado.

Então, resumindo, a expressão não é avaliada antes de sair do `using`. Uma possível solução para este problema, que ainda faz uso de `using`, é fazer com que a expressão seja avaliada imediatamente chamando um método que irá enumerar o resultado. Por exemplo `ToList()`, `ToArray()`, etc. Se você estiver usando a versão mais recente do Entity Framework, você pode usar as contrapartes `async` como `ToListAsync()` ou `ToArrayAsync()`.

Abaixo você encontra o exemplo em ação:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age).ToList();
        }
    }

É importante notar, porém, que ao chamar `ToList()` ou `ToArray()`, a expressão será avaliada avidamente, o que significa que todas as pessoas com a idade especificada serão carregadas na memória mesmo se você não iterar neles.

## Usando noções básicas de instrução
`using` é um açúcar sintático que permite garantir que um recurso seja limpo sem precisar de um bloco `try-finally` explícito. Isso significa que seu código ficará muito mais limpo e você não vazará recursos não gerenciados.

Padrão de limpeza `Dispose` padrão, para objetos que implementam a interface `IDisposable` (que a classe base `Stream` do `FileStream` faz em .NET):

    int Foo()
    {
        var fileName = "file.txt";

        {
            FileStream disposable = null;

            try
            {
                disposable = File.Open(fileName, FileMode.Open);

                return disposable.ReadByte();
            }
            finally
            {
                // finally blocks are always run
                if (disposable != null) disposable.Dispose();
            }
        }
    }

`using` simplifica sua sintaxe ocultando o `try-finally` explícito:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            return disposable.ReadByte();
        }
        // disposable.Dispose is called even if we return earlier
    }

Assim como os blocos `finally` sempre executam independentemente de erros ou retornos, `using` sempre chama `Dispose()`, mesmo no caso de um erro:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            throw new InvalidOperationException();
        }
        // disposable.Dispose is called even if we throw an exception earlier
    }
**Observação:**
Como `Dispose` é garantido para ser chamado independentemente do fluxo de código, é uma boa ideia garantir que `Dispose` nunca lance uma exceção quando você implementar `IDisposable`. Caso contrário, uma exceção real seria substituída pela nova exceção, resultando em um pesadelo de depuração.

Retornando do uso do bloco
---

    using ( var disposable = new DisposableItem() )
    {
        return disposable.SomeProperty;
    }

Por causa da semântica de `try..finally` para a qual o bloco `using` traduz, a instrução `return` funciona como esperado - o valor de retorno é avaliado antes que o bloco `finally` seja executado e o valor descartado. A ordem de avaliação é a seguinte:

1. Avalie o corpo `try`
2. Avalie e armazene em cache o valor retornado
3. Execute o bloco finalmente
4. Retorne o valor de retorno em cache

No entanto, você não pode retornar a própria variável `disposable`, pois ela conteria referência inválida e descartada - veja [exemplo relacionado][1].


[1]: https://www.wikiod.com/pt/docs/c%23/38/using-statement/327/gotcha-returning-the-resource-which-you-are-disposing#t=201608220847304515557

## Várias instruções using com um bloco
É possível usar várias instruções `using` aninhadas sem adicionar vários níveis de chaves aninhadas. Por exemplo:

    using (var input = File.OpenRead("input.txt"))
    {
        using (var output = File.OpenWrite("output.txt"))
        {
            input.CopyTo(output);
        } // output is disposed here
    } // input is disposed here

Uma alternativa é escrever:

    using (var input = File.OpenRead("input.txt"))
    using (var output = File.OpenWrite("output.txt"))
    {
        input.CopyTo(output);
    } // output and then input are disposed here

O que é exatamente equivalente ao primeiro exemplo.

*Observação:* instruções `using` aninhadas podem acionar a regra de análise de código da Microsoft [CS2002][1] (consulte [esta resposta][2] para esclarecimentos) e gerar um aviso. Conforme explicado na resposta vinculada, geralmente é seguro aninhar instruções `using`.

Quando os tipos dentro da instrução `using` são do mesmo tipo, você pode delimitá-los por vírgula e especificar o tipo apenas uma vez (embora isso seja incomum):

    using (FileStream file = File.Open("MyFile.txt"), file2 = File.Open("MyFile2.txt"))
    {
    }

Isso também pode ser usado quando os tipos têm uma hierarquia compartilhada:

    using (Stream file = File.Open("MyFile.txt"), data = new MemoryStream())
    {
    }

A palavra-chave `var` *não* pode ser usada no exemplo acima. Ocorreria um erro de compilação. Mesmo a declaração separada por vírgula não funcionará quando as variáveis ​​declaradas tiverem tipos de hierarquias diferentes.

[1]: https://msdn.microsoft.com/en-us/library/ms182334.aspx
[2]: http://stackoverflow.com/a/22323027/501011

## Gotcha: Exceção no método Dispose mascarando outros erros em Using blocks
Considere o seguinte bloco de código.

    try
    {
        using (var disposable = new MyDisposable())
        {
            throw new Exception("Couldn't perform operation.");
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine(ex.Message);
    }

    class MyDisposable : IDisposable
    {
        public void Dispose()
        {
            throw new Exception("Couldn't dispose successfully.");
        }
    }

Você pode esperar ver "Não foi possível realizar a operação" impresso no Console, mas na verdade você verá "Não foi possível descartar com sucesso". pois o método Dispose ainda é chamado mesmo após a primeira exceção ser lançada.

Vale a pena ficar atento a essa sutileza, pois pode estar mascarando o erro real que impediu o descarte do objeto e dificultar a depuração.



## As instruções de uso são nulas
Você não precisa verificar o objeto `IDisposable` para `null`. `using` não lançará uma exceção e `Dispose()` não será chamado:

    DisposableObject TryOpenFile()
    {
        return null;
    }

    // disposable is null here, but this does not throw an exception 
    using (var disposable = TryOpenFile())
    {
        // this will throw a NullReferenceException because disposable is null
        disposable.DoSomething(); 

        if(disposable != null)
        {
            // here we are safe because disposable has been checked for null
            disposable.DoSomething();
        }
    }

## Usando a sintaxe de descarte para definir o escopo personalizado
Para alguns casos de uso, você pode usar a sintaxe `using` para ajudar a definir um escopo personalizado. Por exemplo, você pode definir a classe a seguir para executar código em uma cultura específica.

    public class CultureContext : IDisposable
    {
        private readonly CultureInfo originalCulture;

        public CultureContext(string culture)
        {
            originalCulture = CultureInfo.CurrentCulture;
            Thread.CurrentThread.CurrentCulture = new CultureInfo(culture);
        }

        public void Dispose()
        {
            Thread.CurrentThread.CurrentCulture = originalCulture;
        }
    }

Você pode usar essa classe para definir blocos de código que são executados em uma cultura específica.

    Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");

    using (new CultureContext("nl-NL"))
    {
        // Code in this block uses the "nl-NL" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25-12-2016 00:00:00
    }

    using (new CultureContext("es-ES"))
    {        
        // Code in this block uses the "es-ES" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25/12/2016 0:00:00
    }

    // Reverted back to the original culture
    Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 12/25/2016 12:00:00 AM

Nota: como não usamos a instância `CultureContext` que criamos, não atribuímos uma variável para ela.

Esta técnica é usada pelo ``BeginForm`` [helper][1] no ASP.NET MVC.

[1]: https://msdn.microsoft.com/en-us/library/dd410596%28v=vs.100%29.aspx

## Usando instruções e conexões de banco de dados
A palavra-chave `using` garante que o recurso definido na instrução exista apenas dentro do escopo da própria instrução. Quaisquer recursos definidos na instrução devem implementar a interface `IDisposable`.

Estes são incrivelmente importantes ao lidar com qualquer conexão que implemente a interface `IDisposable`, pois pode garantir que as conexões não sejam apenas fechadas corretamente, mas que seus recursos sejam liberados depois que a instrução `using` estiver fora do escopo.


**Classes de dados `IDisposable` comuns**
---

Muitas das seguintes são classes relacionadas a dados que implementam a interface `IDisposable` e são candidatas perfeitas para uma instrução `using`:

- `SqlConnection`,`SqlCommand`,`SqlDataReader`, etc.
- `OleDbConnection`, `OleDbCommand`, `OleDbDataReader`, etc.
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader`, etc.
- `DbContext`

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

**Usando instruções com DataContexts**
---

Muitos ORMs, como o Entity Framework, expõem classes de abstração que são usadas para interagir com bancos de dados subjacentes na forma de classes como `DbContext`. Esses contextos geralmente implementam a interface `IDisposable` e devem tirar vantagem disso através de instruções `using` quando possível:

    using(var context = new YourDbContext())
    {
          // Access your context and perform your query
          var data = context.Widgets.ToList();
    }




## Executando código no contexto de restrição
Se você tem um código (uma *rotina*) que deseja executar em um contexto específico (restrição), pode usar injeção de dependência.

O exemplo a seguir mostra a restrição de execução em uma conexão SSL aberta. Essa primeira parte estaria em sua biblioteca ou estrutura, que você não exporá ao código do cliente.

    public static class SSLContext
    {
        // define the delegate to inject
        public delegate void TunnelRoutine(BinaryReader sslReader, BinaryWriter sslWriter);

        // this allows the routine to be executed under SSL
        public static void ClientTunnel(TcpClient tcpClient, TunnelRoutine routine)
        {
            using (SslStream sslStream = new SslStream(tcpClient.GetStream(), true, _validate))
            {
                sslStream.AuthenticateAsClient(HOSTNAME, null, SslProtocols.Tls, false);

                if (!sslStream.IsAuthenticated)
                {
                    throw new SecurityException("SSL tunnel not authenticated");
                }

                if (!sslStream.IsEncrypted)
                {
                    throw new SecurityException("SSL tunnel not encrypted");
                }

                using (BinaryReader sslReader = new BinaryReader(sslStream))
                using (BinaryWriter sslWriter = new BinaryWriter(sslStream))
                {
                    routine(sslReader, sslWriter);
                }
            }
        }
    }

Agora, o código do cliente que deseja fazer algo no SSL, mas não deseja lidar com todos os detalhes do SSL. Agora você pode fazer o que quiser dentro do túnel SSL, por exemplo, trocar uma chave simétrica:

    public void ExchangeSymmetricKey(BinaryReader sslReader, BinaryWriter sslWriter)
    {
        byte[] bytes = new byte[8];
        (new RNGCryptoServiceProvider()).GetNonZeroBytes(bytes);
        sslWriter.Write(BitConverter.ToUInt64(bytes, 0));
    }

Você executa esta rotina da seguinte forma:

    SSLContext.ClientTunnel(tcpClient, this.ExchangeSymmetricKey);

Para fazer isso, você precisa da cláusula `using()` porque é a única maneira (além de um bloco `try..finally`) de garantir que o código do cliente (`ExchangeSymmetricKey`) nunca saia sem descartar adequadamente o descartável Recursos. Sem a cláusula `using()`, você nunca saberia se uma rotina poderia quebrar a restrição do contexto para descartar esses recursos.


