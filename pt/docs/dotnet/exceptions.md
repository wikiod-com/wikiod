---
title: "Exceções"
slug: "excecoes"
draft: false
images: []
weight: 9760
type: docs
toc: true
---

Relacionado:

* [MSDN: Exceções e Tratamento de Exceções (Guia de Programação C#)](https://msdn.microsoft.com/en-us/library/ms173160.aspx)
* [MSDN: Tratamento e lançamento de exceções](https://msdn.microsoft.com/en-us/library/5b2yeyab.aspx)
* [MSDN: CA1031: Não detecte tipos de exceção gerais](https://msdn.microsoft.com/en-us/library/ms182137.aspx)
* [MSDN: try-catch (referência C#)](https://msdn.microsoft.com/en-us/library/0yd65esw.aspx)

## Capturando e relançando exceções capturadas
Quando você deseja capturar uma exceção e fazer algo, mas não pode continuar a execução do bloco de código atual por causa da exceção, convém relançar a exceção para o próximo manipulador de exceção na pilha de chamadas. Existem boas e más maneiras de fazer isso.

    private static void AskTheUltimateQuestion()
    {
        try
        {
            var x = 42;
            var y = x / (x - x); // will throw a DivideByZeroException

            // IMPORTANT NOTE: the error in following string format IS intentional
            // and exists to throw an exception to the FormatException catch, below
            Console.WriteLine("The secret to life, the universe, and everything is {1}", y); 
        }
        catch (DivideByZeroException)
        {
            // we do not need a reference to the exception
            Console.WriteLine("Dividing by zero would destroy the universe.");

            // do this to preserve the stack trace:
            throw;
        }
        catch (FormatException ex)
        {
            // only do this if you need to change the type of the Exception to be thrown 
            // and wrap the inner Exception

            // remember that the stack trace of the outer Exception will point to the
            // next line

            // you'll need to examine the InnerException property to get the stack trace
            // to the line that actually started the problem

            throw new InvalidOperationException("Watch your format string indexes.", ex);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Something else horrible happened. The exception: " + ex.Message);

            // do not do this, because the stack trace will be changed to point to
            // this location instead of the location where the exception
            // was originally thrown:
            throw ex; 
        }
    }

    static void Main()
    {
        try
        {
            AskTheUltimateQuestion();
        }
        catch
        {
            // choose this kind of catch if you don't need any information about 
            // the exception that was caught

            // this block "eats" all exceptions instead of rethrowing them
        }
    }

Você pode filtrar por tipo de exceção e até mesmo por propriedades de exceção (novo no C# 6.0, um pouco mais disponível no VB.NET (citação necessária)):

[Documentação/C#/novos recursos][1]


[1]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/46/exception-filters

## Usando um bloco finally
O bloco `finally { ... }` de um `try-finally` ou `try-catch-finally` sempre será executado, independentemente de ter ocorrido uma exceção ou não (exceto quando um `StackOverflowException` foi lançado ou a chamada foi foi feito para `Environment.FailFast()`).

Ele pode ser utilizado para liberar ou limpar recursos adquiridos no bloco `try { ... }` com segurança.

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream = null;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }
    finally
    {
        if (fileStream != null)
        {
            fileStream.Dispose();
        }
    }


## Filtros de exceção
Como as exceções do C# 6.0 podem ser filtradas usando o operador `when`.

Isso é semelhante a usar um simples `if`, mas não desenrola a pilha se a condição dentro do `when` não for atendida.

__Exemplo__
    
    try
    { 
      // ...
    }
    catch (Exception e) when (e.InnerException != null) // Any condition can go in here.
    {
      // ...
    }

A mesma informação pode ser encontrada em [C# 6.0 Features][1] aqui: [Exception filter][2]


[1]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features
[2]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/46/exception-filters#t=201607211048375185447

## Relançamento de uma exceção dentro de um bloco catch
Dentro de um bloco `catch`, a palavra-chave `throw` pode ser usada sozinha, sem especificar um valor de exceção, para *relançar* a exceção que acabou de ser capturada. Relançar uma exceção permite que a exceção original continue na cadeia de tratamento de exceções, preservando sua pilha de chamadas ou dados associados:
 
    try {...}
    catch (Exception ex) {
      // Note: the ex variable is *not* used
      throw;
    }

Um antipadrão comum é, em vez disso, `throw ex`, que tem o efeito de limitar a visão do próximo handler de exceção do rastreamento de pilha:

    try {...}
    catch (Exception ex) {
      // Note: the ex variable is thrown
      //  future stack traces of the exception will not see prior calls
      throw ex;  
    }

Em geral, usar `throw ex` não é desejável, pois futuros manipuladores de exceção que inspecionam o rastreamento de pilha só poderão ver chamadas até `throw ex`. Ao omitir a variável `ex` e usar a palavra-chave `throw` sozinha, a exceção original ["bubble-up"][1].

[1]: http://stackoverflow.com/questions/4065893/about-throw-and-exception-bubbling

## Lançando uma exceção de um método diferente preservando suas informações
Ocasionalmente, você deseja capturar uma exceção e lançá-la de um thread ou método diferente, preservando a pilha de exceção original. Isso pode ser feito com `ExceptionDispatchInfo`:

    using System.Runtime.ExceptionServices;

    void Main()
    {
        ExceptionDispatchInfo capturedException = null;
        try
        {
            throw new Exception();
        }
        catch (Exception ex)
        {
            capturedException = ExceptionDispatchInfo.Capture(ex);
        }
        
        Foo(capturedException);
    }
    
    void Foo(ExceptionDispatchInfo exceptionDispatchInfo)
    {
        // Do stuff
    
        if (capturedException != null)
        {
            // Exception stack trace will show it was thrown from Main() and not from Foo()
            exceptionDispatchInfo.Throw();
        }
    }

## Capturando uma exceção
O código pode e deve lançar exceções em circunstâncias excepcionais. Exemplos disso incluem:

- Tentativa de [ler após o final de um stream][1]
- [Não ter permissões necessárias][2] para acessar um arquivo
- Tentativa de realizar uma operação inválida, como [dividindo por zero][3]
- [Ocorrendo um tempo limite][4] ao baixar um arquivo da Internet

O chamador pode lidar com essas exceções "capturando-as" e só deve fazê-lo quando:

- Ele pode realmente resolver a circunstância excepcional ou recuperar adequadamente, ou;
- Ele pode fornecer contexto adicional para a exceção que seria útil se a exceção precisasse ser relançada (exceções relançadas são capturadas por manipuladores de exceção mais acima na pilha de chamadas)

Deve-se notar que escolher *não* capturar uma exceção é perfeitamente válido se a intenção for que ela seja tratada em um nível superior.

A captura de uma exceção é feita envolvendo o código potencialmente lançado em um bloco `try { ... }` da seguinte forma, e capturando as exceções que podem ser tratadas em um bloco `catch (ExceptionType) { ... }`:

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }


[1]: https://msdn.microsoft.com/en-us/library/system.io.endofstreamexception(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.unauthorizedaccessexception(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.dividebyzeroexception(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.net.webexception.aspx

