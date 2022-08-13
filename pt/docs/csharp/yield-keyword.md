---
title: "Palavra-chave de rendimento"
slug: "palavra-chave-de-rendimento"
draft: false
images: []
weight: 8758
type: docs
toc: true
---

Ao usar a palavra-chave yield em uma instrução, você indica que o método, operador ou acessador get no qual ela aparece é um iterador. Usar yield para definir um iterador elimina a necessidade de uma classe extra explícita (a classe que mantém o estado de uma enumeração) quando você implementa o padrão IEnumerable e IEnumerator para um tipo de coleção personalizado.

## Sintaxe
- retorno de rendimento [TYPE]
- quebra de rendimento

Colocar a palavra-chave `yield` em um método com o tipo de retorno `IEnumerable`, `IEnumerable<T>`, `IEnumerator` ou `IEnumerator<T>` informa ao compilador para gerar uma implementação do tipo de retorno (`IEnumerable ` ou `IEnumerator`) que, quando repetido, executa o método até cada "rendimento" para obter cada resultado.

A palavra-chave `yield` é útil quando você deseja retornar "o próximo" elemento de uma sequência teoricamente ilimitada, portanto, calcular a sequência inteira de antemão seria impossível, ou quando calcular a sequência completa de valores antes de retornar levaria a uma pausa indesejável para o usuário.

`yield break` também pode ser usado para encerrar a sequência a qualquer momento.

Como a palavra-chave `yield` requer um tipo de interface de iterador como o tipo de retorno, como `IEnumerable<T>`, você não pode usar isso em um método assíncrono, pois isso retorna um objeto `Task<IEnumerable<T>>`.

**Leitura adicional**

- https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx

## Uso Simples
A palavra-chave `yield` é usada para definir uma função que retorna um `IEnumerable` ou `IEnumerator` (assim como suas variantes genéricas derivadas) cujos valores são gerados lentamente à medida que um chamador itera sobre a coleção retornada. Leia mais sobre o objetivo na [seção de comentários](https://www.wikiod.com/pt/docs/c%23/61/yield-keyword#remarks).

O exemplo a seguir tem uma instrução yield return que está dentro de um loop `for`.

    public static IEnumerable<int> Count(int start, int count)
    {
        for (int i = 0; i <= count; i++)
        {
            yield return start + i;
        }
    }

Então você pode chamá-lo:

    foreach (int value in Count(start: 4, count: 10))
    {
        Console.WriteLine(value);
    }

**Saída do console**

    4
    5
    6
    ...
    14


[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/qtKObr)

Cada iteração do corpo da instrução `foreach` cria uma chamada para a função iteradora `Count`. Cada chamada para a função iteradora prossegue para a próxima execução da instrução `yield return`, que ocorre durante a próxima iteração do loop `for`.

## Verificando corretamente os argumentos
Um método iterador não é executado até que o valor de retorno seja enumerado. Portanto, é vantajoso afirmar pré-condições fora do iterador.

    public static IEnumerable<int> Count(int start, int count)
    {
        // The exception will throw when the method is called, not when the result is iterated
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));

        return CountCore(start, count);
    }

    private static IEnumerable<int> CountCore(int start, int count)
    {
        // If the exception was thrown here it would be raised during the first MoveNext()
        // call on the IEnumerator, potentially at a point in the code far away from where
        // an incorrect value was passed.
        for (int i = 0; i < count; i++)
        {
            yield return start + i;
        }
    }

**Código do lado da chamada (uso):**
        
    // Get the count
    var count = Count(1,10);
    // Iterate the results
    foreach(var x in count)
    {
        Console.WriteLine(x);
    }
**Resultado:**
>1
>2
>3
>4
>5
>6
>7
>8
>9
>10

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/yIYxo6)

Quando um método usa `yield` para gerar um enumerável, o compilador cria uma máquina de estado que, quando iterada, executará o código até um `yield`. Em seguida, ele retorna o item gerado e salva seu estado.

Isso significa que você não descobrirá argumentos inválidos (passando `null` etc.) dentro do método é executado pela máquina de estado). Ao envolvê-lo em um método normal que primeiro verifica os argumentos, você pode verificá-los quando o método é chamado. Este é um exemplo de falha rápida.

Ao usar C# 7+, a função `CountCore` pode ser convenientemente ocultada na função `Count` como uma _função local_. Veja o exemplo [aqui](https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/6330/local-functions#t=201607251321358412005#t=201607251057101259341).

## Rescisão antecipada
Você pode estender a funcionalidade dos métodos `yield` existentes passando um ou mais valores ou elementos que podem definir uma condição de término dentro da função chamando um `yield break` para interromper a execução do loop interno.

    public static IEnumerable<int> CountUntilAny(int start, HashSet<int> earlyTerminationSet)
    {
        int curr = start;

        while (true)
        {
            if (earlyTerminationSet.Contains(curr))
            {
                // we've hit one of the ending values
                yield break;
            }

            yield return curr;

            if (curr == Int32.MaxValue)
            {
                // don't overflow if we get all the way to the end; just stop
                yield break;
            }

            curr++;
        }
    }

O método acima iria iterar de uma dada posição `start` até que um dos valores dentro do `earlyTerminationSet` fosse encontrado.

    // Iterate from a starting point until you encounter any elements defined as 
    // terminating elements
    var terminatingElements = new HashSet<int>{ 7, 9, 11 };
    // This will iterate from 1 until one of the terminating elements is encountered (7)
    foreach(var x in CountUntilAny(1,terminatingElements))
    {
        // This will write out the results from 1 until 7 (which will trigger terminating)
        Console.WriteLine(x);
    }
**Resultado:**
>1
>2
>3
>4
>5
>6

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/pctiOz)

## Uso mais pertinente
    public IEnumerable<User> SelectUsers()
    {
        // Execute an SQL query on a database.
        using (IDataReader reader = this.Database.ExecuteReader(CommandType.Text, "SELECT Id, Name FROM Users"))
        {
            while (reader.Read())
            {
                int id = reader.GetInt32(0);
                string name = reader.GetString(1);
                yield return new User(id, name);
            }
        }
    }

Existem outras maneiras de obter um `IEnumerable<User>` de um banco de dados SQL, é claro - isso apenas demonstra que você pode usar `yield` para transformar qualquer coisa que tenha semântica de "sequência de elementos" em um `IEnumerable<T> ` que alguém pode iterar.

## Avaliação preguiçosa
Somente quando a instrução `foreach` se move para o próximo item o bloco iterador avalia até a próxima instrução `yield`.

Considere o seguinte exemplo:

    private IEnumerable<int> Integers()
    {
        var i = 0;
        while(true)
        {
            Console.WriteLine("Inside iterator: " + i);
            yield return i;
            i++;
        }
    }
    
    private void PrintNumbers()
    {
        var numbers = Integers().Take(3);
        Console.WriteLine("Starting iteration");

        foreach(var number in numbers)
        {
            Console.WriteLine("Inside foreach: " + number);
        }
    }


Isso irá gerar:

>Iniciando iteração
> Dentro do iterador: 0
>Foreach interno: 0
> Dentro do iterador: 1
>Foreach interno: 1
> Dentro do iterador: 2
>Foreach interno: 2

[Ver demonstração][1]

Como consequência:

- "Iniciando iteração" é impresso primeiro mesmo que o método iterador tenha sido chamado antes da linha imprimi-lo porque a linha `Integers().Take(3);` na verdade não inicia a iteração (nenhuma chamada para `IEnumerator.MoveNext()` foi feito)
- As linhas impressas no console alternam entre as que estão dentro do método iterador e as que estão dentro do `foreach`, em vez de todas as que estão dentro do método iterador avaliando primeiro
- Este programa termina devido ao método `.Take()`, mesmo que o método iterador tenha um `while true` do qual ele nunca quebra.


[1]: https://dotnetfiddle.net/2qGV0B

## Tente...finalmente
Se um método iterador tiver um yield dentro de um `try...finally`, então o `IEnumerator` retornado executará a instrução `finally` quando `Dispose` for chamado nele, desde que o ponto atual de avaliação esteja dentro do bloco `tentar`.

Dada a função:

    private IEnumerable<int> Numbers()
    {
        yield return 1;
        try
        {
            yield return 2;
            yield return 3;
        }
        finally
        {
            Console.WriteLine("Finally executed");
        }
    }

Ao ligar:

    private void DisposeOutsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Em seguida, imprime:
>1

[Ver demonstração][1]

Ao ligar:

    private void DisposeInsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Em seguida, imprime:
>1
>2
>Finalmente executado

[Ver demonstração][2]


[1]: https://dotnetfiddle.net/MJt7dt
[2]: https://dotnetfiddle.net/HlMroV

## Usando yield para criar um IEnumerator<T> ao implementar IEnumerable<T>
A interface `IEnumerable<T>` tem um único método, `GetEnumerator()`, que retorna um `IEnumerator<T>`.

Embora a palavra-chave `yield` possa ser usada para criar diretamente um `IEnumerable<T>`, ela *também* pode ser usada exatamente da mesma maneira para criar um `IEnumerator<T>`. A única coisa que muda é o tipo de retorno do método.

Isso pode ser útil se quisermos criar nossa própria classe que implementa `IEnumerable<T>`:

    public class PrintingEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> _wrapped;
    
        public PrintingEnumerable(IEnumerable<T> wrapped)
        {
            _wrapped = wrapped;
        }
    
        // This method returns an IEnumerator<T>, rather than an IEnumerable<T>
        // But the yield syntax and usage is identical.
        public IEnumerator<T> GetEnumerator()
        {
            foreach(var item in _wrapped)
            {
                Console.WriteLine("Yielding: " + item);
                yield return item;
            }
        }
    
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

(Observe que este exemplo específico é apenas ilustrativo e pode ser implementado de forma mais limpa com um único método iterador retornando um `IEnumerable<T>`.)
    

## Avaliação ansiosa
A palavra-chave `yield` permite uma avaliação lenta da coleção. O carregamento forçado de toda a coleção na memória é chamado de **avaliação antecipada**.

O código a seguir mostra isso:

    IEnumerable<int> myMethod()
    {
        for(int i=0; i <= 8675309; i++)
        {
            yield return i;
        }
    }
    ...
    // define the iterator
    var it = myMethod.Take(3);
    // force its immediate evaluation
    // list will contain 0, 1, 2
    var list = it.ToList();

Chamar `ToList`, `ToDictionary` ou `ToArray` forçará a avaliação imediata da enumeração, recuperando todos os elementos em uma coleção.

## Retorna outro Enumerable dentro de um método que retorna Enumerable
    public IEnumerable<int> F1()
    {
        for (int i = 0; i < 3; i++)
            yield return i;
    
        //return F2(); // Compile Error!!
        foreach (var element in F2())
            yield return element;
    }
    
    public int[] F2()
    {
        return new[] { 3, 4, 5 };
    }

## Exemplo de avaliação preguiçosa: números de Fibonacci
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics; // also add reference to System.Numberics
    
    namespace ConsoleApplication33
    {
        class Program
        {
            private static IEnumerable<BigInteger> Fibonacci()
            {
                BigInteger prev = 0;
                BigInteger current = 1;
                while (true)
                {
                    yield return current;
                    var next = prev + current;
                    prev = current;
                    current = next;
                }
            }
    
            static void Main()
            {
                // print Fibonacci numbers from 10001 to 10010
                var numbers = Fibonacci().Skip(10000).Take(10).ToArray();
                Console.WriteLine(string.Join(Environment.NewLine, numbers));
            }
        }
    }

Como funciona sob o capô (recomendo descompilar o arquivo .exe resultante na ferramenta IL Disambler):
1. O compilador C# gera uma classe implementando `IEnumerable<BigInteger>` e `IEnumerator<BigInteger>` (`<Fibonacci>d__0` em ildasm).
2. Esta classe implementa uma máquina de estado. O estado consiste na posição atual no método e nos valores das variáveis ​​locais.
3. O código mais interessante está no método `bool IEnumerator.MoveNext()`. Basicamente, o que `MoveNext()` faz:
- Restaura o estado atual. Variáveis ​​como `prev` e `current` tornam-se campos em nossa classe (`<current>5__2` e `<prev>5__1` em ildasm). Em nosso método temos duas posições (`<>1__state`): primeiro na chave de abertura, segundo em `yield return`.
- Executa o código até o próximo `yield return` ou `yield break`/`}`.
- Para `yield return` o valor resultante é salvo, então a propriedade `Current` pode devolvê-lo. `true` é retornado. Neste ponto, o estado atual é salvo novamente para a próxima chamada `MoveNext`.
- Para o método `yield break`/`}` apenas retorna `false` significando que a iteração está concluída.

Observe também que o 10001º número tem 468 bytes. A máquina de estado só salva as variáveis ​​`current` e `prev` como campos. Enquanto se quisermos salvar todos os números na sequência do primeiro ao 10.000º, o tamanho da memória consumida será superior a 4 megabytes. Portanto, a avaliação preguiçosa, se usada corretamente, pode reduzir o consumo de memória em alguns casos.

## A diferença entre quebra e quebra de rendimento
Usar `yield break` em vez de `break` pode não ser tão óbvio quanto se pode pensar. Existem muitos exemplos ruins na Internet em que o uso dos dois é intercambiável e não demonstra a diferença.

A parte confusa é que ambas as palavras-chave (ou frases-chave) fazem sentido apenas dentro de loops (`foreach`, `while`...) Então, quando escolher uma em vez da outra?

É importante perceber que, uma vez que você usa a palavra-chave [`yield`](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx) em um método, você efetivamente transforma o método em um [iterador]( https://msdn.microsoft.com/en-us/library/mt639331.aspx). O único propósito de tal método é então iterar sobre uma coleção finita ou infinita e produzir (saída) seus elementos. Uma vez cumprido o propósito, não há razão para continuar a execução do método. Às vezes, isso acontece naturalmente com o último colchete de fechamento do método `}`. Mas às vezes, você deseja encerrar o método prematuramente. Em um método normal (sem iteração), você usaria a palavra-chave [`return`](https://msdn.microsoft.com/en-us/library/1h3swy84.aspx). Mas você não pode usar `return` em um iterador, você tem que usar `yield break`. Em outras palavras, `yield break` para um iterador é o mesmo que `return` para um método padrão. Considerando que, a instrução [`break`](https://msdn.microsoft.com/en-us/library/adbctzc4.aspx) apenas encerra o loop mais próximo.

Vejamos alguns exemplos:

```
    /// <summary>
    /// Yields numbers from 0 to 9
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9}</returns>
    public static IEnumerable<int> YieldBreak()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Indicates that the iteration has ended, everything 
                // from this line on will be ignored
                yield break;
            }
        }
        yield return 10; // This will never get executed
    }
```

```
    /// <summary>
    /// Yields numbers from 0 to 10
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9,10}</returns>
    public static IEnumerable<int> Break()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Terminates just the loop
                break;
            }
        }
        // Execution continues
        yield return 10;
    }
```

