---
title: "Iteradores"
slug: "iteradores"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

Um iterador é um método, acessador get ou operador que executa uma iteração personalizada em uma matriz ou classe de coleção usando a palavra-chave yield

## Criando Iteradores usando Yield
Iteradores *produzem* enumeradores. Em C#, os enumeradores são produzidos definindo métodos, propriedades ou indexadores que contêm instruções `yield`.

A maioria dos métodos retornará o controle para seu chamador por meio de instruções normais de `return`, que descartam todos os estados locais para esse método. Em contraste, os métodos que usam instruções `yield` permitem que eles retornem vários valores para o chamador a pedido, enquanto *preservam* o estado local entre o retorno desses valores. Esses valores retornados constituem uma sequência. Existem dois tipos de instruções `yield` usadas em iteradores:

- `yield return`, que retorna o controle para o chamador, mas preserva o estado. O chamado continuará a execução desta linha quando o controle for passado de volta para ele.

- `yield break`, que funciona de forma semelhante a uma instrução `return` normal - isso significa o fim da sequência. As instruções normais de `return` são ilegais dentro de um bloco iterador.


Este exemplo abaixo demonstra um método iterador que pode ser usado para gerar a [sequência de Fibonacci][1]:

    IEnumerable<int> Fibonacci(int count)
    {
        int prev = 1;
        int curr = 1;
        
        for (int i = 0; i < count; i++)
        {
            yield return prev;
            int temp = prev + curr;
            prev = curr;
            curr = temp;
        }
    }

Esse iterador pode ser usado para produzir um enumerador da sequência de Fibonacci que pode ser consumida por um método de chamada. O código abaixo demonstra como os dez primeiros termos da sequência de Fibonacci podem ser enumerados:

    void Main()
    {
        foreach (int term in Fibonacci(10))
        {
            Console.WriteLine(term);
        }
    }

**Resultado**

    1
    1
    2
    3
    5
    8
    13
    21
    34
    55

[1]: https://en.wikipedia.org/wiki/Fibonacci_number

## Exemplo de iterador numérico simples

Um caso de uso comum para iteradores é realizar alguma operação sobre uma coleção de números. O exemplo abaixo demonstra como cada elemento dentro de uma matriz de números pode ser impresso individualmente no console.

Isso é possível porque os arrays implementam a interface `IEnumerable`, permitindo que os clientes obtenham um iterador para o array usando o método `GetEnumerator()`. Esse método retorna um *enumerador*, que é um cursor somente leitura e somente encaminhamento sobre cada número na matriz.

    int[] numbers = { 1, 2, 3, 4, 5 };

    IEnumerator iterator = numbers.GetEnumerator();

    while (iterator.MoveNext())
    {
        Console.WriteLine(iterator.Current);
    }

**Resultado**

    1
    2
    3
    4
    5

Também é possível obter os mesmos resultados usando uma instrução `foreach`:

    foreach (int number in numbers)
    {
        Console.WriteLine(number);
    }



