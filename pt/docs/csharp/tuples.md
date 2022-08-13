---
title: "Tuplas"
slug: "tuplas"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Acessando elementos de tupla
Para acessar os elementos da tupla, use as propriedades `Item1`-`Item8`. Apenas as propriedades com número de índice menor ou igual ao tamanho da tupla estarão disponíveis (ou seja, não se pode acessar a propriedade `Item3` em `Tuple<T1,T2>`).

    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());
    var item1 = tuple.Item1; // "foo"
    var item2 = tuple.Item2; // 123
    var item3 = tuple.Item3; // true
    var item4 = tuple.Item4; // new My Class()

## Criando tuplas
Tuplas são criadas usando tipos genéricos `Tuple<T1>`-`Tuple<T1,T2,T3,T4,T5,T6,T7,T8>`. Cada um dos tipos representa uma tupla contendo de 1 a 8 elementos. Os elementos podem ser de diferentes tipos.

    // tuple with 4 elements
    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());

Tuplas também podem ser criadas usando métodos estáticos `Tuple.Create`. Nesse caso, os tipos dos elementos são inferidos pelo compilador C#.

    // tuple with 4 elements
    var tuple = Tuple.Create("foo", 123, true, new MyClass());
<!-- if versão [gte 7.0] -->
Desde o C# 7.0, as Tuplas podem ser facilmente criadas usando [ValueTuple][1].

    var tuple = ("foo", 123, true, new MyClass());

Os elementos podem ser nomeados para facilitar a decomposição.

    (int number, bool flag, MyClass instance) tuple = (123, true, new MyClass());

<!-- versão final if -->


[1]: https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

## Comparando e classificando Tuplas
Tuplas podem ser comparadas com base em seus elementos.

Como exemplo, um enumerável cujos elementos são do tipo `Tuple` podem ser classificados com base em operadores de comparação definidos em um elemento especificado:

    List<Tuple<int, string>> list = new List<Tuple<int, string>>();
    list.Add(new Tuple<int, string>(2, "foo"));
    list.Add(new Tuple<int, string>(1, "bar"));
    list.Add(new Tuple<int, string>(3, "qux"));

    list.Sort((a, b) => a.Item2.CompareTo(b.Item2)); //sort based on the string element

    foreach (var element in list) {
        Console.WriteLine(element);
    }
    
    // Output:
    // (1, bar)
    // (2, foo)
    // (3, qux)

Ou para reverter o uso de classificação:

    list.Sort((a, b) => b.Item2.CompareTo(a.Item2));

## Retorna vários valores de um método
As tuplas podem ser usadas para retornar vários valores de um método sem usar parâmetros out. No exemplo a seguir, `AddMultiply` é usado para retornar dois valores (soma, produto).

    void Write()
    {
        var result = AddMultiply(25, 28);
        Console.WriteLine(result.Item1);
        Console.WriteLine(result.Item2);
    }
 
    Tuple<int, int> AddMultiply(int a, int b)
    {
        return new Tuple<int, int>(a + b, a * b);
    }

Resultado:

> 53
> 700


Agora, o C# 7.0 oferece uma maneira alternativa de retornar vários valores de métodos usando tuplas de valor [Mais informações sobre a estrutura `ValueTuple`][1].


[1]: https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

