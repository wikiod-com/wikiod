---
title: "Função com vários valores de retorno"
slug: "funcao-com-varios-valores-de-retorno"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

Não há uma resposta inerente em C# para essa - assim chamada - necessidade. No entanto, existem soluções alternativas para satisfazer essa necessidade.

A razão pela qual qualifico a necessidade como "assim chamada" é que só precisamos de métodos com 2 ou mais de 2 valores para retornar quando violamos bons princípios de programação. Especialmente o [Princípio da Responsabilidade Única][1].

Portanto, seria melhor ser alertado quando precisamos de funções que retornem 2 ou mais valores e melhorar nosso design.


[1]: https://en.wikipedia.org/wiki/Single_responsibility_principle

## solução "objeto anônimo" + "palavra-chave dinâmica"
Você pode retornar um objeto anônimo de sua função

    public static object FunctionWithUnknowReturnValues ()
    {
        /// anonymous object
        return new { a = 1, b = 2 };
    }

E atribua o resultado a um objeto dinâmico e leia os valores nele.

    /// dynamic object
    dynamic x = FunctionWithUnknowReturnValues();

    Console.WriteLine(x.a);
    Console.WriteLine(x.b);

## Solução de tupla
Você pode retornar uma instância da classe `Tuple` da sua função com dois parâmetros de modelo como `Tuple<string, MyClass>`:

    public Tuple<string, MyClass> FunctionWith2ReturnValues ()
    {
        return Tuple.Create("abc", new MyClass());
    }

E leia os valores como abaixo:

    Console.WriteLine(x.Item1);
    Console.WriteLine(x.Item2);

## Parâmetros de referência e saída
A palavra-chave `ref` é usada para passar um [Argumento como Referência][1]. `out` fará o mesmo que `ref`, mas não requer um valor atribuído pelo chamador antes de chamar a função.

**Ref Parameter** :-Se você deseja passar uma variável como parâmetro ref, precisa inicializá-la antes de passá-la como parâmetro ref para o método.

**Parâmetro de saída:-**
Se você deseja passar uma variável como parâmetro de saída, não precisa inicializá-la antes de passá-la como parâmetro de saída para o método.

    static void Main(string[] args)
    {
        int a = 2;
        int b = 3;
        int add = 0;
        int mult= 0;
        AddOrMult(a, b, ref add, ref mult); //AddOrMult(a, b, out add, out mult);
        Console.WriteLine(add); //5
        Console.WriteLine(mult); //6
    }
    
    private static void AddOrMult(int a, int b, ref int add, ref int mult) //AddOrMult(int a, int b, out int add, out int mult)
    {
        add = a + b;
        mult = a * b;
    }


[1]: https://www.wikiod.com/pt/docs/c%23/3014/value-type-vs-reference-type#t=201607261617231313768

