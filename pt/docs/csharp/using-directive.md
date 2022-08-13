---
title: "Usando a Diretiva"
slug: "usando-a-diretiva"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

A palavra-chave `using` é tanto uma diretiva (este tópico) quanto uma declaração.

Para a instrução `using` (ou seja, para encapsular o escopo de um objeto `IDisposable`, garantindo que fora desse escopo o objeto seja descartado corretamente), consulte [Instrução Usando][1].


[1]: https://www.wikiod.com/pt/docs/c%23/38/using-statement

## Acessar membros estáticos de uma classe
<!-- if versão [gte 6.0] -->

Permite importar um tipo específico e usar os membros estáticos do tipo sem qualificá-los com o nome do tipo. Isso mostra um exemplo usando métodos estáticos:

    using static System.Console;

    // ...

    string GetName()
    {
        WriteLine("Enter your name.");
        return ReadLine();
    }

E isso mostra um exemplo usando propriedades e métodos estáticos:

    using static System.Math;

    namespace Geometry
    {
        public class Circle
        {
            public double Radius { get; set; };

            public double Area => PI * Pow(Radius, 2);
        }
    }

<!-- versão final if -->

## Associar um Alias ​​para Resolver Conflitos
Se você estiver usando vários namespaces que podem ter classes de mesmo nome (como `System.Random` e `UnityEngine.Random`), você pode usar um alias para especificar que `Random` vem de um ou de outro sem ter que usar todo o namespace na chamada.

Por exemplo:

    using UnityEngine;
    using System;

    Random rnd = new Random();

Isso fará com que o compilador não tenha certeza de qual `Random` avaliar a nova variável. Em vez disso, você pode fazer:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();

Isso não impede que você chame o outro pelo namespace totalmente qualificado, assim:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();
    int unityRandom = UnityEngine.Random.Range(0,100);

`rnd` será uma variável `System.Random` e `unityRandom` será uma variável `UnityEngine.Random`.

## Usando diretivas de alias
Você pode usar `using` para definir um alias para um namespace ou tipo. Mais detalhes podem ser encontrados em [aqui][1].

Sintaxe:

    using <identifier> = <namespace-or-type-name>;

Exemplo:

    using NewType = Dictionary<string, Dictionary<string,int>>;
    NewType multiDictionary = new NewType();
    //Use instances as you are using the original one
    multiDictionary.Add("test", new Dictionary<string,int>());
 


[1]: https://msdn.microsoft.com/en-us/library/aa664765(v=vs.71).aspx

## Uso básico
    using System;
    using BasicStuff = System;
    using Sayer = System.Console;
    using static System.Console;  //From C# 6
    
    class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Ignoring usings and specifying full type name");
            Console.WriteLine("Thanks to the 'using System' directive");
            BasicStuff.Console.WriteLine("Namespace aliasing");
            Sayer.WriteLine("Type aliasing");
            WriteLine("Thanks to the 'using static' directive (from C# 6)");
        }
    }



## Referenciar um namespace
    using System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //without prefixing them with the namespace.  i.e:

    //...
    var sb = new StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

## Associar um Alias ​​a um Namespace
    using st = System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //prefixing them with only the defined alias and not the full namespace.  i.e:

    //...
    var sb = new st.StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

