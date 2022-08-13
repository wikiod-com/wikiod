---
title: "Recursos do C# 3.0"
slug: "recursos-do-c-30"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

O C# versão 3.0 foi lançado como parte do .Net versão 3.5. Muitos dos recursos adicionados com esta versão eram compatíveis com LINQ (Language Integrated Queries).

Lista de recursos adicionados:

- LINQ
- Expressões lambda
- Métodos de extensão
- Tipos anônimos
- Variáveis ​​de tipo implícito
- Inicializadores de objetos e coleções
- Propriedades implementadas automaticamente
- Árvores de expressão

## Variáveis ​​de tipo implícito (var)
A palavra-chave `var` permite que um programador digite implicitamente uma variável em tempo de compilação. As declarações `var` têm o mesmo tipo que as variáveis ​​declaradas explicitamente.

    var squaredNumber = 10 * 10;
    var squaredNumberDouble = 10.0 * 10.0;
    var builder = new StringBuilder();
    var anonymousObject = new
    { 
        One = SquaredNumber,
        Two = SquaredNumberDouble,
        Three = Builder
    }

Os tipos das variáveis ​​acima são `int`, `double`, `StringBuilder` e um tipo anônimo respectivamente.

É importante notar que uma variável `var` não é tipada dinamicamente. `SquaredNumber = Builder` não é válido porque você está tentando definir um `int` para uma instância de `StringBuilder`

## Consultas integradas à linguagem (LINQ)
    //Example 1
    int[] array = { 1, 5, 2, 10, 7 };

    // Select squares of all odd numbers in the array sorted in descending order
    IEnumerable<int> query = from x in array
                             where x % 2 == 1
                             orderby x descending
                             select x * x;
    // Result: 49, 25, 1
[Exemplo do artigo da Wikipedia sobre C# 3.0, subseção LINQ][1]

O Exemplo 1 usa a sintaxe de consulta que foi projetada para ser semelhante às consultas SQL.

    //Example 2
    IEnumerable<int> query = array.Where(x => x % 2 == 1)
        .OrderByDescending(x => x)
        .Select(x => x * x);
    // Result: 49, 25, 1 using 'array' as defined in previous example
[Exemplo do artigo da Wikipedia sobre C# 3.0, subseção LINQ][1]

O exemplo 2 usa a sintaxe do método para obter o mesmo resultado do exemplo 1.

É importante observar que, em C#, a sintaxe de consulta LINQ é [açúcar sintático][2] para a sintaxe do método LINQ. O compilador traduz as consultas em chamadas de método em tempo de compilação. Algumas consultas devem ser expressas na sintaxe do método. [Do MSDN][3] - "Por exemplo, você deve usar uma chamada de método para expressar uma consulta que recupera o número de elementos que correspondem a uma condição especificada."


[1]: https://en.wikipedia.org/wiki/C_Sharp_3.0#LINQ_.28language-integrated_query.29
[2]: https://en.wikipedia.org/wiki/Syntactic_sugar
[3]: https://msdn.microsoft.com/en-us/library/bb397947.aspx

## Expressões lambda
As expressões lambda são uma extensão de [métodos anônimos][1] que permitem parâmetros de tipo implícito e valores de retorno. Sua sintaxe é menos detalhada do que métodos anônimos e segue um estilo de programação funcional.

    using System;
    using System.Collections.Generic;
    using System.Linq;
                        
    public class Program
    {
        public static void Main()
        {
            var numberList = new List<int> {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            var sumOfSquares = numberList.Select( number => number * number )
                .Aggregate( (int first, int second) => { return first + second; } );
            Console.WriteLine( sumOfSquares );
        }
    }

O código acima produzirá a soma dos quadrados dos números de 1 a 10 para o console.

A primeira expressão lambda eleva os números da lista ao quadrado. Como há apenas 1 parêntese de parâmetro, pode ser omitido. Você pode incluir parênteses se desejar:

    .Select( (number) => number * number);

ou digite explicitamente o parâmetro, mas os parênteses são necessários:

    .Select( (int number) => number * number);

O corpo lambda é uma expressão e tem um retorno implícito. Você pode usar um corpo de instrução se quiser também. Isso é útil para lambdas mais complexos.

    .Select( number => { return number * number; } );

O método select retorna um novo IEnumerable<int> com os valores calculados.

A segunda expressão lambda soma os números na lista retornados do método select. Os parênteses são obrigatórios, pois há vários parâmetros. Os tipos dos parâmetros são explicitamente tipados, mas isso não é necessário. O método abaixo é equivalente.

    .Aggregate( (first, second) => { return first + second; } );

Como é este:

    .Aggregate( (int first, int second) => first + second );

[1]: https://www.wikiod.com/pt/docs/c%23/60/methods/9338/anonymous-method#t=201608051345408629175

## Tipos anônimos
Os tipos anônimos fornecem uma maneira conveniente de encapsular um conjunto de propriedades somente leitura em um único objeto sem precisar definir explicitamente um tipo primeiro. O nome do tipo é gerado pelo compilador e não está disponível no nível do código-fonte. O tipo de cada propriedade é inferido pelo compilador.

Você pode criar tipos anônimos usando a palavra-chave `new` seguida por uma chave _(`{`)_. Dentro das chaves, você pode definir propriedades como no código abaixo.

    var v = new { Amount = 108, Message = "Hello" };

Também é possível criar uma matriz de tipos anônimos. Veja código abaixo:

    var a = new[] { 
        new { 
            Fruit = "Apple", 
            Color = "Red" 
        },
        new {
            Fruit = "Banana",
            Color = "Yellow"
        }
    };

Ou use-o com consultas LINQ:

    var productQuery = from prod in products
                       select new { prod.Color, prod.Price };

