---
title: "Consultas LINQ"
slug: "consultas-linq"
draft: false
images: []
weight: 6793
type: docs
toc: true
---

LINQ é um acrônimo que significa **L**anguage **IN**tegrated **Q**uery. É um conceito que integra uma linguagem de consulta, oferecendo um modelo consistente para trabalhar com dados em vários tipos de fontes e formatos de dados; você usa os mesmos padrões básicos de codificação para consultar e transformar dados em documentos XML, bancos de dados SQL, conjuntos de dados ADO.NET, coleções .NET e qualquer outro formato para o qual um provedor LINQ esteja disponível.

## Sintaxe
- Sintaxe da consulta:
    - from \<range variable\> in \<collection\>
    - [from \<range variable\> in \<collection\>, ...]
    - \<filter, joining, grouping, aggregate operators, ...\> \<lambda expression\>
    - \<select or groupBy operator\> \<formulate the result\>

- Sintaxe do método:

    - Enumerable.Aggregate(func)
    - Enumerable.Aggregate(seed, func)
    - Enumerable.Aggregate(seed, func, resultSelector)
    - Enumerable.All(predicate)
    - Enumerable.Any()
    - Enumerable.Any(predicate)
    - Enumerable.AsEnumerable()
    - Enumerable.Average()
    - Enumerable.Average(selector)
    - Enumerable.Cast\<Result\>()
    - Enumerable.Concat(second)
    - Enumerable.Contains(value)
    - Enumerable.Contains(value, comparer)
    - Enumerable.Count()
    - Enumerable.Count(predicate)
    - Enumerable.DefaultIfEmpty()
    - Enumerable.DefaultIfEmpty(defaultValue)
    - Enumerable.Distinct()
    - Enumerable.Distinct(comparer)
    - Enumerable.ElementAt(index)
    - Enumerable.ElementAtOrDefault(index)
    - Enumerable.Empty()
    - Enumerable.Except(second)
    - Enumerable.Except(second, comparer)
    - Enumerable.First()
    - Enumerable.First(predicate)
    - Enumerable.FirstOrDefault()
    - Enumerable.FirstOrDefault(predicate)
    - Enumerable.GroupBy(keySelector)
    - Enumerable.GroupBy(keySelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector)
    - Enumerable.GroupBy(keySelector, comparer)
    - Enumerable.GroupBy(keySelector, resultSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector, comparer)
    - Enumerable.Intersect(second)
    - Enumerable.Intersect(second, comparer)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
    - Enumerable.Last()
    - Enumerable.Last(predicate)
    - Enumerable.LastOrDefault()
    - Enumerable.LastOrDefault(predicate)
    - Enumerable.LongCount()
    - Enumerable.LongCount(predicate)
    - Enumerable.Max()
    - Enumerable.Max(selector)
    - Enumerable.Min()
    - Enumerable.Min(selector)
    - Enumerable.OfType\<TResult\>()
    - Enumerable.OrderBy(keySelector)
    - Enumerable.OrderBy(keySelector, comparer)
    - Enumerable.OrderByDescending(keySelector)
    - Enumerable.OrderByDescending(keySelector, comparer)
    - Enumerable.Range(start, count)
    - Enumerable.Repeat(element, count)
    - Enumerable.Reverse()
    - Enumerable.Select(selector)
    - Enumerable.SelectMany(selector)
    - Enumerable.SelectMany(collectionSelector, resultSelector)
    - Enumerable.SequenceEqual(second)
    - Enumerable.SequenceEqual(second, comparer)
    - Enumerable.Single()
    - Enumerable.Single(predicate)
    - Enumerable.SingleOrDefault()
    - Enumerable.SingleOrDefault(predicate)
    - Enumerable.Skip(count)
    - Enumerable.SkipWhile(predicate)
    - Enumerable.Sum()
    - Enumerable.Sum(selector)
    - Enumerable.Take(count)
    - Enumerable.TakeWhile(predicate)
    - orderedEnumerable.ThenBy(keySelector)
    - orderedEnumerable.ThenBy(keySelector, comparer)
    - orderedEnumerable.ThenByDescending(keySelector)
    - orderedEnumerable.ThenByDescending(keySelector, comparer)
    - Enumerable.ToArray()
    - Enumerable.ToDictionary(keySelector)
    - Enumerable.ToDictionary(keySelector, elementSelector)
    - Enumerable.ToDictionary(keySelector, comparer)
    - Enumerable.ToDictionary(keySelector, elementSelector, comparer)
    - Enumerable.ToList()
    - Enumerable.ToLookup(keySelector)
    - Enumerable.ToLookup(keySelector, elementSelector)
    - Enumerable.ToLookup(keySelector, comparer)
    - Enumerable.ToLookup(keySelector, elementSelector, comparer)
    - Enumerable.Union(second)
    - Enumerable.Union(second, comparer)
    - Enumerable.Where(predicate)
    - Enumerable.Zip(second, resultSelector)

Para usar consultas LINQ você precisa importar `System.Linq`.

A sintaxe do método é mais poderosa e flexível, mas a sintaxe da consulta pode ser mais simples e familiar. Todas as consultas escritas na sintaxe Query são traduzidas para a sintaxe funcional pelo compilador, portanto, o desempenho é o mesmo.

Os objetos de consulta não são avaliados até que sejam usados, portanto, eles podem ser alterados ou adicionados sem perda de desempenho.

## Métodos de encadeamento
[Muitas funções LINQ][1] operam em um `IEnumerable<TSource>` e também retornam um `IEnumerable<TResult>`. Os parâmetros de tipo `TSource` e `TResult` podem ou não se referir ao mesmo tipo, dependendo do método em questão e de quaisquer funções passadas para ele.

Alguns exemplos disso são

    public static IEnumerable<TResult> Select<TSource, TResult>(
        this IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    )

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source,
        Func<TSource, int, bool> predicate
    )

    public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(
        this IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector
    )

Embora alguns encadeamentos de métodos possam exigir que um conjunto inteiro seja trabalhado antes de prosseguir, o LINQ aproveita a [execução adiada](https://www.wikiod.com/pt/docs/c%23/68/linq-queries/8001/deferred- execução) usando [yield return <sup>**MSDN**</sup>](https://blogs.msdn.microsoft.com/oldnewthing/20080812-00/?p=21273/) que cria um Enumerable e um enumerador nos bastidores. O processo de encadeamento no LINQ é essencialmente construir um enumerável (iterador) para o conjunto original - que é adiado - até materializado por [enumerar o enumerável](https://www.wikiod.com/pt/docs/c%23/68/ linq-queries/17356/enumerating-the-enumerable).

Isso permite que essas funções sejam [ <sup>**wiki**</sup> encadeadas fluentemente](https://en.wikipedia.org/wiki/Fluent_interface), onde uma função pode atuar diretamente no resultado de outra. Esse estilo de código pode ser usado para executar muitas operações baseadas em sequência em uma única instrução.

Por exemplo, é possível combinar `Select`, `Where` e `OrderBy` para transformar, filtrar e classificar uma sequência em uma única instrução.

    var someNumbers = { 4, 3, 2, 1 };

    var processed = someNumbers
            .Select(n => n * 2)   // Multiply each number by 2
            .Where(n => n != 6)   // Keep all the results, except for 6
            .OrderBy(n => n);     // Sort in ascending order

**Resultado:**
>2
>4
>8

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/3Gta8X)

Quaisquer funções que estendem e retornam o tipo genérico `IEnumerable<T>` podem ser usadas como cláusulas encadeadas em uma única instrução. Esse estilo de programação fluente é poderoso e deve ser considerado ao criar seus próprios [métodos de extensão][2].


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx
[2]: https://www.wikiod.com/pt/docs/c%23/20/extension-methods#t=201607220826369208865

## First, FirstOrDefault, Last, LastOrDefault, Single e SingleOrDefault
Todos os seis métodos retornam um único valor do tipo de sequência e podem ser chamados com ou sem um predicado.

Dependendo do número de elementos que correspondem ao `predicate` ou, se nenhum `predicate` for fornecido, o número de elementos na sequência de origem, eles se comportam da seguinte forma:

# Primeiro()

* Retorna o primeiro elemento de uma sequência ou o primeiro elemento que corresponde ao `predicado` fornecido.
* Se a sequência não contiver elementos, uma `InvalidOperationException` será lançada com a mensagem: "A sequência não contém elementos".
* Se a sequência não contiver elementos correspondentes ao `predicate` fornecido, uma `InvalidOperationException` será lançada com a mensagem "A sequência não contém nenhum elemento correspondente".

**Exemplo**

    // Returns "a":
    new[] { "a" }.First();
    
    // Returns "a":
    new[] { "a", "b" }.First();
    
    // Returns "b":
    new[] { "a", "b" }.First(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new string[0].First();

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/ESYLcU)


# FirstOrDefault()

* Retorna o primeiro elemento de uma sequência ou o primeiro elemento que corresponde ao `predicado` fornecido.
* Se a sequência não contiver elementos ou nenhum elemento que corresponda ao `predicado` fornecido, retornará o valor padrão do tipo de sequência usando [`default(T)`](https://www.wikiod.com/pt/docs/c%23/ 26/palavras-chave/109/default#t=201702071640321629621).

**Exemplo**

    // Returns "a":
    new[] { "a" }.FirstOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.FirstOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.FirstOrDefault(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new string[0].FirstOrDefault();

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/XJ93lr)


# Último()

* Retorna o último elemento de uma sequência ou o último elemento correspondente ao `predicado` fornecido.
* Se a sequência não contiver elementos, uma `InvalidOperationException` será lançada com a mensagem "A sequência não contém elementos".
* Se a sequência não contiver elementos correspondentes ao `predicate` fornecido, uma `InvalidOperationException` será lançada com a mensagem "A sequência não contém nenhum elemento correspondente".

**Exemplo**

    // Returns "a":
    new[] { "a" }.Last();
    
    // Returns "b":
    new[] { "a", "b" }.Last();
    
    // Returns "a":
    new[] { "a", "b" }.Last(x => x.Equals("a"));
    
    // Returns "be":
    new[] { "ba", "be" }.Last(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.Last(x => x.Contains("b"));

    // Throws InvalidOperationException:
    new string[0].Last(); 


# LastOrDefault()

* Retorna o último elemento de uma sequência ou o último elemento correspondente ao `predicado` fornecido.
* Se a sequência não contiver elementos ou nenhum elemento que corresponda ao `predicado` fornecido, retornará o valor padrão do tipo de sequência usando `default(T)`.

**Exemplo**

    // Returns "a":
    new[] { "a" }.LastOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.LastOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.LastOrDefault(x => x.Equals("a"));
    
     // Returns "be":
    new[] { "ba", "be" }.LastOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.LastOrDefault(x => x.Contains("b")); 
    
    // Returns null:
    new string[0].LastOrDefault();


# Solteiro()

* Se a sequência contiver exatamente um elemento, ou exatamente um elemento que corresponda ao `predicado` fornecido, esse elemento será retornado.
* Se a sequência não contiver elementos ou nenhum elemento que corresponda ao `predicate` fornecido, uma `InvalidOperationException` será lançada com a mensagem "A sequência não contém elementos".
* Se a sequência contiver mais de um elemento ou mais de um elemento que corresponda ao `predicado` fornecido, uma `InvalidOperationException` será lançada com a mensagem "A sequência contém mais de um elemento".
* __Nota:__ para avaliar se a sequência contém exatamente um elemento, no máximo dois elementos devem ser enumerados.

**Exemplo**

    // Returns "a":
    new[] { "a" }.Single();
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "b" }.Single();
    
    // Returns "b":
    new[] { "a", "b" }.Single(x => x.Equals("b"));
    
    // Throws InvalidOperationException:
    new[] { "a", "b" }.Single(x => x.Equals("c"));
    
    // Throws InvalidOperationException:
    new string[0].Single(); 
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "a" }.Single();


# SingleOrDefault()
* Se a sequência contiver exatamente um elemento, ou exatamente um elemento que corresponda ao `predicado` fornecido, esse elemento será retornado.
* Se a sequência não contiver elementos ou nenhum elemento que corresponda ao `predicado` fornecido, será retornado `default(T)`.
* Se a sequência contiver mais de um elemento ou mais de um elemento que corresponda ao `predicado` fornecido, uma `InvalidOperationException` será lançada com a mensagem "A sequência contém mais de um elemento".
* Se a sequência não contiver elementos que correspondam ao `predicado` fornecido, retorna o valor padrão do tipo de sequência usando `default(T)`.
* __Nota:__ para avaliar se a sequência contém exatamente um elemento, no máximo dois elementos devem ser enumerados.

**Exemplo**

    // Returns "a":
    new[] { "a" }.SingleOrDefault();

    // returns "a"
    new[] { "a", "b" }.SingleOrDefault(x => x == "a"); 

    // Returns null:
    new[] { "a", "b" }.SingleOrDefault(x => x == "c");

    // Throws InvalidOperationException:
    new[] { "a", "a" }.SingleOrDefault(x => x == "a");

    // Throws InvalidOperationException:
    new[] { "a", "b" }.SingleOrDefault();

    // Returns null:
    new string[0].SingleOrDefault();

# Recomendações

- Embora você possa usar `FirstOrDefault`, `LastOrDefault` ou `SingleOrDefault` para verificar se uma sequência contém algum item, `Any` ou `Count` são mais confiáveis. Isso ocorre porque um valor de retorno de `default(T)` de um desses três métodos não prova que a sequência está vazia, pois o valor do primeiro/último/único elemento da sequência pode ser igualmente `default(T )`

- Decida quais métodos se adequam mais ao propósito do seu código. Por exemplo, use `Single` somente se precisar garantir que haja um único item na coleção que corresponda ao seu predicado &mdash; caso contrário, use `Primeiro`; como `Single` lança uma exceção se a sequência tiver mais de um elemento correspondente. Obviamente, isso também se aplica às contrapartes "*OrDefault".

- Em relação à eficiência: Embora muitas vezes seja apropriado garantir que haja apenas um item (`Single`) ou apenas um ou zero (`SingleOrDefault`) itens, retornados por uma consulta, ambos os métodos exigem mais, e muitas vezes o totalidade, da coleção a ser examinada para garantir que não haja uma segunda correspondência com a consulta. Isso é diferente do comportamento, por exemplo, do método `First`, que pode ser satisfeito após encontrar a primeira correspondência.

## Exceto
O método Except retorna o conjunto de itens que estão contidos na primeira coleção, mas não estão contidos na segunda. O padrão [`IEqualityComparer`][1] é usado para comparar os itens dentro dos dois conjuntos. Há uma sobrecarga que aceita um [`IEqualityComparer`][1] como argumento.

**Exemplo:**

    int[] first = { 1, 2, 3, 4 };
    int[] second = { 0, 2, 3, 5 };
    
    IEnumerable<int> inFirstButNotInSecond = first.Except(second);
    // inFirstButNotInSecond = { 1, 4 }

**Resultado:**
>1
>4
 
[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/m3EqTQ)

Neste caso `.Except(second)` exclui elementos contidos no array `second`, ou seja, 2 e 3 (0 e 5 não estão contidos no array `first` e são ignorados).

Observe que `Except` implica `Distinct` (ou seja, remove elementos repetidos). Por exemplo:
    
    int[] third = { 1, 1, 1, 2, 3, 4 };
    
    IEnumerable<int> inThirdButNotInSecond = third.Except(second);
    // inThirdButNotInSecond = { 1, 4 }

**Resultado:**
>1
>4

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/VlXBUp)

Nesse caso, os elementos 1 e 4 são retornados apenas uma vez.

---

Implementar [`IEquatable`][2] ou fornecer a função um [`IEqualityComparer`][1] permitirá usar um método diferente para comparar os elementos.
Observe que o método [`GetHashCode`][3] também deve ser substituído para que retorne um código hash idêntico para `object` que seja idêntico de acordo com a implementação [`IEquatable`][2].

***Exemplo com IEquatable:***

    class Holiday : IEquatable<Holiday>
    {
        public string Name { get; set; }
    
        public bool Equals(Holiday other)
        {
            return Name == other.Name;
        }
    
        // GetHashCode must return true whenever Equals returns true.
        public override int GetHashCode()
        {
            //Get hash code for the Name field if it is not null.
            return Name?.GetHashCode() ?? 0;
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            List<Holiday> holidayDifference = new List<Holiday>();

            List<Holiday> remoteHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Hanukkah" },
                new Holiday { Name = "Ramadan" }
            };

            List<Holiday> localHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Ramadan" }
            };

            holidayDifference = remoteHolidays
                .Except(localHolidays)
                .ToList();

            holidayDifference.ForEach(x => Console.WriteLine(x.Name));
        }
    }

Resultado:

> Hanukkah

[Demonstração ao vivo no .NET Fiddle][4]


[1]: https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.object.gethashcode(v=vs.110).aspx
[4]: https://dotnetfiddle.net/9ilGqy

## Selecionar muitos
O método linq SelectMany 'achata' um `IEnumerable<IEnumerable<T>>` em um `IEnumerable<T>`. Todos os elementos T dentro das instâncias `IEnumerable` contidas na fonte `IEnumerable` serão combinados em um único `IEnumerable`.

    var words = new [] { "a,b,c", "d,e", "f" };
    var splitAndCombine = words.SelectMany(x => x.Split(','));
    // returns { "a", "b", "c", "d", "e", "f" }

Se você usar uma função seletora que transforma elementos de entrada em sequências, o resultado será os elementos dessas sequências retornados um a um.

Observe que, ao contrário de `Select()`, o número de elementos na saída não precisa ser o mesmo que na entrada.

**Mais exemplos do mundo real**

    class School
    {
        public Student[] Students { get; set; }
    }
    
    class Student 
    {
        public string Name { get; set; }
    }    
      
    var schools = new [] {
        new School(){ Students = new [] { new Student { Name="Bob"}, new Student { Name="Jack"} }},
        new School(){ Students = new [] { new Student { Name="Jim"}, new Student { Name="John"} }}
    };
                   
    var allStudents = schools.SelectMany(s=> s.Students);
                 
    foreach(var student in allStudents)
    {
        Console.WriteLine(student.Name);
    }

Resultado:

>Bob
>Jack
>Jim
    John

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/LNyymI)

## Algum
`Any` é usado para verificar se **qualquer** elemento de uma coleção corresponde a uma condição ou não.
<br/>*consulte também: [.All](https://www.wikiod.com/pt/docs/c%23/68/linq-queries/2773/all#t=201707041340119289445), [Any and FirstOrDefault: best practice] (https://www.wikiod.com/pt/docs/c%23/68/linq-queries/16731/any-and-firstordefault-best-practice#t=201707041441456087738)*

## 1. Parâmetro vazio ##
**Qualquer**: Retorna `true` se a coleção tiver algum elemento e `false` se a coleção estiver vazia:

    var numbers = new List<int>();
    bool result = numbers.Any(); // false

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.Any(); //true

## 2. Expressão lambda como parâmetro ##
**Qualquer**: Retorna `true` se a coleção tiver um ou mais elementos que atendam à condição na expressão lambda:

    var arrayOfStrings = new string[] { "a", "b", "c" };
    arrayOfStrings.Any(item => item == "a");    // true
    arrayOfStrings.Any(item => item == "d");    // false
    
## 3. Coleção vazia ##
**Qualquer**: Retorna `false` se a coleção estiver vazia e uma expressão lambda for fornecida:
  
    var numbers = new List<int>();
    bool result = numbers.Any(i => i >= 0); // false

**Observação:**
`Any` interromperá a iteração da coleção assim que encontrar um elemento que corresponda à condição. Isso significa que a coleção não será necessariamente enumerada por completo; ele só será enumerado o suficiente para encontrar o primeiro item que corresponda à condição.

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/IQ4wG4)


## JUNTAS
As junções são usadas para combinar diferentes listas ou tabelas que contêm dados por meio de uma chave comum.

Assim como no SQL, os seguintes tipos de junções são suportados no LINQ: <br/>
**junções internas, esquerdas, direitas, cruzadas** e **externas completas**.

As duas listas a seguir são usadas nos exemplos abaixo:

    var first = new List<string>(){ "a","b","c"}; // Left data
    var second = new List<string>(){ "a", "c", "d"}; // Right data

## (Junção interna

    var result = from f in first
                 join s in second on f equals s
                 select new { f, s };

    var result = first.Join(second, 
                            f => f, 
                            s => s,
                            (f, s) => new { f, s });

    // Result: {"a","a"}
    //         {"c","c"}

## Junção externa esquerda
    var leftOuterJoin = from f in first
                        join s in second on f equals s into temp
                        from t in temp.DefaultIfEmpty()
                        select new { First = f, Second = t};

    // Or can also do:
    var leftOuterJoin = from f in first
                        from s in second.Where(x => x == f).DefaultIfEmpty()
                        select new { First = f, Second = s};

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  


    // Left outer join method syntax
    var leftOuterJoinFluentSyntax = first.GroupJoin(second,
                                          f => f,
                                          s => s,
                                          (f, s) => new { First = f, Second = s })
                                       .SelectMany(temp => temp.Second.DefaultIfEmpty(),
                                          (f, s) => new { First = f.First, Second = s });




## Junção Externa Direita
    var rightOuterJoin = from s in second
                         join f in first on s equals f into temp
                         from t in temp.DefaultIfEmpty()
                         select new {First=t,Second=s};

    // Result: {"a","a"}
    //         {"c","c"}  
    //         {null,"d"}  


## União cruzada

    var CrossJoin = from f in first
                    from s in second
                    select new { f, s };

    // Result: {"a","a"}
    //         {"a","c"}  
    //         {"a","d"}  
    //         {"b","a"}
    //         {"b","c"}  
    //         {"b","d"}  
    //         {"c","a"}
    //         {"c","c"}  
    //         {"c","d"}

## Junção Externa Completa

    var fullOuterjoin = leftOuterJoin.Union(rightOuterJoin);

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  
    //         {null,"d"}

## **Exemplo prático** ##
Os exemplos acima têm uma estrutura de dados simples para que você possa se concentrar em entender tecnicamente as diferentes junções de LINQ, mas no mundo real você teria tabelas com colunas que você precisa para unir.

No exemplo a seguir, há apenas uma classe `Region` usada, na realidade você juntaria duas ou mais tabelas diferentes que possuem a mesma chave (neste exemplo `first` e `second` são unidos através da chave comum `ID` ).

**Exemplo:** considere a seguinte estrutura de dados:

    public class Region 
    {
        public Int32 ID;
        public string RegionDescription;
        
        public Region(Int32 pRegionID, string pRegionDescription=null)
        {
            ID = pRegionID; RegionDescription = pRegionDescription;
        }
    }

Agora prepare os dados (ou seja, preencha com dados):

    // Left data
    var first = new List<Region>() 
                     { new Region(1), new Region(3), new Region(4) }; 
    // Right data
    var second = new List<Region>() 
                     { 
                        new Region(1, "Eastern"),  new Region(2, "Western"),
                        new Region(3, "Northern"), new Region(4, "Southern")
                     }; 

Você pode ver que neste exemplo `first` não contém nenhuma descrição de região, então você deseja juntá-las a partir de `second`. Então a junção interna ficaria assim:

    // do the inner join
    var result = from f in first
                 join s in second on f.ID equals s.ID
                 select new { f.ID, s.RegionDescription };
 

     // Result: {1,"Eastern"}
     //         {3, Northern}  
     //         {4,"Southern"}  

Este resultado criou objetos anônimos em tempo real, o que é bom, mas já criamos uma classe adequada - então podemos especificá-la: Em vez de `select new { f.ID, s.RegionDescription };` podemos dizer `select new Region(f.ID, s.RegionDescription);`, que retornará os mesmos dados, mas criará objetos do tipo `Region` - que manterá a compatibilidade com os outros objetos.



[Demonstração ao vivo no violino .NET](https://dotnetfiddle.net/pP6enP)


## Pular e pegar
O método Skip retorna uma coleção excluindo vários itens do início da coleção de origem. O número de itens excluídos é o número fornecido como argumento. Se houver menos itens na coleção do que o especificado no argumento, uma coleção vazia será retornada.

O método Take retorna uma coleção contendo vários elementos desde o início da coleção de origem. O número de itens incluídos é o número fornecido como argumento. Se houver menos itens na coleção do que o especificado no argumento, a coleção retornada conterá os mesmos elementos que a coleção de origem.

    var values = new [] { 5, 4, 3, 2, 1 };

    var skipTwo        = values.Skip(2);         // { 3, 2, 1 }
    var takeThree      = values.Take(3);         // { 5, 4, 3 }
    var skipOneTakeTwo = values.Skip(1).Take(2); // { 4, 3 }
    var takeZero       = values.Take(0);         // An IEnumerable<int> with 0 items

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/U2b76y)

**Skip and Take** são comumente usados ​​juntos para paginar resultados, por exemplo:

    IEnumerable<T> GetPage<T>(IEnumerable<T> collection, int pageNumber, int resultsPerPage) {
        int startIndex = (pageNumber - 1) * resultsPerPage;
        return collection.Skip(startIndex).Take(resultsPerPage);
    }


> **Aviso:** LINQ to Entities suporta apenas Skip on [consultas ordenadas][1]. Se você tentar usar Skip sem fazer o pedido, receberá uma **NotSupportedException** com a mensagem "O método 'Skip' só é suportado para entrada classificada em LINQ to Entities. O método 'OrderBy' deve ser chamado antes do método 'Skip '."


[1]: https://www.wikiod.com/pt/docs/c%23/68/linq-queries/4389/query-ordering#t=201607261110520529272

## Definindo uma variável dentro de uma consulta Linq (let palavra-chave)
Para definir uma variável dentro de uma expressão linq, você pode usar a palavra-chave **let**. Isso geralmente é feito para armazenar os resultados de subconsultas intermediárias, por exemplo:

     int[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

     var aboveAverages = from number in numbers
                         let average = numbers.Average()
                         let nSquared = Math.Pow(number,2)
                         where nSquared > average
                         select number;

     Console.WriteLine("The average of the numbers is {0}.", numbers.Average());

     foreach (int n in aboveAverages)
     {
       Console.WriteLine("Query result includes number {0} with square of {1}.", n, Math.Pow(n,2));
     }

**Resultado:**

>A média dos números é de 4,5.
>O resultado da consulta inclui o número 3 com quadrado de 9.
>O resultado da consulta inclui o número 4 com quadrado de 16.
>O resultado da consulta inclui o número 5 com quadrado de 25.
>O resultado da consulta inclui o número 6 com quadrado de 36.
>O resultado da consulta inclui o número 7 com quadrado de 49.
>O resultado da consulta inclui o número 8 com quadrado de 64.
>O resultado da consulta inclui o número 9 com quadrado de 81.

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/zbjrHZ

## Fecho eclair
O método de extensão `Zip` atua em duas coleções. Ele emparelha cada elemento nas duas séries com base na posição. Com uma instância `Func`, usamos `Zip` para manipular elementos das duas coleções C# em pares. Se a série diferir em tamanho, os elementos extras da série maior serão ignorados.

Para pegar um exemplo do livro "C# in a Nutshell",

    int[] numbers = { 3, 5, 7 };
    string[] words = { "three", "five", "seven", "ignored" };
    IEnumerable<string> zip = numbers.Zip(words, (n, w) => n + "=" + w);

**Resultado:**

>3=três
>5=cinco
>7=sete

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/nIA5E9

## Intervalo e Repetição
Os métodos estáticos `Range` e `Repeat` em `Enumerable` podem ser usados ​​para gerar sequências simples.

**Variar**
---------

`Enumerable.Range()` gera uma seqüência de inteiros com um valor inicial e uma contagem.

    // Generate a collection containing the numbers 1-100 ([1, 2, 3, ..., 98, 99, 100])
    var range = Enumerable.Range(1,100);

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/jA0VB1)

**Repetir**
------

`Enumerable.Repeat()` gera uma sequência de elementos repetidos dado um elemento e o número de repetições necessárias.

    // Generate a collection containing "a", three times (["a","a","a"])
    var repeatedValues = Enumerable.Repeat("a", 3);

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/KpZfpt)

## Tudo
`All` é usado para verificar se todos os elementos de uma coleção correspondem a uma condição ou não.
<br/>*veja também: [.Any](https://www.wikiod.com/pt/docs/c%23/68/linq-queries/5098/any#t=201707041342119744775)*
## 1. Parâmetro vazio ##

**All**: não pode ser usado com parâmetro vazio.

## 2. Expressão lambda como parâmetro ##

**All**: Retorna `true` se todos os elementos da coleção satisfizerem a expressão lambda e `false` caso contrário:

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.All(i => i < 10); // true
    bool result = numbers.All(i => i >= 3); // false

## 3. Coleção vazia ##

**All**: Retorna `true` se a coleção estiver vazia e uma expressão lambda for fornecida:

    var numbers = new List<int>();
    bool result = numbers.All(i => i >= 0); // true

**Observação:**
`All` interromperá a iteração da coleção assim que encontrar um elemento **não** que corresponda à condição. Isso significa que a coleção não será necessariamente enumerada por completo; ele só será enumerado o suficiente para encontrar o primeiro item **não correspondendo** à condição.


## Fundamentos
O LINQ é amplamente benéfico para consultar coleções (ou matrizes).

Por exemplo, dados os seguintes dados de exemplo:

    var classroom = new Classroom
    {
        new Student { Name = "Alice", Grade = 97, HasSnack = true  },
        new Student { Name = "Bob",   Grade = 82, HasSnack = false },
        new Student { Name = "Jimmy", Grade = 71, HasSnack = true  },
        new Student { Name = "Greg",  Grade = 90, HasSnack = false },
        new Student { Name = "Joe",   Grade = 59, HasSnack = false }
    }

Podemos "consultar" esses dados usando a sintaxe LINQ. Por exemplo, para recuperar todos os alunos que fizeram um lanche hoje:

    var studentsWithSnacks = from s in classroom.Students
                             where s.HasSnack
                             select s;

Ou, para recuperar alunos com nota igual ou superior a 90 e retornar apenas seus nomes, não o objeto `Student` completo:

    var topStudentNames = from s in classroom.Students
                          where s.Grade >= 90
                          select s.Name;

O recurso LINQ é composto por duas sintaxes que executam as mesmas funções, têm desempenho quase idêntico, mas são escritas de maneira muito diferente. A sintaxe no exemplo acima é chamada de **sintaxe de consulta**. O exemplo a seguir, no entanto, ilustra a **sintaxe do método**. Os mesmos dados serão retornados como no exemplo acima, mas a forma como a consulta é escrita é diferente.

    var topStudentNames = classroom.Students
                                   .Where(s => s.Grade >= 90)
                                   .Select(s => s.Name);
                                        

## Agregado
`Aggregate` Aplica uma função de acumulador sobre uma sequência.
 
    int[] intList = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    int sum = intList.Aggregate((prevSum, current) => prevSum + current);
    // sum = 55

- Na primeira etapa `prevSum = 1`
- No segundo `prevSum = prevSum(at
o primeiro passo) + 2`
- No i-ésimo passo `prevSum = prevSum(no (i-1)
step) + i-ésimo elemento do array`


    string[] stringList = { "Hello", "World", "!" };
    string joinedString = stringList.Aggregate((prev, current) => prev + " " + current);
    // joinedString = "Hello World !"


----------

Uma segunda sobrecarga de `Aggregate` também recebe um parâmetro `seed` que é o valor inicial do acumulador. Isso pode ser usado para calcular várias condições em uma coleção sem iterar mais de uma vez.

    List<int> items = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

Para a coleção de `itens` queremos calcular

1. O total de `.Contagem`
2. A quantidade de números pares
3. Colete cada quarto item

Usando `Aggregate` pode ser feito assim:

    var result = items.Aggregate(new { Total = 0, Even = 0, FourthItems = new List<int>() },
                    (accumelative,item) =>
                    new {
                        Total = accumelative.Total + 1,
                        Even = accumelative.Even + (item % 2 == 0 ? 1 : 0),
                        FourthItems = (accumelative.Total + 1)%4 == 0 ? 
                            new List<int>(accumelative.FourthItems) { item } : 
                            accumelative.FourthItems 
                    });
    // Result:
    // Total = 12
    // Even = 6
    // FourthItems = [4, 8, 12]

_Observe que usando um tipo anônimo como semente é preciso instanciar um novo objeto a cada item porque as propriedades são somente leitura. Usando uma classe personalizada, pode-se simplesmente atribuir as informações e nenhum `novo` é necessário (somente ao fornecer o parâmetro inicial `seed`_

## SelectMany: achatando uma sequência de sequências
    var sequenceOfSequences = new [] { new [] { 1, 2, 3 }, new [] { 4, 5 }, new [] { 6 } };
    var sequence = sequenceOfSequences.SelectMany(x => x);
    // returns { 1, 2, 3, 4, 5, 6 }

Use `SelectMany()` se tiver, ou se estiver criando uma sequência de sequências, mas deseja o resultado como uma sequência longa.

Na sintaxe de consulta LINQ:

    var sequence = from subSequence in sequenceOfSequences
                   from item in subSequence
                   select item;

Se você tem uma coleção de coleções e gostaria de poder trabalhar com dados da coleção pai e filha ao mesmo tempo, também é possível com `SelectMany`.

Vamos definir classes simples

    public class BlogPost
    {
        public int Id { get; set; }
        public string Content { get; set; }
        public List<Comment> Comments { get; set; }
    }

    public class Comment
    {
        public int Id { get; set; }
        public string Content { get; set; }
    }

Vamos supor que temos a seguinte coleção.

    List<BlogPost> posts = new List<BlogPost>()
    {
        new BlogPost()
        {
            Id = 1,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 1,
                    Content = "It's really great!",
                },
                new Comment()
                {
                    Id = 2,
                    Content = "Cool post!"
                }
            }
        },
        new BlogPost()
        {
            Id = 2,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 3,
                    Content = "I don't think you're right",
                },
                new Comment()
                {
                    Id = 4,
                    Content = "This post is a complete nonsense"
                }
            }
        }
    };

Agora queremos selecionar os comentários `Content` junto com o `Id` do `BlogPost` associado a este comentário. Para fazer isso, podemos usar a sobrecarga `SelectMany` apropriada.

    var commentsWithIds = posts.SelectMany(p => p.Comments, (post, comment) => new { PostId = post.Id, CommentContent = comment.Content });

Nosso `commentsWithIds` se parece com isso

    {
        PostId = 1,
        CommentContent = "It's really great!"
    },
    {
        PostId = 1,
        CommentContent = "Cool post!"
    },
    {
        PostId = 2,
        CommentContent = "I don't think you're right"
    },
    {
        PostId = 2,
        CommentContent = "This post is a complete nonsense"
    }




## Distinto
Retorna valores exclusivos de um `IEnumerable`. A exclusividade é determinada usando o comparador de igualdade padrão.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

    var distinct = array.Distinct();
    // distinct = { 1, 2, 3, 4, 5 }

Para comparar um tipo de dados personalizado, precisamos implementar a interface `IEquatable<T>` e fornecer os métodos `GetHashCode` e `Equals` para o tipo. Ou o comparador de igualdade pode ser substituído:

    class SSNEqualityComparer : IEqualityComparer<Person> {
        public bool Equals(Person a, Person b) => return a.SSN == b.SSN;
        public int GetHashCode(Person p) => p.SSN;
    }

    List<Person> people;

    distinct = people.Distinct(SSNEqualityComparer);

## Coleção de consultas por tipo/elementos de conversão para tipo
    interface IFoo { }
    class Foo : IFoo { }
    class Bar : IFoo { }

----------
    var item0 = new Foo();
    var item1 = new Foo();
    var item2 = new Bar();
    var item3 = new Bar();
    var collection = new IFoo[] { item0, item1, item2, item3 };

Usando `OfType`

    var foos = collection.OfType<Foo>(); // result: IEnumerable<Foo> with item0 and item1
    var bars = collection.OfType<Bar>(); // result: IEnumerable<Bar> item item2 and item3
    var foosAndBars = collection.OfType<IFoo>(); // result: IEnumerable<IFoo> with all four items

Usando `Onde`

    var foos = collection.Where(item => item is Foo); // result: IEnumerable<IFoo> with item0 and item1
    var bars = collection.Where(item => item is Bar); // result: IEnumerable<IFoo> with item2 and item3

Usando `Cast`

    var bars = collection.Cast<Bar>();                // throws InvalidCastException on the 1st item
    var foos = collection.Cast<Foo>();                // throws InvalidCastException on the 3rd item
    var foosAndBars = collection.Cast<IFoo>();        // OK 
    

## Agrupar por
GroupBy é uma maneira fácil de classificar uma coleção `IEnumerable<T>` de itens em grupos distintos.
## Exemplo Simples ##
Neste primeiro exemplo, acabamos com dois grupos, itens ímpares e pares.

    List<int> iList = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var grouped = iList.GroupBy(x => x % 2 == 0);

    //Groups iList into odd [13579] and even[2468] items 
           
    foreach(var group in grouped)
    {
        foreach (int item in group)
        {
            Console.Write(item); // 135792468  (first odd then even)
        }
    }

## Exemplo mais complexo ##

Tomemos como exemplo o agrupamento de uma lista de pessoas por idade.
Primeiro, vamos criar um objeto Person que tem duas propriedades, Name e Age.

    public class Person
    {
        public int Age {get; set;}
        public string Name {get; set;}
    }

Em seguida, criamos nossa lista de amostra de pessoas com vários nomes e idades.

    List<Person> people = new List<Person>();
    people.Add(new Person{Age = 20, Name = "Mouse"});
    people.Add(new Person{Age = 30, Name = "Neo"});
    people.Add(new Person{Age = 40, Name = "Morpheus"});
    people.Add(new Person{Age = 30, Name = "Trinity"});
    people.Add(new Person{Age = 40, Name = "Dozer"});
    people.Add(new Person{Age = 40, Name = "Smith"});

Em seguida, criamos uma consulta LINQ para agrupar nossa lista de pessoas por idade.

    var query = people.GroupBy(x => x.Age);

Fazendo isso, podemos ver a Idade de cada grupo e ter uma lista de cada pessoa do grupo.

    foreach(var result in query)
    {
        Console.WriteLine(result.Key);
                    
        foreach(var person in result)
            Console.WriteLine(person.Name);
    }

Isso resulta na seguinte saída:

    20
    Mouse
    30
    Neo
    Trinity
    40
    Morpheus
    Dozer
    Smith

Você pode jogar com a [demonstração ao vivo no .NET Fiddle][1]


[1]: https://dotnetfiddle.net/VFOZ1x

## Enumerando o enumerável
A interface IEnumerable<<a>T> é a interface base para todos os enumeradores genéricos e é uma parte essencial da compreensão do LINQ. Em sua essência, representa a sequência.

Essa interface subjacente é herdada por todas as coleções genéricas, como [Collection<<a>T>](https://msdn.microsoft.com/en-us/library/ms132397(v=vs.110).aspx ), [Array](https://msdn.microsoft.com/en-us/library/system.array(v=vs.110).aspx), [Lista<<a>T>](https:// msdn.microsoft.com/en-us/library/6sh2ey19(v=vs.110).aspx), [Dictionary<TKey, TValue> Class](https://msdn.microsoft.com/en-us/library/ xfhwa508(v=vs.110).aspx) e [HashSet<<a>T>](https://msdn.microsoft.com/en-us/library/bb359438(v=vs.110).aspx) .

Além de representar a sequência, qualquer classe que herda de IEnumerable<<a>T> deve fornecer um IEnumerator<<a>T>. O enumerador expõe o iterador para o enumerável, e essas duas interfaces e ideias interconectadas são a fonte do ditado "enumere o enumerable".

"Enumerar o enumerável" é uma frase importante. O enumerable é simplesmente uma estrutura de como iterar, não contém nenhum objeto materializado. Por exemplo, ao ordenar, um enumerable pode conter os critérios do campo para ordenar, mas usar `.OrderBy()` em si retornará um IEnumerable<<a>T> que só sabe *como* classificar. Usar uma chamada que irá materializar os objetos, como na iteração do conjunto, é conhecido como enumeração (por exemplo `.ToList()`). O processo de enumeração usará a definição enumerável de *como* para percorrer a série e retornar os objetos relevantes (em ordem, filtrados, projetados etc.).

Somente depois que o enumerável é enumerado, ele causa a materialização dos objetos, que é quando métricas como [complexidade de tempo](https://en.wikipedia.org/wiki/Time_complexity) (quanto tempo deve levar em relação ao tamanho da série ) e a complexidade espacial (quanto espaço ele deve usar em relação ao tamanho da série) podem ser medidos.

Criar sua própria classe que herda de IEnumerable<<a>T> pode ser um pouco complicado dependendo da série subjacente que precisa ser enumerável. Em geral, é melhor usar uma das coleções genéricas existentes. Dito isso, também é possível herdar da interface IEnumerable<<a>T> sem ter uma matriz definida como estrutura subjacente.

Por exemplo, usando a série de Fibonacci como a sequência subjacente. Observe que a chamada para `Where` simplesmente cria um `IEnumerable`, e não é até uma chamada para enumerate que enumerable é feita que qualquer um dos valores é materializado.

    void Main()
    {
        Fibonacci Fibo = new Fibonacci();
        IEnumerable<long> quadrillionplus = Fibo.Where(i => i > 1000000000000);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(quadrillionplus.Take(2).Sum());
        Console.WriteLine(quadrillionplus.Skip(2).First());
    
        IEnumerable<long> fibMod612 = Fibo.OrderBy(i => i % 612);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(fibMod612.First());//smallest divisible by 612
    }
    
    public class Fibonacci : IEnumerable<long>
    {
        private int max = 90;
    
        //Enumerator called typically from foreach
        public IEnumerator GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
        
        //Enumerable called typically from linq
        IEnumerator<long> IEnumerable<long>.GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
    }

Resultado

    Enumerable built
    Enumerating the Enumerable
    4052739537881
    Enumerating the Enumerable
    4052739537881
    Enumerable built
    Enumerating the Enumerable
    14930352

O ponto forte no segundo conjunto (o fibMod612) é que, embora tenhamos feito a chamada para ordenar todo o nosso conjunto de números de Fibonacci, já que apenas um valor foi obtido usando `.First()` a complexidade de tempo foi O(n) como apenas 1 valor necessário para ser comparado durante a execução do algoritmo de ordenação. Isso ocorre porque nosso enumerador pediu apenas 1 valor e, portanto, todo o enumerável não precisou ser materializado. Se tivéssemos usado `.Take(5)` em vez de `.First()` o enumerador teria pedido 5 valores, e no máximo 5 valores precisariam ser materializados. Comparado com a necessidade de pedir um conjunto inteiro *e depois* pegar os primeiros 5 valores, o princípio de economiza muito tempo e espaço de execução.



## Onde
Retorna um subconjunto de itens cujo predicado especificado é verdadeiro para eles.

    List<string> trees = new List<string>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

## Sintaxe do método

    // Select all trees with name of length 3
    var shortTrees = trees.Where(tree => tree.Length == 3); // Oak, Elm

## Sintaxe da consulta

    var shortTrees = from tree in trees
                     where tree.Length == 3
                     select tree; // Oak, Elm


## Usando Range com vários métodos Linq
Você pode usar a classe Enumerable junto com as consultas do Linq para converter loops em Linq one liners.

**Selecione Exemplo**

Contra fazer isso:


    var asciiCharacters = new List<char>();
    for (var x = 0; x < 256; x++)
    {
        asciiCharacters.Add((char)x);
    }

Você consegue fazer isso:

    var asciiCharacters = Enumerable.Range(0, 256).Select(a => (char) a);


**Onde Exemplo**

Neste exemplo, 100 números serão gerados e os pares serão extraídos

    var evenNumbers = Enumerable.Range(1, 100).Where(a => a % 2 == 0);



## Usando SelectMany em vez de loops aninhados
Dadas 2 listas

    var list1 = new List<string> { "a", "b", "c" };
    var list2 = new List<string> { "1", "2", "3", "4" };

se você quiser gerar todas as permutações, você pode usar loops aninhados como

    var result = new List<string>();
    foreach (var s1 in list1)
        foreach (var s2 in list2)
            result.Add($"{s1}{s2}");

Usando SelectMany você pode fazer a mesma operação que

    var result = list1.SelectMany(x => list2.Select(y => $"{x}{y}", x, y)).ToList();


## Contém
MSDN:
> Determina se uma sequência contém um elemento especificado usando um
> especificado `IEqualityComparer<T>`


    List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
    var result1 = numbers.Contains(4); // true
    var result2 = numbers.Contains(8); // false

    List<int> secondNumberCollection = new List<int> { 4, 5, 6, 7 };
    // Note that can use the Intersect method in this case
    var result3 = secondNumberCollection.Where(item => numbers.Contains(item)); // will be true only for 4,5


Usando um objeto definido pelo usuário:

    public class Person
    {
       public string Name { get; set; }
    }

    List<Person> objects = new List<Person>
    {
        new Person { Name = "Nikki"},
        new Person { Name = "Gilad"},
        new Person { Name = "Phil"},
        new Person { Name = "John"}
    };

    //Using the Person's Equals method - override Equals() and GetHashCode() - otherwise it
    //will compare by reference and result will be false
    var result4 = objects.Contains(new Person { Name = "Phil" }); // true

Usando a sobrecarga `Enumerable.Contains(value, comparer)`:

    public class Compare : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name;
        }
        public int GetHashCode(Person codeh)
        {
            return codeh.Name.GetHashCode();
        }
    }

    var result5 = objects.Contains(new Person { Name = "Phil" }, new Compare()); // true

**Um uso inteligente de ````Contains```` seria substituir várias cláusulas ````if```` para uma chamada ````Contains````.**

Então, ao invés de fazer isso:

    if(status == 1 || status == 3 || status == 4)
    {
        //Do some business operation
    }
    else
    {
        //Do something else
    }
 
Fazem isto:
    
    if(new int[] {1, 3, 4 }.Contains(status)
    {
        //Do some business operaion
    }
    else 
    {
        //Do something else
    }

## Agrupar por um ou vários campos
Vamos supor que temos algum modelo de filme:

    public class Film {
        public string Title { get; set; }
        public string Category { get; set; }
        public int Year { get; set; }
    }

Agrupar por propriedade de categoria:

    foreach (var grp in films.GroupBy(f => f.Category)) {
        var groupCategory = grp.Key;
        var numberOfFilmsInCategory = grp.Count();
    }

Agrupar por Categoria e Ano:

    foreach (var grp in films.GroupBy(f => new { Category = f.Category, Year = f.Year })) {
        var groupCategory = grp.Key.Category;
        var groupYear = grp.Key.Year;
        var numberOfFilmsInCategory = grp.Count();
    }



## Ordenação da consulta - OrderBy() ThenBy() OrderByDescending() ThenByDescending()
    string[] names= { "mark", "steve", "adam" };

**Ascendente:**

*Sintaxe da consulta*

    var sortedNames =
        from name in names
        orderby name
        select name;

*Sintaxe do Método*

    var sortedNames = names.OrderBy(name => name);

sortedNames contém os nomes na seguinte ordem:
"adam", "marca", "steve"

**Descendente:**


*Sintaxe da consulta*

    var sortedNames =
        from name in names
        orderby name descending
        select name;

*Sintaxe do Método*

    var sortedNames = names.OrderByDescending(name => name);

sortedNames contém os nomes na seguinte ordem:
"steve", "marca", "adam"

**Ordenar por vários campos**

    Person[] people =
    {
        new Person { FirstName = "Steve", LastName = "Collins", Age = 30},
        new Person { FirstName = "Phil" , LastName = "Collins", Age = 28},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 29},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 15}
    };

*Sintaxe da consulta*

    var sortedPeople = from person in people
                       orderby person.LastName, person.FirstName, person.Age descending
                       select person;

*Sintaxe do Método*

     sortedPeople = people.OrderBy(person => person.LastName)
                          .ThenBy(person => person.FirstName)
                          .ThenByDescending(person => person.Age);

*Resultado*

    1. Adam Ackerman 29
    2. Adam Ackerman 15
    3. Phil Collins  28
    4. Steve Collins 30



## Para Dicionário
O método LINQ `ToDictionary()` pode ser usado para gerar uma coleção `Dictionary<TKey, TElement>` com base em uma determinada fonte `IEnumerable<T>`.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, User> usersById = users.ToDictionary(x => x.Id);

Neste exemplo, o único argumento passado para `ToDictionary` é do tipo `Func<TSource, TKey>`, que retorna a chave para cada elemento.

Esta é uma maneira concisa de realizar a seguinte operação:

    Dictionary<int, User> usersById = new Dictionary<int User>();
    foreach (User u in users) 
    {
      usersById.Add(u.Id, u);
    }

Você também pode passar um segundo parâmetro para o método `ToDictionary`, que é do tipo `Func<TSource, TElement>` e retorna o `Value` a ser adicionado para cada entrada.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, string> userNamesById = users.ToDictionary(x => x.Id, x => x.Name);

Também é possível especificar o `IComparer` que é usado para comparar valores de chave. Isso pode ser útil quando a chave é uma string e você deseja que ela não faça distinção entre maiúsculas e minúsculas.

    IEnumerable<User> users = GetUsers();
    Dictionary<string, User> usersByCaseInsenstiveName = users.ToDictionary(x => x.Name, StringComparer.InvariantCultureIgnoreCase);

    var user1 = usersByCaseInsenstiveName["john"];
    var user2 = usersByCaseInsenstiveName["JOHN"];
    user1 == user2; // Returns true

Nota: o método `ToDictionary` requer que todas as chaves sejam únicas, não deve haver chaves duplicadas. Se houver, uma exceção será lançada: `ArgumentException: Um item com a mesma chave já foi adicionado.` Se você tiver um cenário em que sabe que terá vários elementos com a mesma chave, é melhor usar [`ToLookup`](https://www.wikiod.com/pt/docs/c%23/68/linq-queries/14871/tolookup) em vez disso.




## SkipWhile
`SkipWhile()` é usado para excluir elementos até a primeira não correspondência (isso pode ser contra intuitivo para a maioria)

    int[] list = { 42, 42, 6, 6, 6, 42 };
    var result = list.SkipWhile(i => i == 42); 
    // Result: 6, 6, 6, 42

## DefaultIfEmpty
DefaultIfEmpty é usado para retornar um Elemento Padrão se a Sequência não contiver elementos. Este Elemento pode ser o Padrão do Tipo ou uma instância definida pelo usuário desse Tipo. Exemplo:

    var chars = new List<string>() { "a", "b", "c", "d" };

    chars.DefaultIfEmpty("N/A").FirstOrDefault(); // returns "a";
    
    chars.Where(str => str.Length > 1)
         .DefaultIfEmpty("N/A").FirstOrDefault(); // return "N/A"

    chars.Where(str => str.Length > 1)
            .DefaultIfEmpty().First(); // returns null;

**Uso em Junções Esquerdas**:
--------------------

Com `DefaultIfEmpty` o Linq Join tradicional pode retornar um objeto padrão se nenhuma correspondência for encontrada. Atuando assim como um Left Join do SQL. Exemplo:

    var leftSequence = new List<int>() { 99, 100, 5, 20, 102, 105 };
    var rightSequence = new List<char>() { 'a', 'b', 'c', 'i', 'd' };
    
    var numbersAsChars = from l in leftSequence
                         join r in rightSequence
                         on l equals (int)r into leftJoin
                         from result in leftJoin.DefaultIfEmpty('?')
                         select new
                         {
                             Number = l,
                             Character = result
                         };
    
    foreach(var item in numbersAsChars)
    {
        Console.WriteLine("Num = {0} ** Char = {1}", item.Number, item.Character);
    }

    ouput: 

    Num = 99         Char = c
    Num = 100        Char = d
    Num = 5          Char = ?
    Num = 20         Char = ?
    Num = 102        Char = ?
    Num = 105        Char = i

No caso em que um `DefaultIfEmpty` é usado (sem especificar um valor padrão) e isso resultará em itens não correspondentes na sequência correta, deve-se ter certeza de que o objeto não é `null` antes de acessar suas propriedades. Caso contrário, resultará em um `NullReferenceException`. Exemplo:

    var leftSequence = new List<int> { 1, 2, 5 };
    var rightSequence = new List<dynamic>()
        {
            new { Value = 1 },
            new { Value = 2 },
            new { Value = 3 },
            new { Value = 4 },
        };

    var numbersAsChars = (from l in leftSequence
                            join r in rightSequence
                            on l equals r.Value into leftJoin
                            from result in leftJoin.DefaultIfEmpty()
                            select new
                            {
                                Left = l,
                                // 5 will not have a matching object in the right so result 
                                // will be equal to null. 
                                // To avoid an error use:
                                //    -  C# 6.0 or above - ?. 
                                //    -  Under           - result == null ? 0 : result.Value
                                Right = result?.Value
                            }).ToList();



## Sequência Igual
`SequenceEqual` é usado para comparar duas sequências `IEnumerable<T>` entre si.


    int[] a = new int[] {1, 2, 3};
    int[] b = new int[] {1, 2, 3};
    int[] c = new int[] {1, 3, 2};

    bool returnsTrue = a.SequenceEqual(b);
    bool returnsFalse = a.SequenceEqual(c);

## ElementAt e ElementAtOrDefault
`ElementAt` retornará o item no índice `n`. Se `n` não estiver dentro do intervalo do enumerável, lança um `ArgumentOutOfRangeException`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAt(2);  // 3
    numbers.ElementAt(10); // throws ArgumentOutOfRangeException

`ElementAtOrDefault` retornará o item no índice `n`. Se `n` não estiver dentro do intervalo do enumerável, retorna um `default(T)`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAtOrDefault(2);  // 3
    numbers.ElementAtOrDefault(10); // 0 = default(int)

Ambos `ElementAt` e `ElementAtOrDefault` são otimizados para quando a fonte é um `IList<T>` e a indexação normal será usada nesses casos.

Observe que para `ElementAt`, se o índice fornecido for maior que o tamanho do `IList<T>`, a lista deve (mas tecnicamente não é garantido) lançar um `ArgumentOutOfRangeException`.


## Juntando várias sequências
Considere as entidades `Customer`, `Purchase` e ​​`PurchaseItem` da seguinte forma:

    public class Customer
    {
       public string Id { get; set } // A unique Id that identifies customer    
       public string Name  {get; set; }
    }

    public class Purchase
    {
       public string Id { get; set }
       public string CustomerId {get; set; }
       public string Description { get; set; }
    }

    public class PurchaseItem
    {
       public string Id { get; set }
       public string PurchaseId {get; set; }
       public string Detail { get; set; }
    }

Considere os seguintes dados de amostra para as entidades acima:

    var customers = new List<Customer>()             
     {
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer1"            
        },
                
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer2"            
        }
     };        
        
     var purchases = new List<Purchase>() 
     {
         new Purchase() {                
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase2"            
         },
         
         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase2"            
         }
      };
        
     var purchaseItems = new List<PurchaseItem>() 
     {
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[0].Id,
             Detail = "Purchase1-PurchaseItem1"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem1"            
         },
         
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem2"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[3].Id,
             Detail = "Purchase3-PurchaseItem1"
         }
     };

Agora, considere abaixo a consulta linq:

    var result = from c in customers
                join p in purchases on c.Id equals p.CustomerId           // first join
                join pi in purchaseItems on p.Id equals pi.PurchaseId     // second join
                select new
                {
                   c.Name, p.Description, pi.Detail
                };


Para gerar o resultado da consulta acima:

    foreach(var resultItem in result)
    {
        Console.WriteLine($"{resultItem.Name}, {resultItem.Description}, {resultItem.Detail}");
    }
        
A saída da consulta seria:

> Cliente1, Cliente1-Compra1, Compra1-CompraItem1
> 
> Cliente1, Cliente1-Compra2, Compra2-CompraItem1
> 
> Cliente1, Cliente1-Compra2, Compra2-CompraItem2
> 
> Cliente2, Cliente2-Compra2, Compra3-CompraItem1

[Demonstração ao vivo no .NET Fiddle][1]


[1]: https://dotnetfiddle.net/Db8uqp

## Juntando-se a várias chaves
  

      PropertyInfo[] stringProps = typeof (string).GetProperties();//string properties
      PropertyInfo[] builderProps = typeof(StringBuilder).GetProperties();//stringbuilder properties
        
        var query =
            from s in stringProps
            join b in builderProps
                on new { s.Name, s.PropertyType } equals new { b.Name, b.PropertyType }
            select new
            {
                s.Name,
                s.PropertyType,
                StringToken = s.MetadataToken,
                StringBuilderToken = b.MetadataToken
            };

Observe que os tipos anônimos no `join` acima devem conter as mesmas propriedades, pois os objetos são considerados iguais somente se todas as suas propriedades forem iguais. Caso contrário, a consulta não será compilada.

## Soma
O método de extensão `Enumerable.Sum` calcula a soma dos valores numéricos.

Caso os elementos da coleção sejam eles próprios números, você pode calcular a soma diretamente.

    int[] numbers = new int[] { 1, 4, 6 };
    Console.WriteLine( numbers.Sum() ); //outputs 11

Caso o tipo dos elementos seja um tipo complexo, você pode usar uma expressão lambda para especificar o valor que deve ser calculado:

    var totalMonthlySalary = employees.Sum( employee => employee.MonthlySalary );

O método de extensão de soma pode calcular com os seguintes tipos:

- Int32
- Int64
- Solteiro
- Dobro
- Decimal

Caso sua coleção contenha tipos anuláveis, você pode usar o operador null-coalescing para definir um valor padrão para elementos nulos:

    int?[] numbers = new int?[] { 1, null, 6 };
    Console.WriteLine( numbers.Sum( number => number ?? 0 ) ); //outputs 7

## Procurar
> ToLookup retorna uma estrutura de dados que permite indexação. É um
> método de extensão. Ele produz uma instância ILookup que pode ser indexada
> ou enumerado usando um loop foreach. As entradas são combinadas em
> agrupamentos em cada tecla. - dotnetperls

    string[] array = { "one", "two", "three" };
    //create lookup using string length as key
    var lookup = array.ToLookup(item => item.Length);
    
    //join the values whose lengths are 3
    Console.WriteLine(string.Join(",",lookup[3]));
    //output: one,two

Outro exemplo:

    int[] array = { 1,2,3,4,5,6,7,8 };
    //generate lookup for odd even numbers (keys will be 0 and 1)
    var lookup = array.ToLookup(item => item % 2);
    
    //print even numbers after joining
    Console.WriteLine(string.Join(",",lookup[0]));
    //output: 2,4,6,8

    //print odd numbers after joining
    Console.WriteLine(string.Join(",",lookup[1]));
    //output: 1,3,5,7

## Qualquer e primeiro(OrDefault) - prática recomendada
Não vou explicar o que `Any` e `FirstOrDefault` fazem porque já existem dois bons exemplos sobre eles. Consulte https://www.wikiod.com/pt/docs/c%23/68/linq-queries/5098/any#t=201707200324548979636 e https://www.wikiod.com/pt/docs/c%23/68/linq-queries/329 /first-firstordefault-last-lastordefault-single-and-singleordefault#t=201707200328069088515 para obter mais informações.

Um padrão que vejo com frequência no código que **deve ser evitado** é

    if (myEnumerable.Any(t=>t.Foo == "Bob"))
    {
        var myFoo = myEnumerable.First(t=>t.Foo == "Bob");
        //Do stuff
    }

Poderia ser escrito de forma mais eficiente assim

    var myFoo = myEnumerable.FirstOrDefault(t=>t.Foo == "Bob");
    if (myFoo != null)
    {
        //Do stuff
    }

Usando o segundo exemplo, a coleção é pesquisada apenas uma vez e fornece o mesmo resultado que o primeiro. A mesma ideia pode ser aplicada a `Single`.

## Agrupar por soma e contagem
Vamos fazer uma aula de exemplo:

    public class Transaction
    {
        public string Category { get; set; }
        public DateTime Date { get; set; }
        public decimal Amount { get; set; }
    }

Agora, vamos considerar uma lista de transações:

    var transactions = new List<Transaction>
    {
       new Transaction { Category = "Saving Account", Amount = 56, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Saving Account", Amount = 10, Date = DateTime.Today.AddDays(-10) },
       new Transaction { Category = "Credit Card", Amount = 15, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Credit Card", Amount = 56, Date = DateTime.Today },
       new Transaction { Category = "Current Account", Amount = 100, Date = DateTime.Today.AddDays(5) },
    };

Se você deseja calcular a soma de valor e contagem por categoria, pode usar GroupBy da seguinte maneira:

    var summaryApproach1 = transactions.GroupBy(t => t.Category)
                               .Select(t => new
                               {
                                   Category = t.Key,
                                   Count = t.Count(),
                                   Amount = t.Sum(ta => ta.Amount),
                               }).ToList();
    
    Console.WriteLine("-- Summary: Approach 1 --");
    summaryApproach1.ForEach(
                row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

Alternativamente, você pode fazer isso em uma etapa:

    var summaryApproach2 = transactions.GroupBy(t => t.Category, (key, t) =>
    {
            var transactionArray = t as Transaction[] ?? t.ToArray();
            return new
            {
                Category = key,
                Count = transactionArray.Length,
                Amount = transactionArray.Sum(ta => ta.Amount),
            };
    }).ToList();

    Console.WriteLine("-- Summary: Approach 2 --");
    summaryApproach2.ForEach(
    row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

A saída para ambas as consultas acima seria a mesma:

> Categoria: Conta Poupança, Valor: 66, Contagem: 2
>
> Categoria: Cartão de Crédito, Valor: 71, Contagem: 2
>
> Categoria: Conta Corrente, Valor: 100, Contagem: 1

[Demonstração ao vivo no .NET Fiddle][1]


[1]: https://dotnetfiddle.net/1PfLGq#

## Ordenar por
> Ordena uma coleção por um valor especificado.

Quando o valor é um **inteiro**, **duplo** ou **float**, ele começa com o *valor mínimo*, o que significa que você obtém primeiro os valores negativos, depois zero e depois os valores positivos (consulte Exemplo 1).

Quando você ordena por um **char**, o método compara os *valores ascii* dos chars para ordenar a coleção (veja o Exemplo 2).

Quando você classifica **strings**, o método OrderBy as compara examinando suas [CultureInfo][1], mas normalmente começando com a *primeira letra* do alfabeto (a,b,c...).

Esse tipo de ordem é chamado de ascendente, se você quiser o contrário, precisa de descendente (consulte OrderByDescending).

**Exemplo 1:**

    int[] numbers = {2, 1, 0, -1, -2};
    IEnumerable<int> ascending = numbers.OrderBy(x => x);
    // returns {-2, -1, 0, 1, 2}

**Exemplo 2:**

     char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
     IEnumerable<char> ascending = letters.OrderBy(x => x);
     // returns { ' ', '!', '+', '1', '9', '?', 'A', 'B', 'Y', 'Z', '[', 'a', 'b', 'y', 'z', '{' }

**Exemplo:**

    class Person
    {
       public string Name { get; set; }
       public int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var youngestPerson = people.OrderBy(x => x.Age).First();
    var name = youngestPerson.Name; // Bob


[1]: https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Select - Transformando elementos
Select permite que você aplique uma transformação a cada elemento em qualquer estrutura de dados que implemente IEnumerable.

Obtendo o primeiro caractere de cada string na lista a seguir:

    List<String> trees = new List<String>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };
    
Usando a sintaxe regular (lambda)

    //The below select stament transforms each element in tree into its first character.
    IEnumerable<String> initials = trees.Select(tree => tree.Substring(0, 1));
    foreach (String initial in initials) {
        System.Console.WriteLine(initial);
    }

**Resultado:**
>O
>B
>B
>E
>H
>M

[Demonstração ao vivo no .NET Fiddle][1]

Usando a sintaxe de consulta LINQ

    initials = from tree in trees
               select tree.Substring(0, 1);


[1]: https://dotnetfiddle.net/yYLT0K

## União
Mescla duas coleções para criar uma coleção distinta usando o comparador de igualdade padrão

    int[] numbers1 = { 1, 2, 3 };
    int[] numbers2 = { 2, 3, 4, 5 };
    
    var allElement = numbers1.Union(numbers2);   // AllElement now contains 1,2,3,4,5


[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/oet2Uq)

## Contagem e Contagem Longa
`Count` retorna o número de elementos em um `IEnumerable<T>`. `Count` também expõe um parâmetro de predicado opcional que permite filtrar os elementos que você deseja contar.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };
    
    int n = array.Count(); // returns the number of elements in the array
    int x = array.Count(i => i > 2); // returns the number of elements in the array greater than 2

`LongCount` funciona da mesma forma que `Count`, mas tem um tipo de retorno `long` e é usado para contar sequências `IEnumerable<T>` que são maiores que `int.MaxValue`

    int[] array = GetLargeArray();
    
    long n = array.LongCount(); // returns the number of elements in the array
    long x = array.LongCount(i => i > 100); // returns the number of elements in the array greater than 100


## Criando uma consulta de forma incremental
Como o LINQ usa **execução adiada**, podemos ter um objeto de consulta que na verdade não contém os valores, mas retornará os valores quando avaliado. Podemos, assim, construir dinamicamente a consulta com base em nosso fluxo de controle e avaliá-la quando terminarmos:

    IEnumerable<VehicleModel> BuildQuery(int vehicleType, SearchModel search, int start = 1, int count = -1) {
        IEnumerable<VehicleModel> query = _entities.Vehicles
            .Where(x => x.Active && x.Type == vehicleType)
            .Select(x => new VehicleModel {
                Id = v.Id,
                Year = v.Year,
                Class = v.Class,
                Make = v.Make,
                Model = v.Model,
                Cylinders = v.Cylinders ?? 0
            });

Podemos aplicar filtros condicionalmente:

        if (!search.Years.Contains("all", StringComparer.OrdinalIgnoreCase))
            query = query.Where(v => search.Years.Contains(v.Year));

        if (!search.Makes.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Makes.Contains(v.Make));
        }

        if (!search.Models.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Models.Contains(v.Model));
        }

        if (!search.Cylinders.Equals("all", StringComparer.OrdinalIgnoreCase)) {
            decimal minCylinders = 0;
            decimal maxCylinders = 0;
            switch (search.Cylinders) {
                case "2-4":
                    maxCylinders = 4;
                    break;
                case "5-6":
                    minCylinders = 5;
                    maxCylinders = 6;
                    break;
                case "8":
                    minCylinders = 8;
                    maxCylinders = 8;
                    break;
                case "10+":
                    minCylinders = 10;
                    break;
            }
            if (minCylinders > 0) {
                query = query.Where(v => v.Cylinders >= minCylinders);
            }
            if (maxCylinders > 0) {
                query = query.Where(v => v.Cylinders <= maxCylinders);
            }
        }

Podemos adicionar uma ordem de classificação à consulta com base em uma condição:

        switch (search.SortingColumn.ToLower()) {
            case "make_model":
                query = query.OrderBy(v => v.Make).ThenBy(v => v.Model);
                break;
            case "year":
                query = query.OrderBy(v => v.Year);
                break;
            case "engine_size":
                query = query.OrderBy(v => v.EngineSize).ThenBy(v => v.Cylinders);
                break;
            default:
                query = query.OrderBy(v => v.Year); //The default sorting.
        }
        
Nossa consulta pode ser definida para começar a partir de um determinado ponto:
        
        query = query.Skip(start - 1);
        
e definido para retornar um número específico de registros:

        if (count > -1) {
            query = query.Take(count);
        }
        return query;
    }
    
---

Uma vez que temos o objeto de consulta, podemos avaliar os resultados com um loop `foreach`, ou um dos métodos LINQ que retorna um conjunto de valores, como `ToList` ou `ToArray`:

    SearchModel sm;
    
    // populate the search model here
    // ...
    
    List<VehicleModel> list = BuildQuery(5, sm).ToList();

## GroupJoin com variável de intervalo externo


    Customer[] customers = Customers.ToArray();
    Purchase[] purchases = Purchases.ToArray();
    
    var groupJoinQuery =
        from c in customers
        join p in purchases on c.ID equals p.CustomerID
        into custPurchases
        select new
        {
            CustName = c.Name,
            custPurchases
        };



## Quantificadores Linq
As operações do quantificador retornam um valor booleano se alguns ou todos os elementos em uma sequência satisfazem uma condição. Neste artigo, veremos alguns cenários comuns de LINQ to Objects nos quais podemos usar esses operadores.
Existem 3 operações de Quantifiers que podem ser usadas no LINQ:

`All` – usado para determinar se todos os elementos em uma sequência satisfazem uma condição.
Por exemplo:

    int[] array = { 10, 20, 30 }; 
       
    // Are all elements >= 10? YES
    array.All(element => element >= 10); 
       
    // Are all elements >= 20? NO
    array.All(element => element >= 20);
        
    // Are all elements < 40? YES
    array.All(element => element < 40);
    
`Any` - usado para determinar se algum elemento em uma sequência satisfaz uma condição.
Por exemplo:

    int[] query=new int[] { 2, 3, 4 }
    query.Any (n => n == 3);

`Contém` - usado para determinar se uma sequência contém um elemento especificado.
Por exemplo:

    //for int array
    int[] query =new int[] { 1,2,3 };
    query.Contains(1);
    
    //for string array
    string[] query={"Tom","grey"};
    query.Contains("Tom");
    
    //for a string
    var stringValue="hello";
    stringValue.Contains("h");



## Tome Enquanto
`TakeWhile` retorna elementos de uma sequência desde que a condição seja verdadeira

    int[] list = { 1, 10, 40, 50, 44, 70, 4 };
    var result = list.TakeWhile(item => item < 50).ToList();
    // result = { 1, 10, 40 }

## Crie seus próprios operadores Linq para IEnumerable<T>
Uma das grandes vantagens do Linq é que ele é muito fácil de estender. Você só precisa criar um [método de extensão][1] cujo argumento seja `IEnumerable<T>`.

    public namespace MyNamespace
    {
        public static class LinqExtensions
        {
            public static IEnumerable<List<T>> Batch<T>(this IEnumerable<T> source, int batchSize)
            {
                var batch = new List<T>();
                foreach (T item in source)
                {
                    batch.Add(item);
                    if (batch.Count == batchSize)
                    {
                        yield return batch;
                        batch = new List<T>();
                    }
                }
                if (batch.Count > 0)
                    yield return batch;
            }
        }
    }

Este exemplo divide os itens em um `IEnumerable<T>` em listas de tamanho fixo, a última lista contendo o restante dos itens. Observe como o objeto ao qual o método de extensão é aplicado é passado (argumento `source`) como o argumento inicial usando a palavra-chave `this`. Em seguida, a palavra-chave `yield` é usada para gerar o próximo item na saída `IEnumerable<T>` antes de continuar com a execução a partir desse ponto (consulte [palavra-chave Yield][2]).

Este exemplo seria usado em seu código assim:

    //using MyNamespace;
    var items = new List<int> { 2, 3, 4, 5, 6 };
    foreach (List<int> sublist in items.Batch(3))
    {
        // do something
    }

No primeiro loop, a sublista seria `{2, 3, 4}` e no segundo `{5, 6}`.

Os métodos personalizados do LinQ também podem ser combinados com os métodos padrão do LinQ. por exemplo.:

    //using MyNamespace;
    var result = Enumerable.Range(0, 13)         // generate a list
                           .Where(x => x%2 == 0) // filter the list or do something other
                           .Batch(3)             // call our extension method
                           .ToList()             // call other standard methods

Esta consulta retornará números pares agrupados em lotes com tamanho 3: `{0, 2, 4}, {6, 8, 10}, {12}`

Lembre-se que você precisa de uma linha `using MyNamespace;` para poder acessar o método de extensão.

[1]: https://www.wikiod.com/pt/docs/c%23/20/extension-methods/33/using-an-extension-method#t=201607280952261411896
[2]: https://www.wikiod.com/pt/docs/c%23/61/yield-keyword#t=201607281004094832778

## Marcha ré

- Inverte a ordem dos elementos em uma sequência.
- Se não houver itens, lança um `ArgumentNullException: source is null.`

***Exemplo:***

    // Create an array.
    int[] array = { 1, 2, 3, 4 };                         //Output:
    // Call reverse extension method on the array.        //4
    var reverse = array.Reverse();                        //3
    // Write contents of array to screen.                 //2
    foreach (int value in reverse)                        //1
        Console.WriteLine(value);
  
[Exemplo de código ao vivo](https://dotnetfiddle.net/ckrWUo)

Lembre-se que `Reverse()` pode funcionar de forma diferente dependendo da ordem da cadeia de suas instruções LINQ.

            //Create List of chars
            List<int> integerlist = new List<int>() { 1, 2, 3, 4, 5, 6 };

            //Reversing the list then taking the two first elements
            IEnumerable<int> reverseFirst = integerlist.Reverse<int>().Take(2);
            
            //Taking 2 elements and then reversing only thos two
            IEnumerable<int> reverseLast = integerlist.Take(2).Reverse();
            
            //reverseFirst output: 6, 5
            //reverseLast output:  2, 1

[Exemplo de código ao vivo](https://dotnetfiddle.net/ckrWUo)

*Reverse()* funciona armazenando tudo em buffer e depois percorrendo-o de trás para frente, o que não é muito eficiente, mas OrderBy também não é dessa perspectiva.

No LINQ-to-Objects, existem operações de buffer (Reverse, OrderBy, GroupBy, etc) e operações sem buffer (Where, Take, Skip, etc).


***Exemplo: Extensão reversa sem buffer***
    
    public static IEnumerable<T> Reverse<T>(this IList<T> list) {
        for (int i = list.Count - 1; i >= 0; i--) 
            yield return list[i];
    }

[Exemplo de código ao vivo](https://dotnetfiddle.net/ckrWUo)

Este método pode encontrar problemas se você alterar a lista durante a iteração.





## Ordenar Por Descendente
> Ordena uma coleção por um valor especificado.

Quando o valor é um **inteiro**, **duplo** ou **float**, ele começa com o *valor máximo*, o que significa que você obtém primeiro os valores positivos, depois zero e depois os valores negativos (consulte Exemplo 1).

Quando você ordena por um **char**, o método compara os *valores ascii* dos chars para ordenar a coleção (veja o Exemplo 2).

Quando você classifica **strings**, o método OrderBy as compara examinando suas [CultureInfo][1], mas normalmente começando com a *última letra* do alfabeto (z,y,x,...).

Esse tipo de ordem é chamado de descendente, se você quiser o contrário, precisará de ascendente (consulte OrderBy).

**Exemplo 1:**

    int[] numbers = {-2, -1, 0, 1, 2};
    IEnumerable<int> descending = numbers.OrderByDescending(x => x);
    // returns {2, 1, 0, -1, -2}

**Exemplo 2:**

    char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
    IEnumerable<char> descending = letters.OrderByDescending(x => x);
    // returns { '{', 'z', 'y', 'b', 'a', '[', 'Z', 'Y', 'B', 'A', '?', '9', '1', '+', '!', ' ' }

**Exemplo 3:**
    
    class Person
    {
       public  string Name { get; set; }
       public  int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var oldestPerson = people.OrderByDescending(x => x.Age).First();
    var name = oldestPerson.Name; // Carol


[1]: https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Concatenar
Mescla duas coleções (sem remover duplicatas)

    List<int> foo = new List<int> { 1, 2, 3 };
    List<int> bar = new List<int> { 3, 4, 5 };

    // Through Enumerable static class
    var result = Enumerable.Concat(foo, bar).ToList(); // 1,2,3,3,4,5

    // Through extension method
    var result = foo.Concat(bar).ToList(); // 1,2,3,3,4,5

## Select com seletor Func<TSource, int, TResult> - Use para obter a classificação dos elementos
Uma das sobrecargas dos métodos de extensão `Select` também passa o `index` do item atual na coleção que está sendo `selecionada`. Esses são alguns usos dele.

**Obter o "número da linha" dos itens**

    var rowNumbers = collection.OrderBy(item => item.Property1)
                               .ThenBy(item => item.Property2)
                               .ThenByDescending(item => item.Property3)
                               .Select((item, index) => new { Item = item, RowNumber = index })
                               .ToList();

**Obtenha a classificação de um item *dentro* de seu grupo**

    var rankInGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .SelectMany(group => group.OrderBy(item => item.Property2)
                                                       .ThenByDescending(item => item.Property3)
                                                       .Select((item, index) => new 
                                                       { 
                                                           Item = item, 
                                                           RankInGroup = index 
                                                       })).ToList();

**Obtenha o ranking dos grupos (também conhecido no Oracle como denso_rank)**

    var rankOfBelongingGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .Select((group, index) => new
                                {
                                    Items = group,
                                    Rank = index
                                })
                                .SelectMany(v => v.Items, (s, i) => new
                                {
                                    Item = i,
                                    DenseRank = s.Rank
                                }).ToList();

Para testar isso, você pode usar:

    public class SomeObject
    {
        public int Property1 { get; set; }
        public int Property2 { get; set; }
        public int Property3 { get; set; }

        public override string ToString()
        {
            return string.Join(", ", Property1, Property2, Property3);
        }
    }

E dados:

    List<SomeObject> collection = new List<SomeObject>
    {
        new SomeObject { Property1 = 1, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 2},
        new SomeObject { Property1 = 2, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 3, Property3 = 1}
    };


