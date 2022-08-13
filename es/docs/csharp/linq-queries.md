---
title: "Consultas LINQ"
slug: "consultas-linq"
draft: false
images: []
weight: 6793
type: docs
toc: true
---

LINQ es un acrónimo que significa **L**idioma **IN**tegrated **Q**uery. Es un concepto que integra un lenguaje de consulta al ofrecer un modelo consistente para trabajar con datos en varios tipos de fuentes y formatos de datos; utiliza los mismos patrones de codificación básicos para consultar y transformar datos en documentos XML, bases de datos SQL, conjuntos de datos ADO.NET, colecciones .NET y cualquier otro formato para el que esté disponible un proveedor LINQ.

## Sintaxis
- Sintaxis de consulta:
    - from \<range variable\> in \<collection\>
    - [from \<range variable\> in \<collection\>, ...]
    - \<filter, joining, grouping, aggregate operators, ...\> \<lambda expression\>
    - \<select or groupBy operator\> \<formulate the result\>

- Sintaxis del método:

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

Para usar consultas LINQ, debe importar `System.Linq`.

La sintaxis del método es más poderosa y flexible, pero la sintaxis de consulta puede ser más simple y familiar. El compilador traduce todas las consultas escritas en sintaxis de consulta a la sintaxis funcional, por lo que el rendimiento es el mismo.

Los objetos de consulta no se evalúan hasta que se utilizan, por lo que se pueden cambiar o agregar sin penalizar el rendimiento.

## Métodos de encadenamiento
[Muchas funciones de LINQ][1] operan en un `IEnumerable<TSource>` y también devuelven un `IEnumerable<TResult>`. Los parámetros de tipo `TSource` y `TResult` pueden o no referirse al mismo tipo, según el método en cuestión y las funciones que se le pasen.

Algunos ejemplos de esto son

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

Si bien el encadenamiento de algunos métodos puede requerir que se trabaje con un conjunto completo antes de continuar, LINQ aprovecha la [ejecución diferida] (https://www.wikiod.com/es/docs/c%23/68/linq-queries/8001/deferred- ejecución) usando [yield return <sup>**MSDN**</sup>](https://blogs.msdn.microsoft.com/oldnewthing/20080812-00/?p=21273/) que crea un Enumerable y un enumerador detrás de escena. El proceso de encadenamiento en LINQ consiste esencialmente en crear un enumerable (iterador) para el conjunto original, que se difiere, hasta que se materializa [enumerando el enumerable] (https://www.wikiod.com/es/docs/c%23/68/ linq-queries/17356/enumerating-the-enumerable).

Esto permite que estas funciones estén [encadenadas con fluidez <sup>**wiki**</sup>](https://en.wikipedia.org/wiki/Fluent_interface), donde una función puede actuar directamente sobre el resultado de otra. Este estilo de código se puede usar para realizar muchas operaciones basadas en secuencias en una sola declaración.

Por ejemplo, es posible combinar `Select`, `Where` y `OrderBy` para transformar, filtrar y ordenar una secuencia en una sola declaración.

    var someNumbers = { 4, 3, 2, 1 };

    var processed = someNumbers
            .Select(n => n * 2)   // Multiply each number by 2
            .Where(n => n != 6)   // Keep all the results, except for 6
            .OrderBy(n => n);     // Sort in ascending order

**Producción:**
>2
>4
>8

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/3Gta8X)

Cualquier función que extienda y devuelva el tipo genérico `IEnumerable<T>` se puede usar como cláusulas encadenadas en una sola instrucción. Este estilo de programación fluida es poderoso y debe tenerse en cuenta al crear sus propios [métodos de extensión][2].


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx
[2]: https://www.wikiod.com/es/docs/c%23/20/extension-methods#t=201607220826369208865

## First, FirstOrDefault, Last, LastOrDefault, Single y SingleOrDefault
Los seis métodos devuelven un único valor del tipo de secuencia y se pueden llamar con o sin predicado.

Dependiendo de la cantidad de elementos que coincidan con el "predicado" o, si no se proporciona ningún "predicado", la cantidad de elementos en la secuencia de origen, se comportan de la siguiente manera:

# Primero()

* Devuelve el primer elemento de una secuencia, o el primer elemento que coincide con el "predicado" proporcionado.
* Si la secuencia no contiene elementos, se lanza una `InvalidOperationException` con el mensaje: "La secuencia no contiene elementos".
* Si la secuencia no contiene elementos que coincidan con el `predicado` proporcionado, se lanzará una `InvalidOperationException` con el mensaje "La secuencia no contiene ningún elemento coincidente".

**Ejemplo**

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

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/ESYLcU)


# PrimeroOPredeterminado()

* Devuelve el primer elemento de una secuencia, o el primer elemento que coincide con el "predicado" proporcionado.
* Si la secuencia no contiene elementos, o no hay elementos que coincidan con el `predicado` proporcionado, devuelve el valor predeterminado del tipo de secuencia usando [`default(T)`](https://www.wikiod.com/es/docs/c%23/ 26/palabras clave/109/predeterminado#t=201702071640321629621).

**Ejemplo**

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

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/XJ93lr)


# Ultimo()

* Devuelve el último elemento de una secuencia, o el último elemento que coincide con el "predicado" proporcionado.
* Si la secuencia no contiene elementos, se lanza una `InvalidOperationException` con el mensaje "La secuencia no contiene elementos".
* Si la secuencia no contiene elementos que coincidan con el `predicado` proporcionado, se lanzará una `InvalidOperationException` con el mensaje "La secuencia no contiene ningún elemento coincidente".

**Ejemplo**

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


# ÚltimoOPredeterminado()

* Devuelve el último elemento de una secuencia, o el último elemento que coincide con el "predicado" proporcionado.
* Si la secuencia no contiene elementos, o no hay elementos que coincidan con el `predicado` proporcionado, devuelve el valor predeterminado del tipo de secuencia usando `default(T)`.

**Ejemplo**

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


# Único()

* Si la secuencia contiene exactamente un elemento, o exactamente un elemento que coincida con el "predicado" proporcionado, se devuelve ese elemento.
* Si la secuencia no contiene elementos, o no hay elementos que coincidan con el `predicado` proporcionado, se lanza una `InvalidOperationException` con el mensaje "La secuencia no contiene elementos".
* Si la secuencia contiene más de un elemento, o más de un elemento que coincide con el `predicado` proporcionado, se lanza una `InvalidOperationException` con el mensaje "La secuencia contiene más de un elemento".
* __Nota:__ para evaluar si la secuencia contiene exactamente un elemento, se deben enumerar como máximo dos elementos.

**Ejemplo**

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
* Si la secuencia contiene exactamente un elemento, o exactamente un elemento que coincida con el "predicado" proporcionado, se devuelve ese elemento.
* Si la secuencia no contiene elementos, o no hay elementos que coincidan con el `predicado` proporcionado, se devuelve `default(T)`.
* Si la secuencia contiene más de un elemento, o más de un elemento que coincide con el `predicado` proporcionado, se lanza una `InvalidOperationException` con el mensaje "La secuencia contiene más de un elemento".
* Si la secuencia no contiene elementos que coincidan con el `predicado` proporcionado, devuelve el valor predeterminado del tipo de secuencia usando `default(T)`.
* __Nota:__ para evaluar si la secuencia contiene exactamente un elemento, se deben enumerar como máximo dos elementos.

**Ejemplo**

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

# Recomendaciones

- Aunque puede usar `FirstOrDefault`, `LastOrDefault` o `SingleOrDefault` para verificar si una secuencia contiene elementos, `Any` o `Count` son más confiables. Esto se debe a que un valor de retorno de `default(T)` de uno de estos tres métodos no prueba que la secuencia esté vacía, ya que el valor del primer/último/único elemento de la secuencia podría ser igualmente `default(T) )`

- Decida qué métodos se ajustan más al propósito de su código. Por ejemplo, use `Single` solo si debe asegurarse de que haya un solo elemento en la colección que coincida con su predicado &mdash; de lo contrario, utilice `Primero`; como `Single` lanza una excepción si la secuencia tiene más de un elemento coincidente. Por supuesto, esto también se aplica a las contrapartes "*OrDefault".

- Con respecto a la eficiencia: aunque a menudo es apropiado asegurarse de que solo haya un elemento (`Single`) o solo uno o cero elementos (`SingleOrDefault`) devueltos por una consulta, ambos métodos requieren más y, a menudo, el totalidad, de la colección a ser examinada para asegurar que no haya una segunda coincidencia con la consulta. Esto es diferente al comportamiento de, por ejemplo, el método `First`, que puede satisfacerse después de encontrar la primera coincidencia.

## Excepto
El método Except devuelve el conjunto de elementos que están contenidos en la primera colección pero no en la segunda. El [`IEqualityComparer`][1] predeterminado se usa para comparar los elementos dentro de los dos conjuntos. Hay una sobrecarga que acepta un [`IEqualityComparer`][1] como argumento.

**Ejemplo:**

    int[] first = { 1, 2, 3, 4 };
    int[] second = { 0, 2, 3, 5 };
    
    IEnumerable<int> inFirstButNotInSecond = first.Except(second);
    // inFirstButNotInSecond = { 1, 4 }

**Producción:**
>1
>4
 
[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/m3EqTQ)

En este caso, '.Excepto (segundo)' excluye los elementos contenidos en la matriz 'segundo', es decir, 2 y 3 (0 y 5 no están contenidos en la matriz 'primera' y se omiten).

Tenga en cuenta que 'Excepto' implica 'Distinto' (es decir, elimina elementos repetidos). Por ejemplo:
    
    int[] third = { 1, 1, 1, 2, 3, 4 };
    
    IEnumerable<int> inThirdButNotInSecond = third.Except(second);
    // inThirdButNotInSecond = { 1, 4 }

**Producción:**
>1
>4

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/VlXBUp)

En este caso, los elementos 1 y 4 se devuelven una sola vez.

---

Implementar [`IEquatable`][2] o proporcionar la función [`IEqualityComparer`][1] permitirá usar un método diferente para comparar los elementos.
Tenga en cuenta que el método [`GetHashCode`][3] también debe anularse para que devuelva un código hash idéntico para el `objeto` que sea idéntico según la implementación de [`IEquatable`][2].

***Ejemplo con IEquatable:***

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

Producción:

> Jánuca

[Demostración en vivo en .NET Fiddle][4]


[1]: https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.object.gethashcode(v=vs.110).aspx
[4]: https://dotnetfiddle.net/9ilGqy

## SeleccionarMuchos
El método linq de SelectMany 'aplana' un `IEnumerable<IEnumerable<T>>` en un `IEnumerable<T>`. Todos los elementos T dentro de las instancias de `IEnumerable` contenidas en la fuente `IEnumerable` se combinarán en un único `IEnumerable`.

    var words = new [] { "a,b,c", "d,e", "f" };
    var splitAndCombine = words.SelectMany(x => x.Split(','));
    // returns { "a", "b", "c", "d", "e", "f" }

Si utiliza una función de selección que convierte los elementos de entrada en secuencias, el resultado serán los elementos de esas secuencias devueltos uno por uno.

Tenga en cuenta que, a diferencia de `Select()`, el número de elementos en la salida no necesita ser el mismo que en la entrada.

**Más ejemplos del mundo real**

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

Producción:

>Bob
>Jack
>Jim
    John

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/LNyymI)

## Ningún
`Any` se usa para comprobar si **cualquier** elemento de una colección coincide con una condición o no.
<br/>*ver también: [.All](https://www.wikiod.com/es/docs/c%23/68/linq-queries/2773/all#t=201707041340119289445), [Cualquiera y FirstOrDefault: mejores prácticas] (https://www.wikiod.com/es/docs/c%23/68/linq-queries/16731/any-and-firstordefault-best-practice#t=201707041441456087738)*

## 1. Parámetro vacío ##
**Cualquiera**: Devuelve `verdadero` si la colección tiene algún elemento y `falso` si la colección está vacía:

    var numbers = new List<int>();
    bool result = numbers.Any(); // false

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.Any(); //true

## 2. Expresión lambda como parámetro ##
**Cualquiera**: Devuelve `true` si la colección tiene uno o más elementos que cumplen la condición en la expresión lambda:

    var arrayOfStrings = new string[] { "a", "b", "c" };
    arrayOfStrings.Any(item => item == "a");    // true
    arrayOfStrings.Any(item => item == "d");    // false
    
## 3. Colección vacía ##
**Cualquiera**: devuelve `falso` si la colección está vacía y se proporciona una expresión lambda:
  
    var numbers = new List<int>();
    bool result = numbers.Any(i => i >= 0); // false

**Nota:**
`Any` detendrá la iteración de la colección tan pronto como encuentre un elemento que coincida con la condición. Esto significa que la colección no estará necesariamente enumerada en su totalidad; solo se enumerará lo suficiente como para encontrar el primer elemento que coincida con la condición.

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/IQ4wG4)


## UNIONES
Las uniones se utilizan para combinar diferentes listas o tablas que contienen datos a través de una clave común.

Al igual que en SQL, los siguientes tipos de uniones son compatibles con LINQ: <br/>
**Uniones internas, izquierdas, derechas, cruzadas** y **externas completas**.

Las siguientes dos listas se utilizan en los siguientes ejemplos:

    var first = new List<string>(){ "a","b","c"}; // Left data
    var second = new List<string>(){ "a", "c", "d"}; // Right data

## (Unir internamente

    var result = from f in first
                 join s in second on f equals s
                 select new { f, s };

    var result = first.Join(second, 
                            f => f, 
                            s => s,
                            (f, s) => new { f, s });

    // Result: {"a","a"}
    //         {"c","c"}

## Izquierda combinación externa
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




## Unión exterior derecha
    var rightOuterJoin = from s in second
                         join f in first on s equals f into temp
                         from t in temp.DefaultIfEmpty()
                         select new {First=t,Second=s};

    // Result: {"a","a"}
    //         {"c","c"}  
    //         {null,"d"}  


## Unión cruzada

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

## Unión exterior completa

    var fullOuterjoin = leftOuterJoin.Union(rightOuterJoin);

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  
    //         {null,"d"}

## **Ejemplo práctico** ##
Los ejemplos anteriores tienen una estructura de datos simple para que pueda concentrarse en comprender técnicamente las diferentes uniones de LINQ, pero en el mundo real tendría tablas con columnas que necesita unir.

En el siguiente ejemplo, solo se usa una clase "Región", en realidad uniría dos o más tablas diferentes que tienen la misma clave (en este ejemplo, "primera" y "segunda" se unen a través de la clave común "ID" ).

**Ejemplo:** Considere la siguiente estructura de datos:

    public class Region 
    {
        public Int32 ID;
        public string RegionDescription;
        
        public Region(Int32 pRegionID, string pRegionDescription=null)
        {
            ID = pRegionID; RegionDescription = pRegionDescription;
        }
    }

Ahora prepare los datos (es decir, complete con datos):

    // Left data
    var first = new List<Region>() 
                     { new Region(1), new Region(3), new Region(4) }; 
    // Right data
    var second = new List<Region>() 
                     { 
                        new Region(1, "Eastern"),  new Region(2, "Western"),
                        new Region(3, "Northern"), new Region(4, "Southern")
                     }; 

Puede ver que en este ejemplo `primero` no contiene descripciones de ninguna región, por lo que desea unirlas desde `segundo`. Entonces la unión interna se vería así:

    // do the inner join
    var result = from f in first
                 join s in second on f.ID equals s.ID
                 select new { f.ID, s.RegionDescription };
 

     // Result: {1,"Eastern"}
     //         {3, Northern}  
     //         {4,"Southern"}  

Este resultado ha creado objetos anónimos sobre la marcha, lo cual está bien, pero ya hemos creado una clase adecuada, por lo que podemos especificarla: en lugar de `select new { f.ID, s.RegionDescription };` podemos decir `select new Region(f.ID, s.RegionDescription);`, que devolverá los mismos datos pero creará objetos de tipo `Region`, que mantendrán la compatibilidad con los otros objetos.



[Demostración en vivo en .NET fiddle](https://dotnetfiddle.net/pP6enP)


## Saltar y tomar
El método Skip devuelve una colección que excluye una cantidad de elementos desde el principio de la colección de origen. El número de elementos excluidos es el número dado como argumento. Si hay menos elementos en la colección que los especificados en el argumento, se devuelve una colección vacía.

El método Take devuelve una colección que contiene varios elementos desde el principio de la colección de origen. El número de elementos incluidos es el número dado como argumento. Si hay menos elementos en la colección que los especificados en el argumento, la colección devuelta contendrá los mismos elementos que la colección de origen.

    var values = new [] { 5, 4, 3, 2, 1 };

    var skipTwo        = values.Skip(2);         // { 3, 2, 1 }
    var takeThree      = values.Take(3);         // { 5, 4, 3 }
    var skipOneTakeTwo = values.Skip(1).Take(2); // { 4, 3 }
    var takeZero       = values.Take(0);         // An IEnumerable<int> with 0 items

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/U2b76y)

**Omitir y Tomar** se usan comúnmente juntos para paginar resultados, por ejemplo:

    IEnumerable<T> GetPage<T>(IEnumerable<T> collection, int pageNumber, int resultsPerPage) {
        int startIndex = (pageNumber - 1) * resultsPerPage;
        return collection.Skip(startIndex).Take(resultsPerPage);
    }


> **Advertencia:** LINQ to Entities solo admite Saltar en [consultas ordenadas][1]. Si intenta usar Omitir sin realizar un pedido, obtendrá una **NotSupportedException** con el mensaje "El método 'Omitir' solo se admite para la entrada ordenada en LINQ to Entities. Se debe llamar al método 'OrderBy' antes que al método 'Omitir '."


[1]: https://www.wikiod.com/es/docs/c%23/68/linq-queries/4389/query-ordering#t=201607261110520529272

## Definición de una variable dentro de una consulta Linq (palabra clave let)
Para definir una variable dentro de una expresión linq, puede usar la palabra clave **let**. Esto generalmente se hace para almacenar los resultados de subconsultas intermedias, por ejemplo:

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

**Producción:**

>El promedio de los números es 4.5.
>El resultado de la consulta incluye el número 3 con un cuadrado de 9.
>El resultado de la consulta incluye el número 4 con un cuadrado de 16.
>El resultado de la consulta incluye el número 5 con un cuadrado de 25.
>El resultado de la consulta incluye el número 6 con un cuadrado de 36.
>El resultado de la consulta incluye el número 7 con un cuadrado de 49.
>El resultado de la consulta incluye el número 8 con un cuadrado de 64.
>El resultado de la consulta incluye el número 9 con un cuadrado de 81.

[Ver demostración][1]


[1]: https://dotnetfiddle.net/zbjrHZ

## Cremallera
El método de extensión `Zip` actúa sobre dos colecciones. Empareja cada elemento de las dos series en función de su posición. Con una instancia de `Func`, usamos `Zip` para manejar elementos de las dos colecciones de C# en pares. Si las series difieren en tamaño, se ignorarán los elementos adicionales de la serie más grande.

Para tomar un ejemplo del libro "C# en pocas palabras",

    int[] numbers = { 3, 5, 7 };
    string[] words = { "three", "five", "seven", "ignored" };
    IEnumerable<string> zip = numbers.Zip(words, (n, w) => n + "=" + w);

**Producción:**

>3=tres
>5=cinco
>7=siete

[Ver demostración][1]


[1]: https://dotnetfiddle.net/nIA5E9

## Rango y repetición
Los métodos estáticos `Range` y `Repeat` en `Enumerable` se pueden usar para generar secuencias simples.

**Rango**
---------

`Enumerable.Range()` genera una secuencia de enteros dado un valor inicial y un conteo.

    // Generate a collection containing the numbers 1-100 ([1, 2, 3, ..., 98, 99, 100])
    var range = Enumerable.Range(1,100);

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/jA0VB1)

**Repetir**
------

`Enumerable.Repeat()` genera una secuencia de elementos repetidos dado un elemento y el número de repeticiones requeridas.

    // Generate a collection containing "a", three times (["a","a","a"])
    var repeatedValues = Enumerable.Repeat("a", 3);

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/KpZfpt)

## Todos
`All` se utiliza para comprobar si todos los elementos de una colección cumplen una condición o no.
<br/>*ver también: [.Any](https://www.wikiod.com/es/docs/c%23/68/linq-queries/5098/any#t=201707041342119744775)*
## 1. Parámetro vacío ##

**Todos**: no se permite su uso con un parámetro vacío.

## 2. Expresión lambda como parámetro ##

**Todos**: Devuelve `verdadero` si todos los elementos de la colección satisfacen la expresión lambda y `falso` en caso contrario:

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.All(i => i < 10); // true
    bool result = numbers.All(i => i >= 3); // false

## 3. Colección vacía ##

**Todos**: Devuelve `verdadero` si la colección está vacía y se proporciona una expresión lambda:

    var numbers = new List<int>();
    bool result = numbers.All(i => i >= 0); // true

**Nota:**
`All` detendrá la iteración de la colección tan pronto como encuentre un elemento que **no** coincida con la condición. Esto significa que la colección no estará necesariamente enumerada en su totalidad; solo se enumerará lo suficiente como para encontrar el primer elemento **que no coincida** con la condición.


## Conceptos básicos
LINQ es muy útil para consultar colecciones (o matrices).

Por ejemplo, dados los siguientes datos de ejemplo:

    var classroom = new Classroom
    {
        new Student { Name = "Alice", Grade = 97, HasSnack = true  },
        new Student { Name = "Bob",   Grade = 82, HasSnack = false },
        new Student { Name = "Jimmy", Grade = 71, HasSnack = true  },
        new Student { Name = "Greg",  Grade = 90, HasSnack = false },
        new Student { Name = "Joe",   Grade = 59, HasSnack = false }
    }

Podemos "consultar" estos datos usando la sintaxis LINQ. Por ejemplo, para recuperar a todos los alumnos que meriendan hoy:

    var studentsWithSnacks = from s in classroom.Students
                             where s.HasSnack
                             select s;

O bien, para recuperar estudiantes con una calificación de 90 o superior, y solo devolver sus nombres, no el objeto 'Estudiante' completo:

    var topStudentNames = from s in classroom.Students
                          where s.Grade >= 90
                          select s.Name;

La función LINQ se compone de dos sintaxis que realizan las mismas funciones, tienen un rendimiento casi idéntico, pero están escritas de manera muy diferente. La sintaxis del ejemplo anterior se denomina **sintaxis de consulta**. Sin embargo, el siguiente ejemplo ilustra la **sintaxis del método**. Se devolverán los mismos datos que en el ejemplo anterior, pero la forma en que se escribe la consulta es diferente.

    var topStudentNames = classroom.Students
                                   .Where(s => s.Grade >= 90)
                                   .Select(s => s.Name);
                                        

## Agregado
`Agregado` Aplica una función de acumulador sobre una secuencia.
 
    int[] intList = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    int sum = intList.Aggregate((prevSum, current) => prevSum + current);
    // sum = 55

- En el primer paso `prevSum = 1`
- En el segundo `prevSum = prevSum(en
el primer paso) + 2`
- En el i-ésimo paso `prevSum = prevSum(en el (i-1)
paso) + i-ésimo elemento del arreglo`


    string[] stringList = { "Hello", "World", "!" };
    string joinedString = stringList.Aggregate((prev, current) => prev + " " + current);
    // joinedString = "Hello World !"


----------

Una segunda sobrecarga de 'Agregado' también recibe un parámetro 'semilla' que es el valor acumulador inicial. Esto se puede usar para calcular múltiples condiciones en una colección sin iterar más de una vez.

    List<int> items = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

Para la colección de `ítems` queremos calcular

1. El `.Count` total
2. La cantidad de números pares
3. Recoge cada artículo

Usando `Agregado` se puede hacer así:

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

_Tenga en cuenta que al usar un tipo anónimo como semilla, debe crear una instancia de un nuevo objeto en cada elemento porque las propiedades son de solo lectura. Usando una clase personalizada, uno puede simplemente asignar la información y no se necesita 'nueva' (solo cuando se proporciona el parámetro 'semilla' inicial_

## SelectMany: aplanar una secuencia de secuencias
    var sequenceOfSequences = new [] { new [] { 1, 2, 3 }, new [] { 4, 5 }, new [] { 6 } };
    var sequence = sequenceOfSequences.SelectMany(x => x);
    // returns { 1, 2, 3, 4, 5, 6 }

Use `SelectMany()` si tiene, o si está creando una secuencia de secuencias, pero desea que el resultado sea una secuencia larga.

En la sintaxis de consulta LINQ:

    var sequence = from subSequence in sequenceOfSequences
                   from item in subSequence
                   select item;

Si tiene una colección de colecciones y le gustaría poder trabajar con datos de la colección principal y secundaria al mismo tiempo, también es posible con `SelectMany`.

Definamos clases simples

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

Supongamos que tenemos la siguiente colección.

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

Ahora queremos seleccionar los comentarios `Contenido` junto con `Id` de `BlogPost` asociado con este comentario. Para hacerlo, podemos usar la sobrecarga `SelectMany` apropiada.

    var commentsWithIds = posts.SelectMany(p => p.Comments, (post, comment) => new { PostId = post.Id, CommentContent = comment.Content });

Nuestros `commentsWithIds` se ven así

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
Devuelve valores únicos de un `IEnumerable`. La singularidad se determina mediante el comparador de igualdad predeterminado.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

    var distinct = array.Distinct();
    // distinct = { 1, 2, 3, 4, 5 }

Para comparar un tipo de datos personalizado, debemos implementar la interfaz `IEquatable<T>` y proporcionar los métodos `GetHashCode` y `Equals` para el tipo. O el comparador de igualdad puede ser anulado:

    class SSNEqualityComparer : IEqualityComparer<Person> {
        public bool Equals(Person a, Person b) => return a.SSN == b.SSN;
        public int GetHashCode(Person p) => p.SSN;
    }

    List<Person> people;

    distinct = people.Distinct(SSNEqualityComparer);

## Colección de consultas por tipo / elementos de conversión a tipo
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

Usando `Dónde`

    var foos = collection.Where(item => item is Foo); // result: IEnumerable<IFoo> with item0 and item1
    var bars = collection.Where(item => item is Bar); // result: IEnumerable<IFoo> with item2 and item3

Usando 'Transmitir'

    var bars = collection.Cast<Bar>();                // throws InvalidCastException on the 1st item
    var foos = collection.Cast<Foo>();                // throws InvalidCastException on the 3rd item
    var foosAndBars = collection.Cast<IFoo>();        // OK 
    

## Agrupar por
GroupBy es una manera fácil de ordenar una colección de elementos `IEnumerable<T>` en grupos distintos.
## Ejemplo sencillo ##
En este primer ejemplo, terminamos con dos grupos, elementos pares e impares.

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

## Ejemplo más complejo ##

Tomemos como ejemplo la agrupación de una lista de personas por edad.
Primero, crearemos un objeto Persona que tiene dos propiedades, Nombre y Edad.

    public class Person
    {
        public int Age {get; set;}
        public string Name {get; set;}
    }

Luego creamos nuestra lista de muestra de personas con varios nombres y edades.

    List<Person> people = new List<Person>();
    people.Add(new Person{Age = 20, Name = "Mouse"});
    people.Add(new Person{Age = 30, Name = "Neo"});
    people.Add(new Person{Age = 40, Name = "Morpheus"});
    people.Add(new Person{Age = 30, Name = "Trinity"});
    people.Add(new Person{Age = 40, Name = "Dozer"});
    people.Add(new Person{Age = 40, Name = "Smith"});

Luego creamos una consulta LINQ para agrupar nuestra lista de personas por edad.

    var query = people.GroupBy(x => x.Age);

Al hacerlo, podemos ver la Edad de cada grupo y tener una lista de cada persona en el grupo.

    foreach(var result in query)
    {
        Console.WriteLine(result.Key);
                    
        foreach(var person in result)
            Console.WriteLine(person.Name);
    }

Esto da como resultado el siguiente resultado:

    20
    Mouse
    30
    Neo
    Trinity
    40
    Morpheus
    Dozer
    Smith

Puedes jugar con la [demostración en vivo en .NET Fiddle][1]


[1]: https://dotnetfiddle.net/VFOZ1x

## Enumerando lo Enumerable
La interfaz IEnumerable<<a>T> es la interfaz base para todos los enumeradores genéricos y es una parte esencial de la comprensión de LINQ. En esencia, representa la secuencia.

Esta interfaz subyacente la heredan todas las colecciones genéricas, como [Collection<<a>T>](https://msdn.microsoft.com/en-us/library/ms132397(v=vs.110).aspx ), [Array](https://msdn.microsoft.com/en-us/library/system.array(v=vs.110).aspx), [List<<a>T>](https:// msdn.microsoft.com/en-us/library/6sh2ey19(v=vs.110).aspx), [Diccionario<TKey, TValue> Clase](https://msdn.microsoft.com/en-us/library/ xfhwa508(v=vs.110).aspx) y [HashSet<<a>T>](https://msdn.microsoft.com/en-us/library/bb359438(v=vs.110).aspx) .

Además de representar la secuencia, cualquier clase que herede de IEnumerable<<a>T> debe proporcionar un IEnumerator<<a>T>. El enumerador expone el iterador para el enumerable, y estas dos interfaces e ideas interconectadas son la fuente del dicho "enumerar el enumerable".

"Enumerar lo enumerable" es una frase importante. El enumerable es simplemente una estructura sobre cómo iterar, no contiene ningún objeto materializado. Por ejemplo, al ordenar, un enumerable puede contener los criterios del campo a ordenar, pero usar `.OrderBy()` en sí mismo devolverá un IEnumerable<<a>T> que solo sabe *cómo* ordenar. El uso de una llamada que materializará los objetos, como en la iteración del conjunto, se conoce como enumeración (por ejemplo, `.ToList()`). El proceso de enumeración utilizará la definición enumerable de *cómo* para moverse a través de la serie y devolver los objetos relevantes (en orden, filtrados, proyectados, etc.).

Solo una vez que se ha enumerado el enumerable, provoca la materialización de los objetos, que es cuando métricas como [complejidad del tiempo] (https://en.wikipedia.org/wiki/Time_complexity) (cuánto tiempo debe tomar en relación con el tamaño de la serie ) y se puede medir la complejidad espacial (cuánto espacio debe usar en relación con el tamaño de la serie).

Crear su propia clase que herede de IEnumerable<<a>T> puede ser un poco complicado según la serie subyacente que debe ser enumerable. En general, es mejor utilizar una de las colecciones genéricas existentes. Dicho esto, también es posible heredar de la interfaz IEnumerable<<a>T> sin tener una matriz definida como estructura subyacente.

Por ejemplo, usando la serie de Fibonacci como la secuencia subyacente. Tenga en cuenta que la llamada a 'Dónde' simplemente crea un 'IEnumerable', y no es hasta que se realiza una llamada para enumerar ese enumerable que cualquiera de los valores se materializa.

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

Producción

    Enumerable built
    Enumerating the Enumerable
    4052739537881
    Enumerating the Enumerable
    4052739537881
    Enumerable built
    Enumerating the Enumerable
    14930352

La fuerza en el segundo conjunto (el fibMod612) es que a pesar de que hicimos la llamada para ordenar nuestro conjunto completo de números de Fibonacci, dado que solo se tomó un valor usando `.First()`, la complejidad del tiempo era O(n) como solo Se necesitaba comparar 1 valor durante la ejecución del algoritmo de pedido. Esto se debe a que nuestro enumerador solo solicitó 1 valor, por lo que no se tuvo que materializar todo el enumerable. Si hubiéramos usado `.Take(5)` en lugar de `.First()`, el enumerador habría pedido 5 valores y, como máximo, habría que materializar 5 valores. En comparación con la necesidad de pedir un conjunto completo *y luego* tomar los primeros 5 valores, el principio de ahorra mucho tiempo y espacio de ejecución.



## Dónde
Devuelve un subconjunto de elementos para los que el predicado especificado es verdadero.

    List<string> trees = new List<string>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

## Sintaxis del método

    // Select all trees with name of length 3
    var shortTrees = trees.Where(tree => tree.Length == 3); // Oak, Elm

## Sintaxis de consulta

    var shortTrees = from tree in trees
                     where tree.Length == 3
                     select tree; // Oak, Elm


## Uso de Range con varios métodos Linq
Puede usar la clase Enumerable junto con las consultas de Linq para convertir bucles for en Linq one liners.

**Ejemplo seleccionado**

Opuesto a hacer esto:


    var asciiCharacters = new List<char>();
    for (var x = 0; x < 256; x++)
    {
        asciiCharacters.Add((char)x);
    }

Puedes hacerlo:

    var asciiCharacters = Enumerable.Range(0, 256).Select(a => (char) a);


**Donde Ejemplo**

En este ejemplo se generarán 100 números y se extraerán pares

    var evenNumbers = Enumerable.Range(1, 100).Where(a => a % 2 == 0);



## Usar SelectMany en lugar de bucles anidados
Dadas 2 listas

    var list1 = new List<string> { "a", "b", "c" };
    var list2 = new List<string> { "1", "2", "3", "4" };

si desea generar todas las permutaciones, puede usar bucles anidados como

    var result = new List<string>();
    foreach (var s1 in list1)
        foreach (var s2 in list2)
            result.Add($"{s1}{s2}");

Usando SelectMany puedes hacer la misma operación que

    var result = list1.SelectMany(x => list2.Select(y => $"{x}{y}", x, y)).ToList();


## Contiene
MSDN:
> Determina si una secuencia contiene un elemento específico usando un
> especificó `IEqualityComparer<T>`


    List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
    var result1 = numbers.Contains(4); // true
    var result2 = numbers.Contains(8); // false

    List<int> secondNumberCollection = new List<int> { 4, 5, 6, 7 };
    // Note that can use the Intersect method in this case
    var result3 = secondNumberCollection.Where(item => numbers.Contains(item)); // will be true only for 4,5


Usando un objeto definido por el usuario:

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

Usando la sobrecarga `Enumerable.Contains(value, comparer)`:

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

**Un uso inteligente de ````Contains```` sería reemplazar varias cláusulas ````if```` por una llamada ````Contains````.**

Así que en lugar de hacer esto:

    if(status == 1 || status == 3 || status == 4)
    {
        //Do some business operation
    }
    else
    {
        //Do something else
    }
 
Hacer esto:
    
    if(new int[] {1, 3, 4 }.Contains(status)
    {
        //Do some business operaion
    }
    else 
    {
        //Do something else
    }

## GroupBy uno o varios campos
Supongamos que tenemos algún modelo de película:

    public class Film {
        public string Title { get; set; }
        public string Category { get; set; }
        public int Year { get; set; }
    }

Agrupar por propiedad de categoría:

    foreach (var grp in films.GroupBy(f => f.Category)) {
        var groupCategory = grp.Key;
        var numberOfFilmsInCategory = grp.Count();
    }

Agrupar por Categoría y Año:

    foreach (var grp in films.GroupBy(f => new { Category = f.Category, Year = f.Year })) {
        var groupCategory = grp.Key.Category;
        var groupYear = grp.Key.Year;
        var numberOfFilmsInCategory = grp.Count();
    }



## Ordenación de consultas - OrderBy() ThenBy() OrderByDescending() ThenByDescending()
    string[] names= { "mark", "steve", "adam" };

**Ascendente:**

*Sintaxis de consulta*

    var sortedNames =
        from name in names
        orderby name
        select name;

*Sintaxis del método*

    var sortedNames = names.OrderBy(name => name);

sortedNames contiene los nombres en el siguiente orden:
"adán","marca","steve"

**Descendente:**


*Sintaxis de consulta*

    var sortedNames =
        from name in names
        orderby name descending
        select name;

*Sintaxis del método*

    var sortedNames = names.OrderByDescending(name => name);

sortedNames contiene los nombres en el siguiente orden:
"steve","marca","adán"

**Ordenar por varios campos**

    Person[] people =
    {
        new Person { FirstName = "Steve", LastName = "Collins", Age = 30},
        new Person { FirstName = "Phil" , LastName = "Collins", Age = 28},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 29},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 15}
    };

*Sintaxis de consulta*

    var sortedPeople = from person in people
                       orderby person.LastName, person.FirstName, person.Age descending
                       select person;

*Sintaxis del método*

     sortedPeople = people.OrderBy(person => person.LastName)
                          .ThenBy(person => person.FirstName)
                          .ThenByDescending(person => person.Age);

*Resultado*

    1. Adam Ackerman 29
    2. Adam Ackerman 15
    3. Phil Collins  28
    4. Steve Collins 30



## AlDiccionario
El método LINQ `ToDictionary()` se puede usar para generar una colección `Dictionary<TKey, TElement>` basada en una fuente dada `IEnumerable<T>`.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, User> usersById = users.ToDictionary(x => x.Id);

En este ejemplo, el único argumento pasado a `ToDictionary` es del tipo `Func<TSource, TKey>`, que devuelve la clave para cada elemento.

Esta es una forma concisa de realizar la siguiente operación:

    Dictionary<int, User> usersById = new Dictionary<int User>();
    foreach (User u in users) 
    {
      usersById.Add(u.Id, u);
    }

También puede pasar un segundo parámetro al método `ToDictionary`, que es del tipo `Func<TSource, TElement>` y devuelve el `Valor` que se agregará para cada entrada.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, string> userNamesById = users.ToDictionary(x => x.Id, x => x.Name);

También es posible especificar el `IComparer` que se utiliza para comparar valores clave. Esto puede ser útil cuando la clave es una cadena y desea que coincida sin distinguir entre mayúsculas y minúsculas.

    IEnumerable<User> users = GetUsers();
    Dictionary<string, User> usersByCaseInsenstiveName = users.ToDictionary(x => x.Name, StringComparer.InvariantCultureIgnoreCase);

    var user1 = usersByCaseInsenstiveName["john"];
    var user2 = usersByCaseInsenstiveName["JOHN"];
    user1 == user2; // Returns true

Nota: el método `ToDictionary` requiere que todas las claves sean únicas, no debe haber claves duplicadas. Si los hay, se lanza una excepción: `ArgumentException: ya se ha agregado un elemento con la misma clave.` Si tiene un escenario en el que sabe que tendrá varios elementos con la misma clave, es mejor que use [`ToLookup`](https://www.wikiod.com/es/docs/c%23/68/linq-queries/14871/tolookup) en su lugar.




## Saltar Mientras
`SkipWhile()` se usa para excluir elementos hasta la primera no coincidencia (esto puede ser contrario a la intuición para la mayoría)

    int[] list = { 42, 42, 6, 6, 6, 42 };
    var result = list.SkipWhile(i => i == 42); 
    // Result: 6, 6, 6, 42

## Predeterminado si está vacío
DefaultIfEmpty se usa para devolver un elemento predeterminado si la secuencia no contiene elementos. Este elemento puede ser el predeterminado del tipo o una instancia definida por el usuario de ese tipo. Ejemplo:

    var chars = new List<string>() { "a", "b", "c", "d" };

    chars.DefaultIfEmpty("N/A").FirstOrDefault(); // returns "a";
    
    chars.Where(str => str.Length > 1)
         .DefaultIfEmpty("N/A").FirstOrDefault(); // return "N/A"

    chars.Where(str => str.Length > 1)
            .DefaultIfEmpty().First(); // returns null;

**Uso en combinaciones izquierdas**:
--------------------

Con `DefaultIfEmpty`, el Linq Join tradicional puede devolver un objeto predeterminado si no se encuentra ninguna coincidencia. Actuando así como la combinación izquierda de SQL. Ejemplo:

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

En el caso de que se utilice `DefaultIfEmpty` (sin especificar un valor predeterminado) y que no haya elementos coincidentes en la secuencia correcta, debe asegurarse de que el objeto no sea `nulo` antes de acceder a sus propiedades. De lo contrario, dará como resultado una `NullReferenceException`. Ejemplo:

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



## Secuencia Igual
`SequenceEqual` se usa para comparar dos secuencias `IEnumerable<T>` entre sí.


    int[] a = new int[] {1, 2, 3};
    int[] b = new int[] {1, 2, 3};
    int[] c = new int[] {1, 3, 2};

    bool returnsTrue = a.SequenceEqual(b);
    bool returnsFalse = a.SequenceEqual(c);

## ElementAt y ElementAtOrDefault
`ElementAt` devolverá el elemento en el índice `n`. Si `n` no está dentro del rango del enumerable, lanza una excepción `ArgumentOutOfRangeException`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAt(2);  // 3
    numbers.ElementAt(10); // throws ArgumentOutOfRangeException

`ElementAtOrDefault` devolverá el elemento en el índice `n`. Si `n` no está dentro del rango del enumerable, devuelve `default(T)`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAtOrDefault(2);  // 3
    numbers.ElementAtOrDefault(10); // 0 = default(int)

Tanto `ElementAt` como `ElementAtOrDefault` están optimizados para cuando la fuente es un `IList<T>` y se usará la indexación normal en esos casos.

Tenga en cuenta que para `ElementAt`, si el índice provisto es mayor que el tamaño de `IList<T>`, la lista debería (pero técnicamente no se garantiza que lo haga) lanzar una `ArgumentOutOfRangeException`.


## Uniendo múltiples secuencias
Considere las entidades 'Cliente', 'Compra' y 'Artículo de compra' de la siguiente manera:

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

Considere los siguientes datos de muestra para las entidades anteriores:

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

Ahora, considere la siguiente consulta linq:

    var result = from c in customers
                join p in purchases on c.Id equals p.CustomerId           // first join
                join pi in purchaseItems on p.Id equals pi.PurchaseId     // second join
                select new
                {
                   c.Name, p.Description, pi.Detail
                };


Para generar el resultado de la consulta anterior:

    foreach(var resultItem in result)
    {
        Console.WriteLine($"{resultItem.Name}, {resultItem.Description}, {resultItem.Detail}");
    }
        
El resultado de la consulta sería:

> Cliente1, Cliente1-Compra1, Compra1-Artículo de compra1
> 
> Cliente1, Cliente1-Compra2, Compra2-Artículo1
> 
> Cliente1, Cliente1-Compra2, Compra2-Artículo de compra2
> 
> Cliente2, Cliente2-Compra2, Compra3-Artículo de compra1

[Demostración en vivo en .NET Fiddle][1]


[1]: https://dotnetfiddle.net/Db8uqp

## Unirse en varias claves
  

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

Tenga en cuenta que los tipos anónimos en `join` anterior deben contener las mismas propiedades, ya que los objetos se consideran iguales solo si todas sus propiedades son iguales. De lo contrario, la consulta no se compilará.

## Suma
El método de extensión `Enumerable.Sum` calcula la suma de valores numéricos.

En caso de que los elementos de la colección sean en sí mismos números, puede calcular la suma directamente.

    int[] numbers = new int[] { 1, 4, 6 };
    Console.WriteLine( numbers.Sum() ); //outputs 11

En caso de que el tipo de los elementos sea de tipo complejo, puede utilizar una expresión lambda para especificar el valor que debe calcularse:

    var totalMonthlySalary = employees.Sum( employee => employee.MonthlySalary );

El método de extensión de suma puede calcular con los siguientes tipos:

- Int32
- Int64
- Único
- Doble
- Decimales

En caso de que su colección contenga tipos anulables, puede usar el operador de fusión nula para establecer un valor predeterminado para los elementos nulos:

    int?[] numbers = new int?[] { 1, null, 6 };
    Console.WriteLine( numbers.Sum( number => number ?? 0 ) ); //outputs 7

## Para buscar
> ToLookup devuelve una estructura de datos que permite la indexación. Es un
> método de extensión. Produce una instancia de ILookup que se puede indexar
> o enumerado usando un bucle foreach. Las entradas se combinan en
> agrupaciones en cada tecla. - perlas dotnet

    string[] array = { "one", "two", "three" };
    //create lookup using string length as key
    var lookup = array.ToLookup(item => item.Length);
    
    //join the values whose lengths are 3
    Console.WriteLine(string.Join(",",lookup[3]));
    //output: one,two

Otro ejemplo:

    int[] array = { 1,2,3,4,5,6,7,8 };
    //generate lookup for odd even numbers (keys will be 0 and 1)
    var lookup = array.ToLookup(item => item % 2);
    
    //print even numbers after joining
    Console.WriteLine(string.Join(",",lookup[0]));
    //output: 2,4,6,8

    //print odd numbers after joining
    Console.WriteLine(string.Join(",",lookup[1]));
    //output: 1,3,5,7

## Cualquiera y primero (o por defecto) - mejores prácticas
No explicaré qué hacen `Any` y `FirstOrDefault` porque ya hay dos buenos ejemplos sobre ellos. Consulte https://www.wikiod.com/es/docs/c%23/68/linq-queries/5098/any#t=201707200324548979636 y https://www.wikiod.com/es/docs/c%23/68/linq-queries/329 /first-firstordefault-last-lastordefault-single-and-singleordefault#t=201707200328069088515 para obtener más información.

Un patrón que veo a menudo en el código que **debe evitarse** es

    if (myEnumerable.Any(t=>t.Foo == "Bob"))
    {
        var myFoo = myEnumerable.First(t=>t.Foo == "Bob");
        //Do stuff
    }

Podría escribirse más eficientemente así

    var myFoo = myEnumerable.FirstOrDefault(t=>t.Foo == "Bob");
    if (myFoo != null)
    {
        //Do stuff
    }

Usando el segundo ejemplo, la colección se busca solo una vez y da el mismo resultado que el primero. La misma idea se puede aplicar a `Single`.

## Agrupar por suma y conteo
Tomemos una clase de muestra:

    public class Transaction
    {
        public string Category { get; set; }
        public DateTime Date { get; set; }
        public decimal Amount { get; set; }
    }

Ahora, consideremos una lista de transacciones:

    var transactions = new List<Transaction>
    {
       new Transaction { Category = "Saving Account", Amount = 56, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Saving Account", Amount = 10, Date = DateTime.Today.AddDays(-10) },
       new Transaction { Category = "Credit Card", Amount = 15, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Credit Card", Amount = 56, Date = DateTime.Today },
       new Transaction { Category = "Current Account", Amount = 100, Date = DateTime.Today.AddDays(5) },
    };

Si desea calcular la suma de la cantidad y el conteo por categoría, puede usar GroupBy de la siguiente manera:

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

Alternativamente, puede hacer esto en un solo paso:

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

La salida para ambas consultas anteriores sería la misma:

> Categoría: Cuenta de Ahorro, Monto: 66, Cantidad: 2
>
> Categoría: Tarjeta de crédito, Monto: 71, Cantidad: 2
>
> Categoría: Cuenta Corriente, Importe: 100, Recuento: 1

[Demostración en vivo en .NET Fiddle][1]


[1]: https://dotnetfiddle.net/1PfLGq#

## Ordenar por
> Ordena una colección por un valor especificado.

Cuando el valor es un **entero**, **doble** o **flotante**, comienza con el *valor mínimo*, lo que significa que obtiene primero los valores negativos, luego cero y luego los valores positivos (ver Ejemplo 1).

Cuando ordena por un **carácter**, el método compara los *valores ascii* de los caracteres para ordenar la colección (vea el Ejemplo 2).

Cuando ordena **cadenas**, el método OrderBy las compara observando su [CultureInfo][1], pero normalmente comienza con la *primera letra* del alfabeto (a,b,c...).

Este tipo de orden se llama ascendente, si lo desea al revés, necesita descender (consulte OrderByDescending).

**Ejemplo 1:**

    int[] numbers = {2, 1, 0, -1, -2};
    IEnumerable<int> ascending = numbers.OrderBy(x => x);
    // returns {-2, -1, 0, 1, 2}

**Ejemplo 2:**

     char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
     IEnumerable<char> ascending = letters.OrderBy(x => x);
     // returns { ' ', '!', '+', '1', '9', '?', 'A', 'B', 'Y', 'Z', '[', 'a', 'b', 'y', 'z', '{' }

**Ejemplo:**

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

## Seleccionar - Transformar elementos
Seleccionar le permite aplicar una transformación a cada elemento en cualquier estructura de datos que implemente IEnumerable.

Obtener el primer carácter de cada cadena en la siguiente lista:

    List<String> trees = new List<String>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };
    
Uso de sintaxis regular (lambda)

    //The below select stament transforms each element in tree into its first character.
    IEnumerable<String> initials = trees.Select(tree => tree.Substring(0, 1));
    foreach (String initial in initials) {
        System.Console.WriteLine(initial);
    }

**Producción:**
>O
>B
>B
>E
>H
>M

[Demostración en vivo en .NET Fiddle][1]

Uso de la sintaxis de consulta LINQ

    initials = from tree in trees
               select tree.Substring(0, 1);


[1]: https://dotnetfiddle.net/yYLT0K

## Unión
Fusiona dos colecciones para crear una colección distinta utilizando el comparador de igualdad predeterminado

    int[] numbers1 = { 1, 2, 3 };
    int[] numbers2 = { 2, 3, 4, 5 };
    
    var allElement = numbers1.Union(numbers2);   // AllElement now contains 1,2,3,4,5


[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/oet2Uq)

## Cuenta y cuenta larga
`Count` devuelve el número de elementos en `IEnumerable<T>`. `Count` también expone un parámetro de predicado opcional que le permite filtrar los elementos que desea contar.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };
    
    int n = array.Count(); // returns the number of elements in the array
    int x = array.Count(i => i > 2); // returns the number of elements in the array greater than 2

`LongCount` funciona de la misma manera que `Count` pero tiene un tipo de retorno `long` y se usa para contar secuencias `IEnumerable<T>` que son más largas que `int.MaxValue`

    int[] array = GetLargeArray();
    
    long n = array.LongCount(); // returns the number of elements in the array
    long x = array.LongCount(i => i > 100); // returns the number of elements in the array greater than 100


## Construcción incremental de una consulta
Debido a que LINQ usa **ejecución diferida**, podemos tener un objeto de consulta que en realidad no contiene los valores, pero devolverá los valores cuando se evalúe. Por lo tanto, podemos construir dinámicamente la consulta en función de nuestro flujo de control y evaluarla una vez que hayamos terminado:

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

Podemos agregar un orden de clasificación a la consulta en función de una condición:

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
        
Nuestra consulta se puede definir para comenzar desde un punto dado:
        
        query = query.Skip(start - 1);
        
y definido para devolver un número específico de registros:

        if (count > -1) {
            query = query.Take(count);
        }
        return query;
    }
    
---

Una vez que tenemos el objeto de consulta, podemos evaluar los resultados con un bucle `foreach`, o uno de los métodos LINQ que devuelve un conjunto de valores, como `ToList` o `ToArray`:

    SearchModel sm;
    
    // populate the search model here
    // ...
    
    List<VehicleModel> list = BuildQuery(5, sm).ToList();

## GroupJoin con variable de rango exterior


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



## Cuantificadores Linq
Las operaciones de cuantificador devuelven un valor booleano si algunos o todos los elementos de una secuencia cumplen una condición. En este artículo, veremos algunos escenarios comunes de LINQ to Objects donde podemos usar estos operadores.
Hay 3 operaciones de cuantificadores que se pueden usar en LINQ:

`Todos`: se utiliza para determinar si todos los elementos de una secuencia cumplen una condición.
P.ej:

    int[] array = { 10, 20, 30 }; 
       
    // Are all elements >= 10? YES
    array.All(element => element >= 10); 
       
    // Are all elements >= 20? NO
    array.All(element => element >= 20);
        
    // Are all elements < 40? YES
    array.All(element => element < 40);
    
`Cualquiera`: se utiliza para determinar si algún elemento de una secuencia satisface una condición.
P.ej:

    int[] query=new int[] { 2, 3, 4 }
    query.Any (n => n == 3);

`Contiene`: se utiliza para determinar si una secuencia contiene un elemento específico.
P.ej:

    //for int array
    int[] query =new int[] { 1,2,3 };
    query.Contains(1);
    
    //for string array
    string[] query={"Tom","grey"};
    query.Contains("Tom");
    
    //for a string
    var stringValue="hello";
    stringValue.Contains("h");



## TomarMientras
`TakeWhile` devuelve elementos de una secuencia siempre que la condición sea verdadera

    int[] list = { 1, 10, 40, 50, 44, 70, 4 };
    var result = list.TakeWhile(item => item < 50).ToList();
    // result = { 1, 10, 40 }

## Cree sus propios operadores Linq para IEnumerable<T>
Una de las mejores cosas de Linq es que es tan fácil de extender. Solo necesita crear un [método de extensión][1] cuyo argumento sea `IEnumerable<T>`.

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

Este ejemplo divide los elementos en `IEnumerable<T>` en listas de un tamaño fijo, la última lista contiene el resto de los elementos. Observe cómo se pasa el objeto al que se aplica el método de extensión (argumento `fuente`) como argumento inicial usando la palabra clave `this`. Luego, la palabra clave `yield` se usa para generar el siguiente elemento en la salida `IEnumerable<T>` antes de continuar con la ejecución desde ese punto (ver [palabra clave de rendimiento][2]).

Este ejemplo se usaría en su código de esta manera:

    //using MyNamespace;
    var items = new List<int> { 2, 3, 4, 5, 6 };
    foreach (List<int> sublist in items.Batch(3))
    {
        // do something
    }

En el primer ciclo, la sublista sería `{2, 3, 4}` y en el segundo `{5, 6}`.

Los métodos LinQ personalizados también se pueden combinar con los métodos LinQ estándar. p.ej.:

    //using MyNamespace;
    var result = Enumerable.Range(0, 13)         // generate a list
                           .Where(x => x%2 == 0) // filter the list or do something other
                           .Batch(3)             // call our extension method
                           .ToList()             // call other standard methods

Esta consulta devolverá números pares agrupados en lotes con un tamaño de 3: `{0, 2, 4}, {6, 8, 10}, {12}`

Recuerde que necesita una línea `using MyNamespace;` para poder acceder al método de extensión.

[1]: https://www.wikiod.com/es/docs/c%23/20/extension-methods/33/using-an-extension-method#t=201607280952261411896
[2]: https://www.wikiod.com/es/docs/c%23/61/yield-keyword#t=201607281004094832778

## Reverso

- Invierte el orden de los elementos en una secuencia.
- Si no hay elementos, arroja una `ArgumentNullException: la fuente es nula.`

***Ejemplo:***

    // Create an array.
    int[] array = { 1, 2, 3, 4 };                         //Output:
    // Call reverse extension method on the array.        //4
    var reverse = array.Reverse();                        //3
    // Write contents of array to screen.                 //2
    foreach (int value in reverse)                        //1
        Console.WriteLine(value);
  
[Ejemplo de código en vivo] (https://dotnetfiddle.net/ckrWUo)

Recuerde que `Reverse()` puede funcionar de manera diferente según el orden de la cadena de sus declaraciones LINQ.

            //Create List of chars
            List<int> integerlist = new List<int>() { 1, 2, 3, 4, 5, 6 };

            //Reversing the list then taking the two first elements
            IEnumerable<int> reverseFirst = integerlist.Reverse<int>().Take(2);
            
            //Taking 2 elements and then reversing only thos two
            IEnumerable<int> reverseLast = integerlist.Take(2).Reverse();
            
            //reverseFirst output: 6, 5
            //reverseLast output:  2, 1

[Ejemplo de código en vivo] (https://dotnetfiddle.net/ckrWUo)

*Reverse()* funciona almacenando todo en el búfer y luego recorriéndolo hacia atrás, lo que no es muy eficiente, pero tampoco lo es OrderBy desde esa perspectiva.

En LINQ-to-Objects, hay operaciones de almacenamiento en búfer (Reverse, OrderBy, GroupBy, etc.) y operaciones sin almacenamiento en búfer (Dónde, Tomar, Omitir, etc.).


***Ejemplo: extensión inversa sin almacenamiento en búfer***
    
    public static IEnumerable<T> Reverse<T>(this IList<T> list) {
        for (int i = list.Count - 1; i >= 0; i--) 
            yield return list[i];
    }

[Ejemplo de código en vivo] (https://dotnetfiddle.net/ckrWUo)

Este método puede encontrar problemas si muta la lista mientras itera.





## OrdenDescendente
> Ordena una colección por un valor especificado.

Cuando el valor es un **entero**, **doble** o **flotante**, comienza con el *valor máximo*, lo que significa que obtiene primero los valores positivos, luego cero y luego los valores negativos (ver Ejemplo 1).

Cuando ordena por un **carácter**, el método compara los *valores ascii* de los caracteres para ordenar la colección (vea el Ejemplo 2).

Cuando ordena **cadenas**, el método OrderBy las compara observando su [CultureInfo][1], pero normalmente comienza con la *última letra* del alfabeto (z,y,x,...).

Este tipo de orden se llama descendente, si lo desea al revés, debe ascender (consulte OrderBy).

**Ejemplo 1:**

    int[] numbers = {-2, -1, 0, 1, 2};
    IEnumerable<int> descending = numbers.OrderByDescending(x => x);
    // returns {2, 1, 0, -1, -2}

**Ejemplo 2:**

    char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
    IEnumerable<char> descending = letters.OrderByDescending(x => x);
    // returns { '{', 'z', 'y', 'b', 'a', '[', 'Z', 'Y', 'B', 'A', '?', '9', '1', '+', '!', ' ' }

**Ejemplo 3:**
    
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

## Conectar
Fusiona dos colecciones (sin eliminar duplicados)

    List<int> foo = new List<int> { 1, 2, 3 };
    List<int> bar = new List<int> { 3, 4, 5 };

    // Through Enumerable static class
    var result = Enumerable.Concat(foo, bar).ToList(); // 1,2,3,3,4,5

    // Through extension method
    var result = foo.Concat(bar).ToList(); // 1,2,3,3,4,5

## Seleccione con el selector Func<TSource, int, TResult> - Úselo para obtener la clasificación de los elementos
Una de las sobrecargas de los métodos de extensión 'Select' también pasa el 'índice' del elemento actual en la colección que se está 'seleccionando'. Estos son algunos de sus usos.

**Obtener el "número de fila" de los artículos**

    var rowNumbers = collection.OrderBy(item => item.Property1)
                               .ThenBy(item => item.Property2)
                               .ThenByDescending(item => item.Property3)
                               .Select((item, index) => new { Item = item, RowNumber = index })
                               .ToList();

**Obtener el rango de un elemento *dentro* de su grupo**

    var rankInGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .SelectMany(group => group.OrderBy(item => item.Property2)
                                                       .ThenByDescending(item => item.Property3)
                                                       .Select((item, index) => new 
                                                       { 
                                                           Item = item, 
                                                           RankInGroup = index 
                                                       })).ToList();

**Obtener el ranking de grupos (también conocido en Oracle como dense_rank)**

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

Para probar esto puedes usar:

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

Y datos:

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


