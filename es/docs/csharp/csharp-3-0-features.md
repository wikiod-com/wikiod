---
title: "Características de C# 3.0"
slug: "caracteristicas-de-c-30"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

La versión 3.0 de C# se lanzó como parte de la versión 3.5 de .Net. Muchas de las funciones agregadas con esta versión eran compatibles con LINQ (Language INtegrated Queries).

Lista de características añadidas:

-LINQ
- Expresiones lambda
- Métodos de extensión
- Tipos anónimos
- Variables tipificadas implícitamente
- Inicializadores de objetos y colecciones
- Propiedades implementadas automáticamente
- Árboles de expresión

## Variables tipificadas implícitamente (var)
La palabra clave `var` permite a un programador escribir implícitamente una variable en tiempo de compilación. Las declaraciones `var` tienen el mismo tipo que las variables declaradas explícitamente.

    var squaredNumber = 10 * 10;
    var squaredNumberDouble = 10.0 * 10.0;
    var builder = new StringBuilder();
    var anonymousObject = new
    { 
        One = SquaredNumber,
        Two = SquaredNumberDouble,
        Three = Builder
    }

Los tipos de las variables anteriores son `int`, `double`, `StringBuilder` y un tipo anónimo, respectivamente.

Es importante tener en cuenta que una variable `var` no se escribe dinámicamente. `SquaredNumber = Builder` no es válido ya que está intentando establecer un `int` en una instancia de `StringBuilder`

## Consultas integradas de lenguaje (LINQ)
    //Example 1
    int[] array = { 1, 5, 2, 10, 7 };

    // Select squares of all odd numbers in the array sorted in descending order
    IEnumerable<int> query = from x in array
                             where x % 2 == 1
                             orderby x descending
                             select x * x;
    // Result: 49, 25, 1
[Ejemplo del artículo de Wikipedia sobre C# 3.0, subsección LINQ][1]

El ejemplo 1 usa una sintaxis de consulta que fue diseñada para parecerse a las consultas SQL.

    //Example 2
    IEnumerable<int> query = array.Where(x => x % 2 == 1)
        .OrderByDescending(x => x)
        .Select(x => x * x);
    // Result: 49, 25, 1 using 'array' as defined in previous example
[Ejemplo del artículo de Wikipedia sobre C# 3.0, subsección LINQ][1]

El ejemplo 2 usa la sintaxis del método para lograr el mismo resultado que el ejemplo 1.

Es importante tener en cuenta que, en C#, la sintaxis de consulta LINQ es [azúcar sintáctico][2] para la sintaxis del método LINQ. El compilador traduce las consultas en llamadas de método en tiempo de compilación. Algunas consultas deben expresarse en la sintaxis del método. [De MSDN][3] - "Por ejemplo, debe usar una llamada de método para expresar una consulta que recupera la cantidad de elementos que coinciden con una condición específica".


[1]: https://en.wikipedia.org/wiki/C_Sharp_3.0#LINQ_.28language-integrated_query.29
[2]: https://en.wikipedia.org/wiki/Syntactic_sugar
[3]: https://msdn.microsoft.com/en-us/library/bb397947.aspx

## Expresiones lambda
Las expresiones Lambda son una extensión de [métodos anónimos][1] que permiten parámetros tipificados implícitamente y valores devueltos. Su sintaxis es menos detallada que los métodos anónimos y sigue un estilo de programación funcional.

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

El código anterior generará la suma de los cuadrados de los números del 1 al 10 en la consola.

La primera expresión lambda eleva al cuadrado los números de la lista. Dado que solo hay 1 parámetro, se pueden omitir los paréntesis. Puede incluir paréntesis si lo desea:

    .Select( (number) => number * number);

o escriba explícitamente el parámetro pero luego se requieren paréntesis:

    .Select( (int number) => number * number);

El cuerpo lambda es una expresión y tiene un retorno implícito. También puede usar un cuerpo de declaración si lo desea. Esto es útil para lambdas más complejas.

    .Select( number => { return number * number; } );

El método de selección devuelve un nuevo IEnumerable<int> con los valores calculados.

La segunda expresión lambda suma los números de la lista devueltos por el método de selección. Se requieren paréntesis ya que hay varios parámetros. Los tipos de los parámetros se escriben explícitamente pero esto no es necesario. El siguiente método es equivalente.

    .Aggregate( (first, second) => { return first + second; } );

Como es este:

    .Aggregate( (int first, int second) => first + second );

[1]: https://www.wikiod.com/es/docs/c%23/60/methods/9338/anonymous-method#t=201608051345408629175

## Tipos anónimos
Los tipos anónimos brindan una manera conveniente de encapsular un conjunto de propiedades de solo lectura en un solo objeto sin tener que definir explícitamente un tipo primero. El compilador genera el nombre del tipo y no está disponible en el nivel del código fuente. El compilador infiere el tipo de cada propiedad.

Puede crear tipos anónimos utilizando la palabra clave `nuevo` seguida de una llave _(`{`)_. Dentro de las llaves, puede definir propiedades como en el código a continuación.

    var v = new { Amount = 108, Message = "Hello" };

También es posible crear una matriz de tipos anónimos. Vea el código a continuación:

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

O utilícelo con consultas LINQ:

    var productQuery = from prod in products
                       select new { prod.Color, prod.Price };

