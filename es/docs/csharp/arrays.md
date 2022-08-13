---
title: "arreglos"
slug: "arreglos"
draft: false
images: []
weight: 9673
type: docs
toc: true
---

## Sintaxis
- **Declarando una matriz:**

    &lt;type>[] &lt;name>;

- **Declarando array bidimensional:**

    &lt;type>[,] &lt;name> = new &lt;type>[&lt;value>, &lt;value>];

- **Declaración de una matriz irregular:**

    &lt;type>[][] &lt;name> = new &lt;type>[&lt;value>][];

- **Declarar un subarreglo para un Array Jagged:**

    &lt;name>[&lt;value>]  = new &lt;type>[&lt;value>];

- **Inicializar una matriz sin valores:**

    &lt;name> = new &lt;type>[&lt;length>];

- **Inicializar una matriz con valores:**

    &lt;name> = new &lt;type>[] {&lt;value>, &lt;value>, &lt;value>, ...};

- **Inicializar una matriz bidimensional con valores:**

    &lt;name> = new &lt;type>[,] { {&lt;value>, &lt;value>}, {&lt;value>, &lt;value>}, ...};

- **Accediendo a un elemento en el índice i:**

    &lt;name>[i]

- **Obteniendo la longitud de la matriz:**

    &lt;name>.Length



En C#, una matriz es un tipo de referencia, lo que significa que es *anulable*.

Una matriz tiene una longitud fija, lo que significa que no puede `.Add()` ni `.Remove()`. Para usarlos, necesitaría una matriz dinámica: `List` o `ArrayList`.

## Declarar una matriz
Una matriz se puede declarar y rellenar con el valor predeterminado utilizando la sintaxis de inicialización de corchetes (`[]`). Por ejemplo, creando una matriz de 10 enteros:

    int[] arr = new int[10];

Los índices en C# están basados ​​en cero. Los índices de la matriz anterior serán 0-9. Por ejemplo:

    int[] arr = new int[3] {7,9,4};
    Console.WriteLine(arr[0]); //outputs 7
    Console.WriteLine(arr[1]); //outputs 9

Lo que significa que el sistema comienza a contar el índice del elemento desde 0. Además, los accesos a los elementos de los arreglos se realizan en **tiempo constante**. Eso significa que acceder al primer elemento de la matriz tiene el mismo costo (en tiempo) que acceder al segundo elemento, al tercer elemento y así sucesivamente.

También puede declarar una referencia simple a una matriz sin instanciar una matriz.

    int[] arr = null;   // OK, declares a null reference to an array.
    int first = arr[0]; // Throws System.NullReferenceException because there is no actual array.

También se puede crear e inicializar una matriz con valores personalizados utilizando la sintaxis de inicialización de la colección:

    int[] arr = new int[] { 24, 2, 13, 47, 45 };

La porción `new int[]` se puede omitir al declarar una variable de matriz. Esta no es una _expresión_ independiente, por lo que usarla como parte de una llamada diferente no funciona (para eso, use la versión con `nuevo`):

    int[] arr = { 24, 2, 13, 47, 45 };  // OK
    int[] arr1;
    arr1 = { 24, 2, 13, 47, 45 };       // Won't compile

**Arreglos tipificados implícitamente**

Alternativamente, en combinación con la palabra clave `var`, se puede omitir el tipo específico para que se infiera el tipo de la matriz:

    // same as int[]
    var arr = new [] { 1, 2, 3 };
    // same as string[]
    var arr = new [] { "one", "two", "three" };
    // same as double[]
    var arr = new [] { 1.0, 2.0, 3.0 };


## Inicializar una matriz llena con un valor no predeterminado repetido
Como sabemos, podemos declarar una matriz con valores predeterminados:

    int[] arr = new int[10];

Esto creará una matriz de 10 enteros con cada elemento de la matriz con valor `0` (el valor predeterminado de tipo `int`).

Para crear una matriz inicializada con un valor no predeterminado, podemos usar [`Enumerable.Repeat`][1] del espacio de nombres [`System.Linq`][2]:

1. Para crear una matriz `bool` de tamaño 10 llena de **"verdadero"**

        bool[] booleanArray = Enumerable.Repeat(true, 10).ToArray(); 

2. Para crear una matriz `int` de tamaño 5 rellena con **"100"**

        int[] intArray = Enumerable.Repeat(100, 5).ToArray();

3. Para crear una matriz `string` de tamaño 5 rellena con **"C#"**

        string[] strArray = Enumerable.Repeat("C#", 5).ToArray();

[1]: https://msdn.microsoft.com/en-us/library/bb348899(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.linq%28v=vs.100%29.aspx

## Copiando arreglos
Copiar una matriz parcial con el método estático `Array.Copy()`, comenzando en el índice 0 tanto en el origen como en el destino:

    var sourceArray = new int[] { 11, 12, 3, 5, 2, 9, 28, 17 };
    var destinationArray= new int[3];
    Array.Copy(sourceArray, destinationArray, 3);

    // destinationArray will have 11,12 and 3

Copiando toda la matriz con el método de instancia `CopyTo()`, comenzando en el índice 0 de la fuente y el índice especificado en el destino:

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = new int[6];
    sourceArray.CopyTo(destinationArray, 2);

    // destinationArray will have 0, 0, 11, 12, 7 and 0

`Clone` se usa para crear una copia de un objeto de matriz.

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = (int)sourceArray.Clone();

    //destinationArray will be created and will have 11,12,17.

Tanto `CopyTo` como `Clone` realizan una copia superficial, lo que significa que el contenido contiene referencias al mismo objeto que los elementos de la matriz original.

## Comparando arreglos para la igualdad
LINQ proporciona una función integrada para verificar la igualdad de dos `IEnumerable`s, y esa función se puede usar en matrices.

La función [`SequenceEqual`][1] devolverá `true` si las matrices tienen la misma longitud y los valores en los índices correspondientes son iguales, y `false` en caso contrario.

    int[] arr1 = { 3, 5, 7 };
    int[] arr2 = { 3, 5, 7 };
    bool result = arr1.SequenceEqual(arr2);
    Console.WriteLine("Arrays equal? {0}", result);

Esto imprimirá:

<!-- idioma: lang-ninguno -->
    Arrays equal? True

[1]: https://msdn.microsoft.com/en-us/library/bb348567(v=vs.110).aspx

## Matrices multidimensionales
Los arreglos pueden tener más de una dimensión. El siguiente ejemplo crea una matriz bidimensional de diez filas y diez columnas:

    int[,] arr = new int[10, 10];

Una matriz de tres dimensiones:

    int[,,] arr = new int[10, 10, 10];

También puede inicializar la matriz en la declaración:

    int[,] arr = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };

    // Access a member of the multi-dimensional array:
    Console.Out.WriteLine(arr[3, 1]);  // 4

 

## Obtener y establecer valores de matriz
    int[] arr = new int[] { 0, 10, 20, 30}; 

    // Get 
    Console.WriteLine(arr[2]); // 20

    // Set 
    arr[2] = 100;

    // Get the updated value
    Console.WriteLine(arr[2]); // 100


## Iterar sobre una matriz
    int[] arr = new int[] {1, 6, 3, 3, 9};

    for (int i = 0; i < arr.Length; i++) 
    {
        Console.WriteLine(arr[i]);
    }

usando foreach:

    foreach (int element in arr) 
    {
        Console.WriteLine(element);
    }

usar acceso inseguro con punteros
https://msdn.microsoft.com/en-ca/library/y31yhkeb.aspx
 

    unsafe
    {
        int length = arr.Length;
        fixed (int* p = arr)
        {
            int* pInt = p;
            while (length-- > 0)
            {
                Console.WriteLine(*pInt);
                pInt++;// move pointer to next element
            }
        }
    }

Producción:

> 1
> 6
> 3
> 3
> 9


## Matrices irregulares
Las matrices irregulares son matrices que, en lugar de tipos primitivos, contienen matrices (u otras colecciones). Es como una matriz de matrices: cada elemento de la matriz contiene otra matriz.<br/><br/>
Son similares a los arreglos multidimensionales, pero tienen una ligera diferencia: dado que los arreglos multidimensionales están limitados a un número fijo de filas y columnas, con los arreglos irregulares, cada fila puede tener un número diferente de columnas.

**Declarando una matriz irregular**

Por ejemplo, declarar una matriz irregular con 8 columnas:

    int[][] a = new int[8][];
El segundo `[]` se inicializa sin un número. Para inicializar los subconjuntos, deberá hacerlo por separado:

    for (int i = 0; i < a.length; i++) 
    {
        a[i] = new int[10];
    }

**Obtener/Establecer valores**

Ahora, obtener uno de los subarreglos es fácil. Imprimamos todos los números de la 3ra columna de `a`:

    for (int i = 0; i < a[2].length; i++)
    {
        Console.WriteLine(a[2][i]);
    }
Obtener un valor específico:

    a[<row_number>][<column_number>]
Establecer un valor específico:

    a[<row_number>][<column_number>] = <value>

**Recuerde**: siempre se recomienda utilizar arreglos irregulares (arreglos de arreglos) en lugar de arreglos multidimensionales (matrices). Es más rápido y seguro de usar.

----------

**Nota sobre el orden de los corchetes**

Considere una matriz tridimensional de matrices de cinco dimensiones de matrices unidimensionales de `int`. Esto está escrito en C# como:

    int[,,][,,,,][] arr = new int[8, 10, 12][,,,,][];

En el sistema de tipo CLR, la convención para el orden de los corchetes se invierte, por lo que con la instancia `arr` anterior tenemos:

        arr.GetType().ToString() == "System.Int32[][,,,,][,,]"

y de la misma manera:

        typeof(int[,,][,,,,][]).ToString() == "System.Int32[][,,,,][,,]"

## Creando una matriz de números secuenciales
LINQ proporciona un método que facilita la creación de una colección llena de números secuenciales. Por ejemplo, puede declarar una matriz que contenga los números enteros entre 1 y 100.

El método [`Enumerable.Range`][1] nos permite crear una secuencia de números enteros a partir de una posición de inicio específica y una cantidad de elementos.

El método toma dos argumentos: el valor inicial y el número de elementos a generar.

    Enumerable.Range(int start, int count)

_Tenga en cuenta que `count` no puede ser negativo._

## Uso:

    int[] sequence = Enumerable.Range(1, 100).ToArray();

Esto generará una matriz que contiene los números del 1 al 100 (`[1, 2, 3, ..., 98, 99, 100]`).

Debido a que el método `Range` devuelve un `IEnumerable<int>`, podemos usar otros métodos LINQ en él:

    int[] squares = Enumerable.Range(2, 10).Select(x => x * x).ToArray();

Esto generará una matriz que contiene 10 cuadrados enteros que comienzan en `4`: `[4, 9, 16, ..., 100, 121]`.


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable.range(v=vs.110).aspx

## Arreglos de covarianza
    string[] strings = new[] {"foo", "bar"};
    object[] objects = strings; // implicit conversion from string[] to object[]

Esta conversión no es de tipo seguro. El siguiente código generará una excepción de tiempo de ejecución:

    string[] strings = new[] {"Foo"};
    object[] objects = strings;

    objects[0] = new object(); // runtime exception, object is not string
    string str = strings[0];   // would have been bad if above assignment had succeeded

## Comprobando si una matriz contiene otra matriz


## Matrices como instancias de IEnumerable<>
Todas las matrices implementan la interfaz 'IList' no genérica (y, por lo tanto, las interfaces base 'ICollection' e 'IEnumerable' no genéricas).

Más importante aún, las matrices unidimensionales implementan las interfaces genéricas `IList<>` e `IReadOnlyList<>` (y sus interfaces base) para el tipo de datos que contienen. Esto significa que pueden tratarse como tipos enumerables genéricos y pasarse a una variedad de métodos sin necesidad de convertirlos primero a una forma que no sea de matriz.

    int[] arr1 = { 3, 5, 7 };
    IEnumerable<int> enumerableIntegers = arr1; //Allowed because arrays implement IEnumerable<T>
    List<int> listOfIntegers = new List<int>();
    listOfIntegers.AddRange(arr1); //You can pass in a reference to an array to populate a List.

Después de ejecutar este código, la lista `listOfIntegers` contendrá una `List<int>` que contiene los valores 3, 5 y 7.

El soporte `IEnumerable<>` significa que las matrices se pueden consultar con LINQ, por ejemplo `arr1.Select(i => 10 * i)`.

