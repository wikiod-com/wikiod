---
title: "Matrizes"
slug: "matrizes"
draft: false
images: []
weight: 9673
type: docs
toc: true
---

## Sintaxe
- **Declarando uma matriz:**

    &lt;type>[] &lt;name>;

- **Declarando array bidimensional:**

    &lt;type>[,] &lt;name> = new &lt;type>[&lt;value>, &lt;value>];

- **Declarando uma matriz irregular:**

    &lt;type>[][] &lt;name> = new &lt;type>[&lt;value>][];

- **Declarando um subarray para um Jagged Array:**

    &lt;name>[&lt;value>]  = new &lt;type>[&lt;value>];

- **Inicializando um array sem valores:**

    &lt;name> = new &lt;type>[&lt;length>];

- **Inicializando uma matriz com valores:**

    &lt;name> = new &lt;type>[] {&lt;value>, &lt;value>, &lt;value>, ...};

- **Inicializando uma matriz bidimensional com valores:**

    &lt;name> = new &lt;type>[,] { {&lt;value>, &lt;value>}, {&lt;value>, &lt;value>}, ...};

- **Acessando um elemento no índice i:**

    &lt;name>[i]

- **Obtendo o comprimento do array:**

    &lt;name>.Length



Em C#, uma matriz é um tipo de referência, o que significa que é *anulável*.

Um array tem um comprimento fixo, o que significa que você não pode `.Add()` a ele ou `.Remove()` dele. Para usá-los, você precisaria de um array dinâmico - `List` ou `ArrayList`.

## Declarando um array
Um array pode ser declarado e preenchido com o valor padrão usando a sintaxe de inicialização de colchetes (`[]`). Por exemplo, criando uma matriz de 10 inteiros:

    int[] arr = new int[10];

Índices em C# são baseados em zero. Os índices do array acima serão 0-9. Por exemplo:

    int[] arr = new int[3] {7,9,4};
    Console.WriteLine(arr[0]); //outputs 7
    Console.WriteLine(arr[1]); //outputs 9

O que significa que o sistema começa a contar o índice do elemento a partir de 0. Além disso, os acessos aos elementos dos arrays são feitos em **tempo constante**. Isso significa que acessar o primeiro elemento do array tem o mesmo custo (em tempo) de acessar o segundo elemento, o terceiro elemento e assim por diante.

Você também pode declarar uma referência simples a um array sem instanciar um array.

    int[] arr = null;   // OK, declares a null reference to an array.
    int first = arr[0]; // Throws System.NullReferenceException because there is no actual array.

Um array também pode ser criado e inicializado com valores personalizados usando a sintaxe de inicialização de coleção:

    int[] arr = new int[] { 24, 2, 13, 47, 45 };

A parte `new int[]` pode ser omitida ao declarar uma variável de array. Esta não é uma _expression_ autocontida, então usá-la como parte de uma chamada diferente não funciona (para isso, use a versão com `new`):

    int[] arr = { 24, 2, 13, 47, 45 };  // OK
    int[] arr1;
    arr1 = { 24, 2, 13, 47, 45 };       // Won't compile

**Matrizes tipadas implicitamente**

Alternativamente, em combinação com a palavra-chave `var`, o tipo específico pode ser omitido para que o tipo do array seja inferido:

    // same as int[]
    var arr = new [] { 1, 2, 3 };
    // same as string[]
    var arr = new [] { "one", "two", "three" };
    // same as double[]
    var arr = new [] { 1.0, 2.0, 3.0 };


## Inicializando um array preenchido com um valor não padrão repetido
Como sabemos, podemos declarar um array com valores padrão:

    int[] arr = new int[10];

Isso criará um array de 10 inteiros com cada elemento do array tendo valor `0` (o valor padrão do tipo `int`).

Para criar um array inicializado com um valor não padrão, podemos usar [`Enumerable.Repeat`][1] do namespace [`System.Linq`][2]:

1. Para criar um array `bool` de tamanho 10 preenchido com **"true"**

        bool[] booleanArray = Enumerable.Repeat(true, 10).ToArray(); 

2. Para criar um array `int` de tamanho 5 preenchido com **"100"**

        int[] intArray = Enumerable.Repeat(100, 5).ToArray();

3. Para criar um array `string` de tamanho 5 preenchido com **"C#"**

        string[] strArray = Enumerable.Repeat("C#", 5).ToArray();

[1]: https://msdn.microsoft.com/en-us/library/bb348899(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.linq%28v=vs.100%29.aspx

## Copiando matrizes
Copiando um array parcial com o método estático `Array.Copy()`, começando no índice 0 em ambos, origem e destino:

    var sourceArray = new int[] { 11, 12, 3, 5, 2, 9, 28, 17 };
    var destinationArray= new int[3];
    Array.Copy(sourceArray, destinationArray, 3);

    // destinationArray will have 11,12 and 3

Copiando o array inteiro com o método de instância `CopyTo()`, começando no índice 0 da origem e o índice especificado no destino:

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = new int[6];
    sourceArray.CopyTo(destinationArray, 2);

    // destinationArray will have 0, 0, 11, 12, 7 and 0

`Clone` é usado para criar uma cópia de um objeto array.

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = (int)sourceArray.Clone();

    //destinationArray will be created and will have 11,12,17.

Ambos `CopyTo` e `Clone` executam uma cópia superficial, o que significa que o conteúdo contém referências ao mesmo objeto que os elementos do array original.

## Comparando arrays para igualdade
O LINQ fornece uma função interna para verificar a igualdade de dois `IEnumerable`s, e essa função pode ser usada em matrizes.

A função [`SequenceEqual`][1] retornará `true` se os arrays tiverem o mesmo comprimento e os valores nos índices correspondentes forem iguais, e `false` caso contrário.

    int[] arr1 = { 3, 5, 7 };
    int[] arr2 = { 3, 5, 7 };
    bool result = arr1.SequenceEqual(arr2);
    Console.WriteLine("Arrays equal? {0}", result);

Isso imprimirá:

<!-- idioma: lang-none -->
    Arrays equal? True

[1]: https://msdn.microsoft.com/en-us/library/bb348567(v=vs.110).aspx

## Arrays multidimensionais
Arrays podem ter mais de uma dimensão. O exemplo a seguir cria uma matriz bidimensional de dez linhas e dez colunas:

    int[,] arr = new int[10, 10];

Uma matriz de três dimensões:

    int[,,] arr = new int[10, 10, 10];

Você também pode inicializar o array na declaração:

    int[,] arr = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };

    // Access a member of the multi-dimensional array:
    Console.Out.WriteLine(arr[3, 1]);  // 4

 

## Obtendo e configurando valores de array
    int[] arr = new int[] { 0, 10, 20, 30}; 

    // Get 
    Console.WriteLine(arr[2]); // 20

    // Set 
    arr[2] = 100;

    // Get the updated value
    Console.WriteLine(arr[2]); // 100


## Iterar sobre um array
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

usando acesso inseguro com ponteiros
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

Resultado:

> 1
> 6
> 3
> 3
> 9


## Matrizes irregulares
Arrays irregulares são arrays que, em vez de tipos primitivos, contêm arrays (ou outras coleções). É como uma matriz de matrizes - cada elemento da matriz contém outra matriz.<br/><br/>
Eles são semelhantes aos arrays multidimensionais, mas têm uma pequena diferença - como os arrays multidimensionais são limitados a um número fixo de linhas e colunas, com arrays irregulares, cada linha pode ter um número diferente de colunas.

**Declarando uma matriz irregular**

Por exemplo, declarando uma matriz irregular com 8 colunas:

    int[][] a = new int[8][];
O segundo `[]` é inicializado sem um número. Para inicializar as submatrizes, você precisaria fazer isso separadamente:

    for (int i = 0; i < a.length; i++) 
    {
        a[i] = new int[10];
    }

**Como obter/definir valores**

Agora, obter um dos subarrays é fácil. Vamos imprimir todos os números da 3ª coluna de `a`:

    for (int i = 0; i < a[2].length; i++)
    {
        Console.WriteLine(a[2][i]);
    }
Obtendo um valor específico:

    a[<row_number>][<column_number>]
Configurando um valor específico:

    a[<row_number>][<column_number>] = <value>

**Lembre-se**: é sempre recomendável usar arrays irregulares (arrays de arrays) em vez de arrays multidimensionais (matrizes). É mais rápido e seguro de usar.

----------

**Observação sobre a ordem dos colchetes**

Considere um array tridimensional de arrays de cinco dimensões de arrays unidimensionais de `int`. Isso é escrito em C# como:

    int[,,][,,,,][] arr = new int[8, 10, 12][,,,,][];

No sistema de tipo CLR, a convenção para a ordenação dos colchetes é invertida, então com a instância `arr` acima temos:

        arr.GetType().ToString() == "System.Int32[][,,,,][,,]"

e da mesma forma:

        typeof(int[,,][,,,,][]).ToString() == "System.Int32[][,,,,][,,]"

## Criando um array de números sequenciais
O LINQ fornece um método que facilita a criação de uma coleção preenchida com números sequenciais. Por exemplo, você pode declarar uma matriz que contém os inteiros entre 1 e 100.

O método [`Enumerable.Range`][1] nos permite criar uma sequência de números inteiros a partir de uma posição inicial especificada e um número de elementos.

O método recebe dois argumentos: o valor inicial e o número de elementos a serem gerados.

    Enumerable.Range(int start, int count)

_Observe que `count` não pode ser negativo._

## Uso:

    int[] sequence = Enumerable.Range(1, 100).ToArray();

Isso gerará uma matriz contendo os números de 1 a 100 (`[1, 2, 3, ..., 98, 99, 100]`).

Como o método `Range` retorna um `IEnumerable<int>`, podemos usar outros métodos LINQ nele:

    int[] squares = Enumerable.Range(2, 10).Select(x => x * x).ToArray();

Isso gerará uma matriz que contém 10 quadrados inteiros começando em `4`: `[4, 9, 16, ..., 100, 121]`.


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable.range(v=vs.110).aspx

## Covariância de matriz
    string[] strings = new[] {"foo", "bar"};
    object[] objects = strings; // implicit conversion from string[] to object[]

Esta conversão não é de tipo seguro. O código a seguir irá gerar uma exceção de tempo de execução:

    string[] strings = new[] {"Foo"};
    object[] objects = strings;

    objects[0] = new object(); // runtime exception, object is not string
    string str = strings[0];   // would have been bad if above assignment had succeeded

## Verificando se um array contém outro array


## Arrays como instâncias IEnumerable<>
Todos os arrays implementam a interface `IList` não genérica (e, portanto, as interfaces base `ICollection` e `IEnumerable` não genéricas).

Mais importante, arrays unidimensionais implementam as interfaces genéricas `IList<>` e `IReadOnlyList<>` (e suas interfaces base) para o tipo de dados que eles contêm. Isso significa que eles podem ser tratados como tipos enumeráveis ​​genéricos e passados ​​para uma variedade de métodos sem a necessidade de convertê-los primeiro em um formato não array.

    int[] arr1 = { 3, 5, 7 };
    IEnumerable<int> enumerableIntegers = arr1; //Allowed because arrays implement IEnumerable<T>
    List<int> listOfIntegers = new List<int>();
    listOfIntegers.AddRange(arr1); //You can pass in a reference to an array to populate a List.

Após executar este código, a lista `listOfIntegers` conterá uma `List<int>` contendo os valores 3, 5 e 7.

O suporte `IEnumerable<>` significa que arrays podem ser consultados com LINQ, por exemplo `arr1.Select(i => 10 * i)`.

