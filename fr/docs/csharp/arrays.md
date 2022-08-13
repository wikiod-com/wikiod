---
title: "Tableaux"
slug: "tableaux"
draft: false
images: []
weight: 9673
type: docs
toc: true
---

## Syntaxe
- **Déclaration d'un tableau :**

    &lt;type>[] &lt;name>;

- **Déclarer un tableau à deux dimensions :**

    &lt;type>[,] &lt;name> = new &lt;type>[&lt;value>, &lt;value>];

- **Déclaration d'un tableau en escalier :**

    &lt;type>[][] &lt;name> = new &lt;type>[&lt;value>][];

- **Déclaration d'un sous-tableau pour un Jagged Array :**

    &lt;name>[&lt;value>]  = new &lt;type>[&lt;value>];

- **Initialisation d'un tableau sans valeurs :**

    &lt;name> = new &lt;type>[&lt;length>];

- **Initialisation d'un tableau avec des valeurs :**

    &lt;name> = new &lt;type>[] {&lt;value>, &lt;value>, &lt;value>, ...};

- **Initialisation d'un tableau à deux dimensions avec des valeurs :**

    &lt;name> = new &lt;type>[,] { {&lt;value>, &lt;value>}, {&lt;value>, &lt;value>}, ...};

- **Accès à un élément à l'index i :**

    &lt;name>[i]

- **Obtenir la longueur du tableau :**

    &lt;name>.Length



En C#, un tableau est un type référence, ce qui signifie qu'il est *nullable*.

Un tableau a une longueur fixe, ce qui signifie que vous ne pouvez pas lui ajouter `.Add()` ou `.Remove()`. Pour les utiliser, vous auriez besoin d'un tableau dynamique - `List` ou `ArrayList`.

## Déclarer un tableau
Un tableau peut être déclaré et rempli avec la valeur par défaut en utilisant la syntaxe d'initialisation entre crochets (`[]`). Par exemple, créer un tableau de 10 entiers :

    int[] arr = new int[10];

Les index en C# sont basés sur zéro. Les indices du tableau ci-dessus seront 0-9. Par exemple:

    int[] arr = new int[3] {7,9,4};
    Console.WriteLine(arr[0]); //outputs 7
    Console.WriteLine(arr[1]); //outputs 9

Ce qui signifie que le système commence à compter l'index des éléments à partir de 0. De plus, les accès aux éléments des tableaux se font en **temps constant**. Cela signifie que l'accès au premier élément du tableau a le même coût (en temps) que l'accès au deuxième élément, au troisième élément, etc.

Vous pouvez également déclarer une simple référence à un tableau sans instancier un tableau.

    int[] arr = null;   // OK, declares a null reference to an array.
    int first = arr[0]; // Throws System.NullReferenceException because there is no actual array.

Un tableau peut également être créé et initialisé avec des valeurs personnalisées à l'aide de la syntaxe d'initialisation de collection :

    int[] arr = new int[] { 24, 2, 13, 47, 45 };

La partie `new int[]` peut être omise lors de la déclaration d'une variable tableau. Ce n'est pas une _expression_ autonome, donc l'utiliser dans le cadre d'un appel différent ne fonctionne pas (pour cela, utilisez la version avec `new`):

    int[] arr = { 24, 2, 13, 47, 45 };  // OK
    int[] arr1;
    arr1 = { 24, 2, 13, 47, 45 };       // Won't compile

** Tableaux implicitement typés **

Alternativement, en combinaison avec le mot-clé `var`, le type spécifique peut être omis afin que le type du tableau soit déduit :

    // same as int[]
    var arr = new [] { 1, 2, 3 };
    // same as string[]
    var arr = new [] { "one", "two", "three" };
    // same as double[]
    var arr = new [] { 1.0, 2.0, 3.0 };


## Initialisation d'un tableau rempli d'une valeur répétée autre que la valeur par défaut
Comme nous le savons, nous pouvons déclarer un tableau avec des valeurs par défaut :

    int[] arr = new int[10];

Cela créera un tableau de 10 entiers avec chaque élément du tableau ayant la valeur '0' (la valeur par défaut de type 'int').

Pour créer un tableau initialisé avec une valeur autre que celle par défaut, nous pouvons utiliser [`Enumerable.Repeat`][1] de l'espace de noms [`System.Linq`][2] :

1. Pour créer un tableau `bool` de taille 10 rempli de **"true"**

        bool[] booleanArray = Enumerable.Repeat(true, 10).ToArray(); 

2. Pour créer un tableau `int` de taille 5 rempli de **"100"**

        int[] intArray = Enumerable.Repeat(100, 5).ToArray();

3. Pour créer un tableau `string` de taille 5 rempli de **"C#"**

        string[] strArray = Enumerable.Repeat("C#", 5).ToArray();

[1] : https://msdn.microsoft.com/en-us/library/bb348899(v=vs.100).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.linq%28v=vs.100%29.aspx

## Copie de tableaux
Copie d'un tableau partiel avec la méthode statique `Array.Copy()`, en commençant à l'index 0 dans la source et la destination :

    var sourceArray = new int[] { 11, 12, 3, 5, 2, 9, 28, 17 };
    var destinationArray= new int[3];
    Array.Copy(sourceArray, destinationArray, 3);

    // destinationArray will have 11,12 and 3

Copier tout le tableau avec la méthode d'instance `CopyTo()`, en commençant à l'index 0 de la source et à l'index spécifié dans la destination :

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = new int[6];
    sourceArray.CopyTo(destinationArray, 2);

    // destinationArray will have 0, 0, 11, 12, 7 and 0

`Clone` est utilisé pour créer une copie d'un objet tableau.

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = (int)sourceArray.Clone();

    //destinationArray will be created and will have 11,12,17.

`CopyTo` et `Clone` effectuent tous deux une copie superficielle, ce qui signifie que le contenu contient des références au même objet que les éléments du tableau d'origine.

## Comparaison de tableaux pour l'égalité
LINQ fournit une fonction intégrée pour vérifier l'égalité de deux `IEnumerable`, et cette fonction peut être utilisée sur des tableaux.

La fonction [`SequenceEqual`][1] renverra `true` si les tableaux ont la même longueur et que les valeurs des indices correspondants sont égales, et `false` sinon.

    int[] arr1 = { 3, 5, 7 };
    int[] arr2 = { 3, 5, 7 };
    bool result = arr1.SequenceEqual(arr2);
    Console.WriteLine("Arrays equal? {0}", result);

Cela imprimera :

<!-- langue : lang-none -->
    Arrays equal? True

[1] : https://msdn.microsoft.com/en-us/library/bb348567(v=vs.110).aspx

## Tableaux multidimensionnels
Les tableaux peuvent avoir plusieurs dimensions. L'exemple suivant crée un tableau à deux dimensions de dix lignes et dix colonnes :

    int[,] arr = new int[10, 10];

Un tableau à trois dimensions :

    int[,,] arr = new int[10, 10, 10];

Vous pouvez également initialiser le tableau lors de la déclaration :

    int[,] arr = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };

    // Access a member of the multi-dimensional array:
    Console.Out.WriteLine(arr[3, 1]);  // 4

 

## Obtenir et définir des valeurs de tableau
    int[] arr = new int[] { 0, 10, 20, 30}; 

    // Get 
    Console.WriteLine(arr[2]); // 20

    // Set 
    arr[2] = 100;

    // Get the updated value
    Console.WriteLine(arr[2]); // 100


## Itérer sur un tableau
    int[] arr = new int[] {1, 6, 3, 3, 9};

    for (int i = 0; i < arr.Length; i++) 
    {
        Console.WriteLine(arr[i]);
    }

en utilisant foreach :

    foreach (int element in arr) 
    {
        Console.WriteLine(element);
    }

utilisation d'un accès non sécurisé avec des pointeurs
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

Production:

> 1
> 6
> 3
> 3
> 9


## Tableaux en escalier
Les tableaux en escalier sont des tableaux qui, au lieu de types primitifs, contiennent des tableaux (ou d'autres collections). C'est comme un tableau de tableaux - chaque élément du tableau contient un autre tableau.<br/><br/>
Ils sont similaires aux tableaux multidimensionnels, mais ont une légère différence - comme les tableaux multidimensionnels sont limités à un nombre fixe de lignes et de colonnes, avec des tableaux irréguliers, chaque ligne peut avoir un nombre différent de colonnes.

**Déclaration d'un tableau irrégulier**

Par exemple, déclarer un tableau en escalier avec 8 colonnes :

    int[][] a = new int[8][];
Le deuxième `[]` est initialisé sans numéro. Pour initialiser les sous-tableaux, vous devez le faire séparément :

    for (int i = 0; i < a.length; i++) 
    {
        a[i] = new int[10];
    }

**Obtenir/Régler les valeurs**

Maintenant, obtenir l'un des sous-réseaux est facile. Écrivons tous les nombres de la 3e colonne de `a` :

    for (int i = 0; i < a[2].length; i++)
    {
        Console.WriteLine(a[2][i]);
    }
Obtenir une valeur spécifique :

    a[<row_number>][<column_number>]
Définition d'une valeur spécifique :

    a[<row_number>][<column_number>] = <value>

**Rappel** : Il est toujours recommandé d'utiliser des tableaux en escalier (tableaux de tableaux) plutôt que des tableaux multidimensionnels (matrices). C'est plus rapide et plus sûr à utiliser.

----------

**Remarque sur l'ordre des équerres**

Considérons un tableau à trois dimensions de tableaux à cinq dimensions de tableaux à une dimension de "int". Cela s'écrit en C# comme suit :

    int[,,][,,,,][] arr = new int[8, 10, 12][,,,,][];

Dans le système de type CLR, la convention pour l'ordre des crochets est inversée, donc avec l'instance `arr` ci-dessus, nous avons :

        arr.GetType().ToString() == "System.Int32[][,,,,][,,]"

et également:

        typeof(int[,,][,,,,][]).ToString() == "System.Int32[][,,,,][,,]"

## Création d'un tableau de nombres séquentiels
LINQ fournit une méthode qui facilite la création d'une collection remplie de numéros séquentiels. Par exemple, vous pouvez déclarer un tableau contenant les entiers compris entre 1 et 100.

La méthode [`Enumerable.Range`][1] nous permet de créer une séquence de nombres entiers à partir d'une position de départ spécifiée et d'un certain nombre d'éléments.

La méthode prend deux arguments : la valeur de départ et le nombre d'éléments à générer.

    Enumerable.Range(int start, int count)

_Notez que `count` ne peut pas être négatif._

## Utilisation :

    int[] sequence = Enumerable.Range(1, 100).ToArray();

Cela générera un tableau contenant les nombres de 1 à 100 (`[1, 2, 3, ..., 98, 99, 100]`).

Étant donné que la méthode `Range` renvoie un `IEnumerable<int>`, nous pouvons utiliser d'autres méthodes LINQ dessus :

    int[] squares = Enumerable.Range(2, 10).Select(x => x * x).ToArray();

Cela générera un tableau contenant 10 carrés entiers commençant à `4` : `[4, 9, 16, ..., 100, 121]`.


[1] : https://msdn.microsoft.com/en-us/library/system.linq.enumerable.range(v=vs.110).aspx

## Covariance du tableau
    string[] strings = new[] {"foo", "bar"};
    object[] objects = strings; // implicit conversion from string[] to object[]

Cette conversion n'est pas de type sécurisé. Le code suivant déclenchera une exception d'exécution :

    string[] strings = new[] {"Foo"};
    object[] objects = strings;

    objects[0] = new object(); // runtime exception, object is not string
    string str = strings[0];   // would have been bad if above assignment had succeeded

## Vérifier si un tableau contient un autre tableau


## Tableaux en tant qu'instances IEnumerable<>
Tous les tableaux implémentent l'interface non générique `IList` (et donc les interfaces de base non génériques `ICollection` et `IEnumerable`).

Plus important encore, les tableaux unidimensionnels implémentent les interfaces génériques `IList<>` et `IReadOnlyList<>` (et leurs interfaces de base) pour le type de données qu'ils contiennent. Cela signifie qu'ils peuvent être traités comme des types énumérables génériques et transmis à diverses méthodes sans qu'il soit nécessaire de les convertir au préalable en une forme non tableau.

    int[] arr1 = { 3, 5, 7 };
    IEnumerable<int> enumerableIntegers = arr1; //Allowed because arrays implement IEnumerable<T>
    List<int> listOfIntegers = new List<int>();
    listOfIntegers.AddRange(arr1); //You can pass in a reference to an array to populate a List.

Après avoir exécuté ce code, la liste `listOfIntegers` contiendra une `List<int>` contenant les valeurs 3, 5 et 7.

La prise en charge de `IEnumerable<>` signifie que les tableaux peuvent être interrogés avec LINQ, par exemple `arr1.Select(i => 10 * i)`.

