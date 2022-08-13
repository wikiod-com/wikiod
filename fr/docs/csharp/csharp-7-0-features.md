---
title: "Fonctionnalités C# 7.0"
slug: "fonctionnalites-c-70"
draft: false
images: []
weight: 1717
type: docs
toc: true
---

C# 7.0 est la septième version de C#. Cette version contient de nouvelles fonctionnalités : prise en charge de la langue pour Tuples, fonctions locales, déclarations "out var", séparateurs de chiffres, littéraux binaires, correspondance de modèles, expressions de lancement, "ref return" et "ref local" et liste étendue des membres du corps d'expression.

Référence officielle : [Quoi de neuf dans C# 7](https://docs.microsoft.com/en-us/dotnet/articles/csharp/csharp-7)

## Prise en charge de la langue pour les tuples
# Bases

Un **tuple** est une liste ordonnée et finie d'éléments. Les tuples sont couramment utilisés en programmation comme moyen de travailler avec une seule entité collectivement au lieu de travailler individuellement avec chacun des éléments du tuple, et de représenter des lignes individuelles (c'est-à-dire des "enregistrements") dans une base de données relationnelle.

En C# 7.0, les méthodes peuvent avoir plusieurs valeurs de retour. Dans les coulisses, le compilateur utilisera la nouvelle structure [ValueTuple][1].

    public (int sum, int count) GetTallies() 
    {
        return (1, 2);
    }

_Note latérale_ : pour que cela fonctionne dans Visual Studio 2017, vous devez obtenir le package ```System.ValueTuple```.

Si le résultat d'une méthode de retour de tuple est affecté à une seule variable, vous pouvez accéder aux membres par leurs noms définis sur la signature de la méthode :

    var result = GetTallies();
    // > result.sum
    // 1
    // > result.count
    // 2

# Déconstruction de tuples

La déconstruction de tuple sépare un tuple en ses parties.

Par exemple, invoquer `GetTallies` et attribuer la valeur de retour à deux variables distinctes déconstruit le tuple en ces deux variables :

    (int tallyOne, int tallyTwo) = GetTallies();

`var` fonctionne également :

    (var s, var c) = GetTallies();

Vous pouvez également utiliser une syntaxe plus courte, avec `var` en dehors de `()` :

    var (s, c) = GetTallies();

Vous pouvez également déconstruire en variables existantes :

    int s, c;
    (s, c) = GetTallies();

L'échange est maintenant beaucoup plus simple (aucune variable temporaire nécessaire) :

    (b, a) = (a, b);

Fait intéressant, n'importe quel objet peut être déconstruit en définissant une méthode `Deconstruct` dans la classe :

    class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }

        public void Deconstruct(out string firstName, out string lastName)
        {
            firstName = FirstName;
            lastName = LastName;
        }
    }

    var person = new Person { FirstName = "John", LastName = "Smith" };
    var (localFirstName, localLastName) = person;

Dans ce cas, la syntaxe `(localFirstName, localLastName) = person` appelle `Deconstruct` sur la `person`.

La déconstruction peut même être définie dans une méthode d'extension. Ceci est équivalent à ce qui précède :

    public static class PersonExtensions
    {
        public static void Deconstruct(this Person person, out string firstName, out string lastName)
        {
            firstName = person.FirstName;
            lastName = person.LastName;
        }
    }
    
    var (localFirstName, localLastName) = person;

Une approche alternative pour la classe `Person` consiste à définir le `Name` lui-même comme un `Tuple`. Considérer ce qui suit:

    class Person
    {
        public (string First, string Last) Name { get; }

        public Person((string FirstName, string LastName) name)
        {
            Name = name;
        }
    }

Ensuite, vous pouvez instancier une personne comme ceci (où nous pouvons prendre un tuple comme argument):

    var person = new Person(("Jane", "Smith"));

    var firstName = person.Name.First; // "Jane"
    var lastName = person.Name.Last;   // "Smith"

# Initialisation de tuple
Vous pouvez également créer arbitrairement des tuples dans le code :

    var name = ("John", "Smith");
    Console.WriteLine(name.Item1);
    // Outputs John

    Console.WriteLine(name.Item2);
    // Outputs Smith

# 

Lors de la création d'un tuple, vous pouvez attribuer des noms d'éléments ad hoc aux membres du tuple :

    var name = (first: "John", middle: "Q", last: "Smith");
    Console.WriteLine(name.first);
    // Outputs John

# Inférence de type

Plusieurs tuples définis avec la même signature (types et nombre correspondants) seront déduits comme des types correspondants. Par exemple:

    public (int sum, double average) Measure(List<int> items)
    {
        var stats = (sum: 0, average: 0d);
        stats.sum = items.Sum();
        stats.average = items.Average();
        return stats;
    }

`stats` peut être renvoyé puisque la déclaration de la variable `stats` et la signature de retour de la méthode correspondent.

# Noms de champ de réflexion et de tuple
Les noms de membre n'existent pas au moment de l'exécution. Reflection considérera les tuples avec le même nombre et les mêmes types de membres même si les noms de membres ne correspondent pas. La conversion d'un tuple en `object` puis en un tuple avec les mêmes types de membres, mais des noms différents, ne provoquera pas non plus d'exception.

Alors que la classe ValueTuple elle-même ne conserve pas les informations pour les noms de membres, les informations sont disponibles par réflexion dans un TupleElementNamesAttribute. Cet attribut n'est pas appliqué au tuple lui-même mais aux paramètres de méthode, aux valeurs de retour, aux propriétés et aux champs. Cela permet aux noms d'éléments de tuple d'être conservés dans les assemblages, c'est-à-dire que si une méthode renvoie (nom de chaîne, nombre int), le nom et le nombre de noms seront disponibles pour les appelants de la méthode dans un autre assemblage car la valeur de retour sera marquée avec TupleElementNameAttribute contenant les valeurs "nom" et "compte".

# Utiliser avec les génériques et `async`

Les nouvelles fonctionnalités de tuple (utilisant le type sous-jacent `ValueTuple`) prennent entièrement en charge les génériques et peuvent être utilisées comme paramètre de type générique. Cela permet de les utiliser avec le pattern `async`/`wait` :

    public async Task<(string value, int count)> GetValueAsync()
    {
        string fooBar = await _stackoverflow.GetStringAsync();
        int num = await _stackoverflow.GetIntAsync();

        return (fooBar, num);
    }

# Utiliser avec les collections

Il peut devenir avantageux d'avoir une collection de tuples dans (par exemple) un scénario où vous essayez de trouver un tuple correspondant en fonction de conditions pour éviter la ramification du code.

Exemple:

    private readonly List<Tuple<string, string, string>> labels = new List<Tuple<string, string, string>>()
    {
        new Tuple<string, string, string>("test1", "test2", "Value"),
        new Tuple<string, string, string>("test1", "test1", "Value2"),
        new Tuple<string, string, string>("test2", "test2", "Value3"),
    };

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.Item1 == firstElement && w.Item2 == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.Item3;
    }

Avec les nouveaux tuples peuvent devenir :

    private readonly List<(string firstThingy, string secondThingyLabel, string foundValue)> labels = new List<(string firstThingy, string secondThingyLabel, string foundValue)>()
    {
        ("test1", "test2", "Value"),
        ("test1", "test1", "Value2"),
        ("test2", "test2", "Value3"),
    }

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.firstThingy == firstElement && w.secondThingyLabel == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.foundValue;
    }

Bien que la dénomination sur l'exemple de tuple ci-dessus soit assez générique, l'idée d'étiquettes pertinentes permet une compréhension plus approfondie de ce qui est tenté dans le code en faisant référence à "item1", "item2" et "item3".

# Différences entre ValueTuple et Tuple

La principale raison de l'introduction de `ValueTuple` est la performance.

| Tapez le nom | `ValueTuple` | `Tuple` |
|---|---|---|
| Classe ou structure | `structure` | `classe` |
| Mutabilité (changement de valeurs après création) | modifiable | immuable |
| Nommer les membres et prendre en charge d'autres langues | oui | non ([à déterminer][2]) |

# Références

- [Proposition originale de fonctionnalité de langage Tuples sur GitHub][3]
- [Une solution VS 15 exécutable pour les fonctionnalités C# 7.0][4]
- [Paquet NuGet Tuple] [5]


[1] : https://github.com/dotnet/corefx/blob/master/src/System.ValueTuple/src/System/ValueTuple/ValueTuple.cs
[2] : https://github.com/dotnet/roslyn/issues/11031
[3] : https://github.com/dotnet/roslyn/issues/347
[4] : https://code.msdn.microsoft.com/Introduce-new-C-70-features-c639ed88
[5] : https://www.nuget.org/packages/System.ValueTuple/

## Fonctions locales
Les fonctions locales sont définies dans une méthode et ne sont pas disponibles en dehors de celle-ci. Ils ont accès à toutes les variables locales et prennent en charge les itérateurs, `async`/`wait` et la syntaxe lambda. De cette façon, les répétitions spécifiques à une fonction peuvent être fonctionnalisées sans encombrer la classe. En tant qu'effet secondaire, cela améliore les performances de suggestion intellisense.

# Exemple

    double GetCylinderVolume(double radius, double height)
    {
        return getVolume();
  
        double getVolume()
        {
            // You can declare inner-local functions in a local function 
            double GetCircleArea(double r) => Math.PI * r * r;

            // ALL parents' variables are accessible even though parent doesn't have any input. 
            return GetCircleArea(radius) * height;
        }
    }

Les fonctions locales simplifient considérablement le code pour les opérateurs LINQ, où vous devez généralement séparer les vérifications d'arguments de la logique réelle pour rendre les vérifications d'arguments instantanées, non retardées jusqu'au début de l'itération.

# Exemple

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source, 
        Func<TSource, bool> predicate)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (predicate == null) throw new ArgumentNullException(nameof(predicate));
    
        return iterator();

        IEnumerable<TSource> iterator()
        {
            foreach (TSource element in source)
                if (predicate(element))
                    yield return element;
        }
    }

Les fonctions locales prennent également en charge les mots-clés `async` et `wait`.

# Exemple

    async Task WriteEmailsAsync()
    {
        var emailRegex = new Regex(@"(?i)[a-z0-9_.+-]+@[a-z0-9-]+\.[a-z0-9-.]+");
        IEnumerable<string> emails1 = await getEmailsFromFileAsync("input1.txt");
        IEnumerable<string> emails2 = await getEmailsFromFileAsync("input2.txt");
        await writeLinesToFileAsync(emails1.Concat(emails2), "output.txt");

        async Task<IEnumerable<string>> getEmailsFromFileAsync(string fileName)
        {
            string text;

            using (StreamReader reader = File.OpenText(fileName))
            {
                text = await reader.ReadToEndAsync();
            }

            return from Match emailMatch in emailRegex.Matches(text) select emailMatch.Value;
        }

        async Task writeLinesToFileAsync(IEnumerable<string> lines, string fileName)
        {
            using (StreamWriter writer = File.CreateText(fileName))
            {
                foreach (string line in lines)
                {
                    await writer.WriteLineAsync(line);
                }
            }
        }
    }

Une chose importante que vous avez peut-être remarquée est que les fonctions locales peuvent être définies sous l'instruction `return`, elles n'ont **pas** besoin d'être définies au-dessus. De plus, les fonctions locales suivent généralement la convention de dénomination "lowerCamelCase" pour se différencier plus facilement des fonctions de portée de classe.

## out var déclaration
Un modèle courant en C# consiste à utiliser `bool TryParse(object input, out object value)` pour analyser les objets en toute sécurité.

La déclaration `out var` est une fonctionnalité simple pour améliorer la lisibilité. Il permet à une variable d'être déclarée en même temps qu'elle est passée en paramètre de sortie.

Une variable déclarée de cette manière est limitée au reste du corps à l'endroit où elle est déclarée.

# Exemple

En utilisant `TryParse` avant C# 7.0, vous devez déclarer une variable pour recevoir la valeur avant d'appeler la fonction :

<!-- si version [lt 7.0] -->
    int value;
    if (int.TryParse(input, out value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // ok
<!-- fin de version si -->

En C# 7.0, vous pouvez intégrer la déclaration de la variable passée au paramètre `out`, éliminant ainsi le besoin d'une déclaration de variable distincte :

<!-- si version [gte 7.0] -->
    if (int.TryParse(input, out var value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // still ok, the value in scope within the remainder of the body
<!-- fin de version si -->

Si certains des paramètres renvoyés par une fonction dans `out` ne sont pas nécessaires, vous pouvez utiliser l'opérateur _discard_ `_`.

    p.GetCoordinates(out var x, out _); // I only care about x

Une déclaration `out var` peut être utilisée avec n'importe quelle fonction existante qui a déjà des paramètres `out`. La syntaxe de déclaration de fonction reste la même et aucune exigence supplémentaire n'est nécessaire pour rendre la fonction compatible avec une déclaration `out var`. Cette fonctionnalité est simplement du sucre syntaxique.

Une autre caractéristique de la déclaration `out var` est qu'elle peut être utilisée avec des types anonymes.

<!-- si version [gte 7.0] -->
    var a = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var groupedByMod2 = a.Select(x => new
                                      {
                                          Source = x,
                                          Mod2 = x % 2
                                      })
                         .GroupBy(x => x.Mod2)
                         .ToDictionary(g => g.Key, g => g.ToArray());
    if (groupedByMod2.TryGetValue(1, out var oddElements))
    {
        Console.WriteLine(oddElements.Length);
    }
<!-- fin de version si -->
    
Dans ce code, nous créons un "Dictionnaire" avec la clé "int" et un tableau de valeurs de type anonyme. Dans la version précédente de C #, il était impossible d'utiliser la méthode `TryGetValue` ici car elle vous obligeait à déclarer la variable `out` (qui est de type anonyme!). Cependant, avec `out var` nous n'avons pas besoin de spécifier explicitement le type de la variable `out`.

# Limites

Notez que les déclarations out var sont d'une utilité limitée dans les requêtes LINQ car les expressions sont interprétées comme des corps d'expression lambda, de sorte que la portée des variables introduites est limitée à ces lambdas. Par exemple, le code suivant ne fonctionnera pas :

    var nums = 
        from item in seq
        let success = int.TryParse(item, out var tmp)
        select success ? tmp : 0; // Error: The name 'tmp' does not exist in the current context



# Références

* [Proposition de déclaration originale de var sur GitHub] (https://github.com/dotnet/roslyn/issues/6183)

## Correspondance de modèle
Les extensions de correspondance de modèles pour C # offrent de nombreux avantages de la correspondance de modèles à partir de langages fonctionnels, mais d'une manière qui s'intègre en douceur avec la sensation du langage sous-jacent

**expression "changer"**
-----
La correspondance de modèle étend l'instruction `switch` pour basculer sur les types :

    class Geometry {} 

    class Triangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
        public int Base { get; set; }
    }

    class Rectangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
    }

    class Square : Geometry
    {
        public int Width { get; set; }
    }

    public static void PatternMatching()
    {
        Geometry g = new Square { Width = 5 }; 
        
        switch (g)
        {
            case Triangle t:
                Console.WriteLine($"{t.Width} {t.Height} {t.Base}");
                break;
            case Rectangle sq when sq.Width == sq.Height:
                Console.WriteLine($"Square rectangle: {sq.Width} {sq.Height}");
                break;
            case Rectangle r:
                Console.WriteLine($"{r.Width} {r.Height}");
                break;
            case Square s:
                Console.WriteLine($"{s.Width}");
                break;
            default:
                Console.WriteLine("<other>");
                break;
        }
    }


**'est' l'expression**
---- 

La correspondance de modèle étend l'opérateur `is` pour vérifier un type et déclarer une nouvelle variable en même temps.

# Exemple

<!-- si version [lt 7.0] -->
    string s = o as string;
    if(s != null)
    {
        // do something with s
    }
<!-- fin de version si -->

peut être réécrit comme suit :

<!-- si version [gte 7.0] -->
    if(o is string s)
    {
        //Do something with s
    };
<!-- fin de version si -->

Notez également que la portée de la variable de modèle `s` est étendue à l'extérieur du bloc `if` atteignant la fin de la portée englobante, exemple :

    if(someCondition)
    {
       if(o is string s)
       {
          //Do something with s
       }
       else
       {
         // s is unassigned here, but accessible 
       }
    
       // s is unassigned here, but accessible 
    }
    // s is not accessible here

## Séparateurs de chiffres
Le trait de soulignement "_" peut être utilisé comme séparateur de chiffres. Le fait de pouvoir regrouper des chiffres dans de grands littéraux numériques a un impact significatif sur la lisibilité.

Le trait de soulignement peut apparaître n'importe où dans un littéral numérique, sauf comme indiqué ci-dessous. Différents regroupements peuvent avoir un sens dans différents scénarios ou avec différentes bases numériques.

Toute séquence de chiffres peut être séparée par un ou plusieurs traits de soulignement. Le `_` est autorisé en décimales ainsi qu'en exposants. Les séparateurs n'ont aucun impact sémantique - ils sont simplement ignorés.

    int bin = 0b1001_1010_0001_0100;
    int hex = 0x1b_a0_44_fe;
    int dec = 33_554_432;
    int weird = 1_2__3___4____5_____6______7_______8________9;
    double real = 1_000.111_1e-1_000;

**Où le séparateur de chiffres `_` ne peut pas être utilisé :**
- au début de la valeur (`_121`)
- à la fin de la valeur (`121_` ou `121.05_`)
- à côté de la décimale (`10_.0`)
- à côté du caractère exposant (`1.1e_1`)
- à côté du spécificateur de type (`10_f`)
- immédiatement après le '0x' ou '0b' dans les littéraux binaires et hexadécimaux ([peut être modifié pour autoriser, par exemple, 0b_1001_1000][1])

[1] : https://github.com/dotnet/roslyn/issues/12680

## Littéraux binaires
Le préfixe **0b** peut être utilisé pour représenter des littéraux binaires.

Les littéraux binaires permettent de construire des nombres à partir de zéros et de uns, ce qui permet de voir plus facilement quels bits sont définis dans la représentation binaire d'un nombre. Cela peut être utile pour travailler avec des drapeaux binaires.

Voici des façons équivalentes de spécifier un `int` avec la valeur `34` (=2<sup>5</sup> + 2<sup>1</sup>) :

    // Using a binary literal:
    //   bits: 76543210
    int a1 = 0b00100010;          // binary: explicitly specify bits

    // Existing methods:
    int a2 = 0x22;                // hexadecimal: every digit corresponds to 4 bits
    int a3 = 34;                  // decimal: hard to visualise which bits are set
    int a4 = (1 << 5) | (1 << 1); // bitwise arithmetic: combining non-zero bits

# Énumérations de drapeaux

Auparavant, la spécification des valeurs d'indicateur pour un `enum` ne pouvait être effectuée qu'en utilisant l'une des trois méthodes de cet exemple :

    [Flags]
    public enum DaysOfWeek
    {
        // Previously available methods:
        //          decimal        hex       bit shifting
        Monday    =  1,    //    = 0x01    = 1 << 0
        Tuesday   =  2,    //    = 0x02    = 1 << 1
        Wednesday =  4,    //    = 0x04    = 1 << 2
        Thursday  =  8,    //    = 0x08    = 1 << 3
        Friday    = 16,    //    = 0x10    = 1 << 4
        Saturday  = 32,    //    = 0x20    = 1 << 5
        Sunday    = 64,    //    = 0x40    = 1 << 6
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

Avec les littéraux binaires, il est plus évident de savoir quels bits sont définis, et leur utilisation ne nécessite pas de comprendre les nombres hexadécimaux et l'arithmétique au niveau du bit :

    [Flags]
    public enum DaysOfWeek
    {
        Monday    = 0b00000001,
        Tuesday   = 0b00000010,
        Wednesday = 0b00000100,
        Thursday  = 0b00001000,
        Friday    = 0b00010000,
        Saturday  = 0b00100000,
        Sunday    = 0b01000000,
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

## lancer des expressions
C# 7.0 autorise le lancement en tant qu'expression à certains endroits :

    class Person
    {
        public string Name { get; }

        public Person(string name) => Name = name ?? throw new ArgumentNullException(nameof(name));

        public string GetFirstName()
        {
            var parts = Name.Split(' ');
            return (parts.Length > 0) ? parts[0] : throw new InvalidOperationException("No name!");
        }

        public string GetLastName() => throw new NotImplementedException();
    }


Avant C# 7.0, si vous vouliez lever une exception à partir d'un corps d'expression, vous deviez :

    var spoons = "dinner,desert,soup".Split(',');

    var spoonsArray = spoons.Length > 0 ? spoons : null;

    if (spoonsArray == null) 
    {
        throw new Exception("There are no spoons");
    }

Ou

    var spoonsArray = spoons.Length > 0 
        ? spoons 
        : new Func<string[]>(() => 
          {
              throw new Exception("There are no spoons");
          })();

Dans C# 7.0, ce qui précède est désormais simplifié en :

    var spoonsArray = spoons.Length > 0 ? spoons : throw new Exception("There are no spoons");



## Liste des membres du corps de l'expression étendue
C# 7.0 ajoute des accesseurs, des constructeurs et des finaliseurs à la liste des éléments pouvant avoir des corps d'expression :

    class Person
    {
        private static ConcurrentDictionary<int, string> names = new ConcurrentDictionary<int, string>();

        private int id = GetId();

        public Person(string name) => names.TryAdd(id, name); // constructors

        ~Person() => names.TryRemove(id, out _);              // finalizers

        public string Name
        {
            get => names[id];                                 // getters
            set => names[id] = value;                         // setters
        }
    }

Voir également la section [out var declaration][1] pour l'opérateur de suppression.

[1] : https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/6326/out-var-declaration

## ref return et ref local
Les retours de référence et les paramètres locaux de référence sont utiles pour manipuler et renvoyer des références à des blocs de mémoire au lieu de copier de la mémoire sans recourir à des pointeurs non sécurisés.

# Ref Retour

    public static ref TValue Choose<TValue>(
        Func<bool> condition, ref TValue left, ref TValue right)
    {
        return condition() ? ref left : ref right;
    }

Avec cela, vous pouvez passer deux valeurs par référence, l'une d'entre elles étant renvoyée en fonction d'une condition :

    Matrix3D left = …, right = …;
    Choose(chooser, ref left, ref right).M20 = 1.0;


# Réf Locale

    public static ref int Max(ref int first, ref int second, ref int third)
    {
        ref int max = first > second ? ref first : ref second;
        return max > third ? ref max : ref third;
    }
    …
    int a = 1, b = 2, c = 3;
    Max(ref a, ref b, ref c) = 4;
    Debug.Assert(a == 1); // true
    Debug.Assert(b == 2); // true
    Debug.Assert(c == 4); // true

# Opérations de référence non sécurisées
Dans `System.Runtime.CompilerServices.Unsafe`, un ensemble d'opérations non sécurisées a été défini qui vous permet de manipuler les valeurs `ref` comme s'il s'agissait de pointeurs, en gros.

Par exemple, réinterpréter une adresse mémoire (`ref`) comme un type différent :

    byte[] b = new byte[4] { 0x42, 0x42, 0x42, 0x42 };
    
    ref int r = ref Unsafe.As<byte, int>(ref b[0]);
    Assert.Equal(0x42424242, r);
    
    0x0EF00EF0;
    Assert.Equal(0xFE, b[0] | b[1] | b[2] | b[3]);

Méfiez-vous de [endianness] [1] lorsque vous faites cela, par ex. vérifiez `BitConverter.IsLittleEndian` si nécessaire et gérez-le en conséquence.

Ou parcourez un tableau de manière non sécurisée :

    int[] a = new int[] { 0x123, 0x234, 0x345, 0x456 };
    
    ref int r1 = ref Unsafe.Add(ref a[0], 1);
    Assert.Equal(0x234, r1);

    ref int r2 = ref Unsafe.Add(ref r1, 2);
    Assert.Equal(0x456, r2);

    ref int r3 = ref Unsafe.Add(ref r2, -3);
    Assert.Equal(0x123, r3);

Ou le "Soustraire" similaire :

    string[] a = new string[] { "abc", "def", "ghi", "jkl" };
    
    ref string r1 = ref Unsafe.Subtract(ref a[0], -2);
    Assert.Equal("ghi", r1);
    
    ref string r2 = ref Unsafe.Subtract(ref r1, -1);
    Assert.Equal("jkl", r2);
    
    ref string r3 = ref Unsafe.Subtract(ref r2, 3);
    Assert.Equal("abc", r3);

De plus, on peut vérifier si deux valeurs `ref` sont identiques, c'est-à-dire la même adresse :

    long[] a = new long[2];
    
    Assert.True(Unsafe.AreSame(ref a[0], ref a[0]));
    Assert.False(Unsafe.AreSame(ref a[0], ref a[1]));

# Liens
[Problème Roslyn Github] [2]

[System.Runtime.CompilerServices.Unsafe sur github][3]


[1] : https://en.wikipedia.org/wiki/Endianness
[2] : https://github.com/dotnet/roslyn/issues/118
[3] : https://github.com/dotnet/corefx/tree/master/src/System.Runtime.CompilerServices.Unsafe

## ValueTask<T>
`Task<T>` est une **classe** et entraîne une surcharge inutile de son allocation lorsque le résultat est immédiatement disponible.

`ValueTask<T>` est une **structure** et a été introduite pour empêcher l'allocation d'un objet `Task` dans le cas où le résultat de l'opération **async** est déjà disponible au moment de l'attente.

Ainsi, `ValueTask<T>` offre deux avantages :

# 1. Augmentation des performances

Voici un exemple de `Task<T>` :
- Nécessite une allocation de tas
- Prend 120ns avec JIT


    async Task<int> TestTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

Voici l'exemple analogique `ValueTask<T>` :
- Pas d'allocation de tas si le résultat est connu de manière synchrone (ce qui n'est pas le cas dans ce cas à cause de `Task.Delay`, mais c'est souvent le cas dans de nombreux scénarios réels `async`/`wait`)
- Prend 65ns avec JIT


    async ValueTask<int> TestValueTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

# 2. Flexibilité de mise en œuvre accrue

Les implémentations d'une interface asynchrone souhaitant être synchrone seraient autrement obligées d'utiliser `Task.Run` ou `Task.FromResult` (ce qui entraînerait la pénalité de performance discutée ci-dessus). Il y a donc une certaine pression contre les implémentations synchrones.

Mais avec `ValueTask<T>`, les implémentations sont plus libres de choisir entre être synchrone ou asynchrone sans impact sur les appelants.

Par exemple, voici une interface avec une méthode asynchrone :

    interface IFoo<T>
    {
        ValueTask<T> BarAsync();
    }

... et voici comment cette méthode pourrait être appelée :

    IFoo<T> thing = getThing();
    var x = await thing.BarAsync();

Avec `ValueTask`, le code ci-dessus fonctionnera avec **les implémentations synchrones ou asynchrones** :

## Implémentation synchrone :

    class SynchronousFoo<T> : IFoo<T>
    {
        public ValueTask<T> BarAsync()
        {
            var value = default(T);
            return new ValueTask<T>(value);
        }
    }

## Implémentation asynchrone

    class AsynchronousFoo<T> : IFoo<T>
    {
        public async ValueTask<T> BarAsync()
        {
            var value = default(T);
            await Task.Delay(1);
            return value;
        }
    }

# Remarques

Bien qu'il était prévu d'ajouter la structure `ValueTask` à [C# 7.0][1], elle a été conservée comme une autre bibliothèque pour le moment.
https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/28612/valuetaskt#
Le package `System.Threading.Tasks.Extensions` peut être téléchargé à partir de [Nuget Gallery] (https://www.nuget.org/packages/System.Threading.Tasks.Extensions/)

[1] : https://blogs.msdn.microsoft.com/dotnet/2016/08/24/whats-new-in-csharp-7-0/

