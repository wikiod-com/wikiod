---
title: "Récursivité"
slug: "recursivite"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Notez que l'utilisation de la récursivité peut avoir un impact important sur votre code, car chaque appel de fonction récursive sera ajouté à la pile. S'il y a trop d'appels, cela peut entraîner une exception **StackOverflow**. La plupart des "fonctions récursives naturelles" peuvent être écrites sous la forme d'une construction de boucle `for`, `while` ou `foreach`, et bien que n'ayant pas l'air si chic ** ou ** intelligent ** sera plus efficace.

Réfléchissez toujours à deux fois et utilisez la récursivité avec précaution - sachez pourquoi vous l'utilisez :

- la récursivité doit être utilisée lorsque vous savez que le nombre d'appels récursifs n'est pas *excessif*
- *excessif* signifie que cela dépend de la quantité de mémoire disponible
- la récursivité est utilisée car il s'agit d'une version de code plus claire et plus propre, elle est plus lisible qu'une fonction itérative ou basée sur une boucle. C'est souvent le cas car cela donne un code plus propre et plus compact (c'est-à-dire moins de lignes de code).
- mais attention, cela peut être moins efficace ! Par exemple dans la récursivité de Fibonacci, pour calculer le *nième* nombre de la séquence, le temps de calcul va croître de façon exponentielle !

Si vous voulez plus de théorie, lisez :
- https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html
- https://en.wikipedia.org/wiki/Recursion#In_computer_science


## Récursivité en langage courant
La récursivité peut être définie comme :

> Une méthode qui s'appelle jusqu'à ce qu'une condition spécifique soit remplie.

Un excellent et simple exemple de récursivité est une méthode qui obtiendra la factorielle d'un nombre donné :

    public int Factorial(int number)
    {
        return number == 0 ? 1 : n * Factorial(number - 1);
    }

Dans cette méthode, nous pouvons voir que la méthode prendra un argument, `number`.

Pas à pas:

Étant donné l'exemple, l'exécution de `Factorial (4)`

1. Est-ce que `nombre (4) == 1` ?
2. Non ? return `4 * Factoriel(nombre-1)` (3)
3. Parce que la méthode est appelée une fois de plus, elle répète maintenant la première étape en utilisant `Factorial(3)` comme nouvel argument.
4. Cela continue jusqu'à ce que `Factorial(1)` soit exécuté et `number (1) == 1` renvoie 1.
5. Dans l'ensemble, le calcul "accumule" `4 * 3 * 2 * 1` et renvoie finalement 24.

La clé pour comprendre la récursivité est que la méthode appelle une *nouvelle instance* d'elle-même. Après le retour, l'exécution de l'instance appelante se poursuit.

## Suite de Fibonacci
Vous pouvez calculer un nombre dans la séquence de Fibonacci en utilisant la récursivité.

Suivant la théorie mathématique de F(n) = F(n-2) + F(n-1), pour tout i > 0,

    // Returns the i'th Fibonacci number
    public int fib(int i) {
        if(i <= 2) {
            // Base case of the recursive function.
            // i is either 1 or 2, whose associated Fibonacci sequence numbers are 1 and 1.
            return 1;
        }
        // Recursive case. Return the sum of the two previous Fibonacci numbers.
        // This works because the definition of the Fibonacci sequence specifies
        // that the sum of two adjacent elements equals the next element.
        return  fib(i - 2) + fib(i - 1);
        
    }

    fib(10); // Returns 55

## Décrire récursivement une structure d'objet


## Utilisation de la récursivité pour obtenir l'arborescence des répertoires
L'une des utilisations de la récursivité est de naviguer dans une structure de données hiérarchique, comme une arborescence de répertoires de système de fichiers, sans connaître le nombre de niveaux de l'arborescence ni le nombre d'objets à chaque niveau. Dans cet exemple, vous verrez comment utiliser la récursivité sur une arborescence de répertoires pour trouver tous les sous-répertoires d'un répertoire spécifié et imprimer l'arborescence entière sur la console.

    internal class Program
    {
        internal const int RootLevel = 0;
        internal const char Tab = '\t';

        internal static void Main()
        {
            Console.WriteLine("Enter the path of the root directory:");
            var rootDirectorypath = Console.ReadLine();

            Console.WriteLine(
                $"Getting directory tree of '{rootDirectorypath}'");

            PrintDirectoryTree(rootDirectorypath);
            Console.WriteLine("Press 'Enter' to quit...");
            Console.ReadLine();
        }

        internal static void PrintDirectoryTree(string rootDirectoryPath)
        {
            try
            {
                if (!Directory.Exists(rootDirectoryPath))
                {
                    throw new DirectoryNotFoundException(
                        $"Directory '{rootDirectoryPath}' not found.");
                }

                var rootDirectory = new DirectoryInfo(rootDirectoryPath);
                PrintDirectoryTree(rootDirectory, RootLevel);
            }
            catch (DirectoryNotFoundException e)
            {
                Console.WriteLine(e.Message);
            }
        }

        private static void PrintDirectoryTree(
            DirectoryInfo directory, int currentLevel)
        {
            var indentation = string.Empty;
            for (var i = RootLevel; i < currentLevel; i++)
            {
                indentation += Tab;
            }

            Console.WriteLine($"{indentation}-{directory.Name}");
            var nextLevel = currentLevel + 1;
            try
            {
                foreach (var subDirectory in directory.GetDirectories())
                {
                    PrintDirectoryTree(subDirectory, nextLevel);
                }
            }
            catch (UnauthorizedAccessException e)
            {
                Console.WriteLine($"{indentation}-{e.Message}");
            }
        }
    }

Ce code est un peu plus compliqué que le strict minimum pour accomplir cette tâche, car il inclut une vérification des exceptions pour gérer tout problème d'obtention des répertoires. Vous trouverez ci-dessous une répartition du code en segments plus petits avec des explications pour chacun.

`Principal` :

La méthode principale prend une entrée d'un utilisateur sous forme de chaîne, qui doit être utilisée comme chemin d'accès au répertoire racine. Il appelle ensuite la méthode `PrintDirectoryTree` avec cette chaîne comme paramètre.

`PrintDirectoryTree(chaîne)` :

Il s'agit de la première des deux méthodes qui gèrent l'impression réelle de l'arborescence des répertoires. Cette méthode prend une chaîne représentant le chemin d'accès au répertoire racine comme paramètre. Il vérifie si le chemin est un répertoire réel, et si ce n'est pas le cas, lève une `DirectoryNotFoundException` qui est ensuite gérée dans le bloc catch. Si le chemin est un répertoire réel, un objet `DirectoryInfo` `rootDirectory` est créé à partir du chemin et la deuxième méthode `PrintDirectoryTree` est appelée avec l'objet `rootDirectory` et `RootLevel`, qui est une constante entière avec une valeur de zéro.

`PrintDirectoryTree(DirectoryInfo, int)` :

Cette deuxième méthode gère le gros du travail. Il prend un `DirectoryInfo` et un entier comme paramètres. Le `DirectoryInfo` est le répertoire courant et l'entier est la profondeur du répertoire par rapport à la racine. Pour faciliter la lecture, la sortie est indentée pour chaque niveau de profondeur du répertoire actuel, de sorte que la sortie ressemble à ceci :

    -Root
        -Child 1
        -Child 2
            -Grandchild 2.1
        -Child 3

Une fois que le répertoire courant est imprimé, ses sous-répertoires sont récupérés, et cette méthode est alors appelée sur chacun d'eux avec une valeur de niveau de profondeur d'un de plus que le courant. Cette partie est la récursivité : la méthode qui s'appelle elle-même. Le programme fonctionnera de cette manière jusqu'à ce qu'il ait visité tous les répertoires de l'arborescence. Lorsqu'elle atteint un répertoire sans sous-répertoires, la méthode revient automatiquement.

Cette méthode intercepte également une `UnauthorizedAccessException`, qui est levée si l'un des sous-répertoires du répertoire actuel est protégé par le système. Le message d'erreur est imprimé au niveau d'indentation actuel pour plus de cohérence.

La méthode ci-dessous fournit une approche plus basique de ce problème :

    internal static void PrintDirectoryTree(string directoryName)
    {
        try
        {
            if (!Directory.Exists(directoryName)) return;
            Console.WriteLine(directoryName);
            foreach (var d in Directory.GetDirectories(directoryName))
            {
                PrintDirectoryTree(d);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

Cela n'inclut pas la vérification d'erreur spécifique ou le formatage de sortie de la première approche, mais cela fait effectivement la même chose. Puisqu'il n'utilise que des chaînes par opposition à `DirectoryInfo`, il ne peut pas donner accès à d'autres propriétés de répertoire telles que les autorisations.

## Calcul de la puissance de
Le calcul de la puissance d'un nombre donné peut également être effectué de manière récursive.
Étant donné un nombre de base `n` et un exposant `e`, nous devons nous assurer de diviser le problème en morceaux en diminuant l'exposant `e`.

Exemple théorique :

- 2² = 2x2
- 2³ = 2x2x2
ou, 2³ = 2² x 2<br/>C'est là que réside le secret de notre algorithme récursif (voir le code ci-dessous). Il s'agit de prendre le problème et de le séparer en morceaux plus petits et plus simples à résoudre.
- **Remarques**
- lorsque le nombre de base est 0, nous devons être conscients de retourner 0 car 0³ = 0 x 0 x 0
- lorsque l'exposant est 0, il faut être conscient de toujours retourner 1, car c'est une règle mathématique.

Exemple de code :

    public int CalcPowerOf(int b, int e) {
        if (b == 0) { return 0; } // when base is 0, it doesn't matter, it will always return 0
        if (e == 0) { return 1; } // math rule, exponent 0 always returns 1
        return b * CalcPowerOf(b, e - 1); // actual recursive logic, where we split the problem, aka: 2³ = 2 * 2² etc..
    }

Tests dans xUnit pour vérifier la logique :<br/>
Bien que ce ne soit pas nécessaire, il est toujours bon d'écrire des tests pour vérifier votre logique. J'inclus ici ceux écrits dans le [xUnit framework][1].

        [Theory]
        [MemberData(nameof(PowerOfTestData))]
        public void PowerOfTest(int @base, int exponent, int expected) {
            Assert.Equal(expected, CalcPowerOf(@base, exponent));
        }

        public static IEnumerable<object[]> PowerOfTestData() {
            yield return new object[] { 0, 0, 0 };
            yield return new object[] { 0, 1, 0 };
            yield return new object[] { 2, 0, 1 };
            yield return new object[] { 2, 1, 2 };
            yield return new object[] { 2, 2, 4 };
            yield return new object[] { 5, 2, 25 };
            yield return new object[] { 5, 3, 125 };
            yield return new object[] { 5, 4, 625 };
    }


[1] : https://xunit.github.io/

## Calcul factoriel
La factorielle d'un nombre (notée !, comme par exemple 9 !) est la multiplication de ce nombre par la factorielle de la valeur inférieure. Ainsi, par exemple, 9 ! = 9 x 8 ! = 9 x 8 x 7 ! = 9 × 8 × 7 × 6 × 5 × 4 × 3 × 2 × 1.

Donc dans le code qui devient, en utilisant la récursivité :

    long Factorial(long x)
    {
        if (x < 1)
        {
            throw new OutOfRangeException("Factorial can only be used with positive numbers.");
        }
    
        if (x == 1)
        {
            return 1;
        } else {
            return x * Factorial(x - 1);
        }
    }



