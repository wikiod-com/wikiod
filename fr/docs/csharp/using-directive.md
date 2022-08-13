---
title: "Utilisation de la directive"
slug: "utilisation-de-la-directive"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

Le mot clé `using` est à la fois une directive (ce sujet) et une déclaration.

Pour l'instruction `using` (c'est-à-dire pour encapsuler la portée d'un objet `IDisposable`, en s'assurant qu'en dehors de cette portée, l'objet est proprement éliminé), veuillez consulter [Using Statement][1].


[1] : https://www.wikiod.com/fr/docs/c%23/38/using-statement

## Accéder aux membres statiques d'une classe
<!-- si version [gte 6.0] -->

Vous permet d'importer un type spécifique et d'utiliser les membres statiques du type sans les qualifier avec le nom du type. Ceci montre un exemple utilisant des méthodes statiques :

    using static System.Console;

    // ...

    string GetName()
    {
        WriteLine("Enter your name.");
        return ReadLine();
    }

Et ceci montre un exemple utilisant des propriétés et des méthodes statiques :

    using static System.Math;

    namespace Geometry
    {
        public class Circle
        {
            public double Radius { get; set; };

            public double Area => PI * Pow(Radius, 2);
        }
    }

<!-- fin de version si -->

## Associer un alias pour résoudre les conflits
Si vous utilisez plusieurs espaces de noms pouvant avoir des classes de même nom (telles que `System.Random` et `UnityEngine.Random`), vous pouvez utiliser un alias pour spécifier que `Random` provient de l'un ou de l'autre sans avoir à utiliser l'intégralité de l'espace de noms dans l'appel.

Par exemple:

    using UnityEngine;
    using System;

    Random rnd = new Random();

Cela amènera le compilateur à ne pas savoir avec quel `Random` évaluer la nouvelle variable. Au lieu de cela, vous pouvez faire :

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();

Cela ne vous empêche pas d'appeler l'autre par son espace de noms complet, comme ceci :

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();
    int unityRandom = UnityEngine.Random.Range(0,100);

`rnd` sera une variable `System.Random` et `unityRandom` sera une variable `UnityEngine.Random`.

## Utilisation des directives d'alias
Vous pouvez utiliser `using` afin de définir un alias pour un espace de noms ou un type. Plus de détails peuvent être trouvés dans [ici][1].

Syntaxe:

    using <identifier> = <namespace-or-type-name>;

Exemple:

    using NewType = Dictionary<string, Dictionary<string,int>>;
    NewType multiDictionary = new NewType();
    //Use instances as you are using the original one
    multiDictionary.Add("test", new Dictionary<string,int>());
 


[1] : https://msdn.microsoft.com/en-us/library/aa664765(v=vs.71).aspx

## Utilisation de base
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



## Référencer un espace de noms
    using System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //without prefixing them with the namespace.  i.e:

    //...
    var sb = new StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

## Associer un alias à un espace de noms
    using st = System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //prefixing them with only the defined alias and not the full namespace.  i.e:

    //...
    var sb = new st.StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

