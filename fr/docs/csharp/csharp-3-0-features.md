---
title: "Fonctionnalités C# 3.0"
slug: "fonctionnalites-c-30"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

La version 3.0 de C# a été publiée dans le cadre de la version 3.5 de .Net. De nombreuses fonctionnalités ajoutées à cette version étaient compatibles avec LINQ (Language INtegrated Queries).

Liste des fonctionnalités ajoutées :

-LINQ
- Expressions lambda
- Méthodes de vulgarisation
- Types anonymes
- Variables implicitement typées
- Initialiseurs d'objets et de collections
- Propriétés automatiquement mises en œuvre
- Arbres d'expressions

## Variables implicitement typées (var)
Le mot-clé `var` permet à un programmeur de taper implicitement une variable au moment de la compilation. Les déclarations `var` ont le même type que les variables explicitement déclarées.

    var squaredNumber = 10 * 10;
    var squaredNumberDouble = 10.0 * 10.0;
    var builder = new StringBuilder();
    var anonymousObject = new
    { 
        One = SquaredNumber,
        Two = SquaredNumberDouble,
        Three = Builder
    }

Les types des variables ci-dessus sont respectivement `int`, `double`, `StringBuilder` et un type anonyme.

Il est important de noter qu'une variable `var` n'est pas typée dynamiquement. `SquaredNumber = Builder` n'est pas valide car vous essayez de définir un `int` sur une instance de `StringBuilder`

## Requêtes intégrées au langage (LINQ)
    //Example 1
    int[] array = { 1, 5, 2, 10, 7 };

    // Select squares of all odd numbers in the array sorted in descending order
    IEnumerable<int> query = from x in array
                             where x % 2 == 1
                             orderby x descending
                             select x * x;
    // Result: 49, 25, 1
[Exemple tiré d'un article de Wikipedia sur C# 3.0, sous-section LINQ][1]

L'exemple 1 utilise une syntaxe de requête qui a été conçue pour ressembler aux requêtes SQL.

    //Example 2
    IEnumerable<int> query = array.Where(x => x % 2 == 1)
        .OrderByDescending(x => x)
        .Select(x => x * x);
    // Result: 49, 25, 1 using 'array' as defined in previous example
[Exemple tiré d'un article de Wikipedia sur C# 3.0, sous-section LINQ][1]

L'exemple 2 utilise la syntaxe de la méthode pour obtenir le même résultat que l'exemple 1.

Il est important de noter qu'en C#, la syntaxe de requête LINQ est [syntactic sugar][2] pour la syntaxe de la méthode LINQ. Le compilateur traduit les requêtes en appels de méthode au moment de la compilation. Certaines requêtes doivent être exprimées dans la syntaxe de la méthode. [De MSDN][3] - "Par exemple, vous devez utiliser un appel de méthode pour exprimer une requête qui récupère le nombre d'éléments qui correspondent à une condition spécifiée."


[1] : https://en.wikipedia.org/wiki/C_Sharp_3.0#LINQ_.28language-integrated_query.29
[2] : https://en.wikipedia.org/wiki/Syntactic_sugar
[3] : https://msdn.microsoft.com/en-us/library/bb397947.aspx

## Expressions lambda
Les expressions Lambda sont une extension des [méthodes anonymes][1] qui autorisent les paramètres implicitement typés et les valeurs de retour. Leur syntaxe est moins verbeuse que les méthodes anonymes et suit un style de programmation fonctionnel.

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

Le code ci-dessus affichera la somme des carrés des nombres 1 à 10 sur la console.

La première expression lambda met au carré les nombres de la liste. Puisqu'il n'y a qu'un seul paramètre, les parenthèses peuvent être omises. Vous pouvez inclure des parenthèses si vous le souhaitez :

    .Select( (number) => number * number);

ou tapez explicitement le paramètre mais les parenthèses sont alors obligatoires :

    .Select( (int number) => number * number);

Le corps lambda est une expression et a un retour implicite. Vous pouvez également utiliser un corps de déclaration si vous le souhaitez. Ceci est utile pour les lambdas plus complexes.

    .Select( number => { return number * number; } );

La méthode select renvoie un nouveau IEnumerable<int> avec les valeurs calculées.

La deuxième expression lambda additionne les nombres de la liste renvoyée par la méthode select. Les parenthèses sont obligatoires car il y a plusieurs paramètres. Les types des paramètres sont explicitement typés mais ce n'est pas nécessaire. La méthode ci-dessous est équivalente.

    .Aggregate( (first, second) => { return first + second; } );

Comme celui-ci :

    .Aggregate( (int first, int second) => first + second );

[1] : https://www.wikiod.com/fr/docs/c%23/60/methods/9338/anonymous-method#t=201608051345408629175

## Types anonymes
Les types anonymes offrent un moyen pratique d'encapsuler un ensemble de propriétés en lecture seule dans un seul objet sans avoir à définir explicitement un type au préalable. Le nom du type est généré par le compilateur et n'est pas disponible au niveau du code source. Le type de chaque propriété est déduit par le compilateur.

Vous pouvez créer des types anonymes en utilisant le mot-clé `new` suivi d'une accolade _(`{`)_. À l'intérieur des accolades, vous pouvez définir des propriétés comme dans le code ci-dessous.

    var v = new { Amount = 108, Message = "Hello" };

Il est également possible de créer un tableau de types anonymes. Voir le code ci-dessous :

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

Ou utilisez-le avec des requêtes LINQ :

    var productQuery = from prod in products
                       select new { prod.Color, prod.Price };

