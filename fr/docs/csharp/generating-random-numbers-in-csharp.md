---
title: "Génération de nombres aléatoires en C#"
slug: "generation-de-nombres-aleatoires-en-c"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Syntaxe
- Aléatoire()

- Aléatoire (int Seed)

- int Suivant()

- int Suivant(int maxValeur)

- int Suivant(int minValue, int maxValue)


## Paramètres
| Paramètres | Détails |
| ---------- | ------- |
| Graine | Une valeur pour générer des nombres aléatoires. Si elle n'est pas définie, la valeur par défaut est déterminée par l'heure système actuelle.
| valeurmin | Les nombres générés ne seront pas inférieurs à cette valeur. S'il n'est pas défini, la valeur par défaut est 0.
| valeurmax | Les nombres générés seront inférieurs à cette valeur. S'il n'est pas défini, la valeur par défaut est `Int32.MaxValue`.
| valeur de retour | Renvoie un nombre avec une valeur aléatoire.

La graine aléatoire générée par le système n'est pas la même dans chaque exécution différente.

Les graines générées en même temps peuvent être les mêmes.

## Générer un int aléatoire
Cet exemple génère des valeurs aléatoires entre 0 et 2147483647.

    Random rnd = new Random();
    int randomNumber = rnd.Next();

## Générer un int aléatoire dans une plage donnée
Génère un nombre aléatoire entre `minValue` et `maxValue - 1`.

    Random rnd = new Random();
    var randomBetween10And20 = rnd.Next(10, 20);

## Générer la même séquence de nombres aléatoires encore et encore
Lors de la création d'instances "aléatoires" avec la même graine, les mêmes numéros seront générés.

    int seed = 5;
    for (int i = 0; i < 2; i++)
    {
       Console.WriteLine("Random instance " + i);
       Random rnd = new Random(seed);
       for (int j = 0; j < 5; j++)
       {
          Console.Write(rnd.Next());
          Console.Write(" ");
       }
    
       Console.WriteLine();
    }

Production:

    Random instance 0
    726643700 610783965 564707973 1342984399 995276750
    Random instance 1
    726643700 610783965 564707973 1342984399 995276750

## Créez plusieurs classes aléatoires avec différentes graines simultanément
Deux classes aléatoires créées en même temps auront la même valeur de départ.

L'utilisation de `System.Guid.NewGuid().GetHashCode()` peut obtenir une graine différente même en même temps.

    Random rnd1 = new Random();
    Random rnd2 = new Random();
    Console.WriteLine("First 5 random number in rnd1");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());

    Console.WriteLine("First 5 random number in rnd2");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

    rnd1 = new Random(Guid.NewGuid().GetHashCode());
    rnd2 = new Random(Guid.NewGuid().GetHashCode());
    Console.WriteLine("First 5 random number in rnd1 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());
    Console.WriteLine("First 5 random number in rnd2 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

Une autre façon d'obtenir des valeurs de départ différentes consiste à utiliser une autre instance "Random" pour récupérer les valeurs de départ.

    Random rndSeeds = new Random();
    Random rnd1 = new Random(rndSeeds.Next());
    Random rnd2 = new Random(rndSeeds.Next());
Cela permet également de contrôler le résultat de toutes les instances `Random` en définissant uniquement la valeur de départ pour `rndSeeds`. Toutes les autres instances seront dérivées de manière déterministe de cette valeur de départ unique.

## Générer un double aléatoire
Générer un nombre aléatoire entre 0 et 1,0. (hors 1.0)

    Random rnd = new Random();
    var randomDouble = rnd.NextDouble();



## Générer un caractère aléatoire
Générer une lettre aléatoire entre `a` et `z` en utilisant la surcharge `Next()` pour une plage de nombres donnée, puis en convertissant le `int` résultant en un `char`

    Random rnd = new Random();
    char randomChar = (char)rnd.Next('a','z'); 
    //'a' and 'z' are interpreted as ints for parameters for Next()
    

## Générer un nombre qui est un pourcentage d'une valeur maximale
Un besoin courant de nombres aléatoires est de générer un nombre qui est `X%` d'une certaine valeur maximale. cela peut être fait en traitant le résultat de `NextDouble()` comme un pourcentage :

    var rnd = new Random();
    var maxValue = 5000;
    var percentage = rnd.NextDouble();
    var result = maxValue * percentage; 
    //suppose NextDouble() returns .65, result will hold 65% of 5000: 3250.



