---
title: "Fonction avec plusieurs valeurs de retour"
slug: "fonction-avec-plusieurs-valeurs-de-retour"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

Il n'y a pas de réponse inhérente en C# à ce soi-disant besoin. Néanmoins, il existe des solutions de contournement pour répondre à ce besoin.

La raison pour laquelle je qualifie le besoin de "soi-disant" est que nous n'avons besoin que de méthodes avec 2 ou plus de 2 valeurs pour revenir lorsque nous violons les bons principes de programmation. En particulier le [principe de responsabilité unique][1].

Par conséquent, il serait préférable d'être alerté lorsque nous avons besoin de fonctions renvoyant 2 valeurs ou plus, et d'améliorer notre conception.


[1] : https://en.wikipedia.org/wiki/Single_responsibility_principle

## Solution "objet anonyme" + "mot clé dynamique"
Vous pouvez renvoyer un objet anonyme depuis votre fonction

    public static object FunctionWithUnknowReturnValues ()
    {
        /// anonymous object
        return new { a = 1, b = 2 };
    }

Et assignez le résultat à un objet dynamique et lisez les valeurs qu'il contient.

    /// dynamic object
    dynamic x = FunctionWithUnknowReturnValues();

    Console.WriteLine(x.a);
    Console.WriteLine(x.b);

## Solution tuple
Vous pouvez renvoyer une instance de la classe `Tuple` à partir de votre fonction avec deux paramètres de modèle comme `Tuple<string, MyClass>` :

    public Tuple<string, MyClass> FunctionWith2ReturnValues ()
    {
        return Tuple.Create("abc", new MyClass());
    }

Et lisez les valeurs comme ci-dessous :

    Console.WriteLine(x.Item1);
    Console.WriteLine(x.Item2);

## Paramètres de référence et de sortie
Le mot clé `ref` est utilisé pour passer un [Argument as Reference][1]. `out` fera la même chose que `ref` mais il ne nécessite pas de valeur assignée par l'appelant avant d'appeler la fonction.

**Paramètre Ref** :-Si vous voulez passer une variable en tant que paramètre ref, vous devez l'initialiser avant de la passer en tant que paramètre ref à la méthode.

**Paramètre de sortie :-**
Si vous souhaitez transmettre une variable en tant que paramètre de sortie, vous n'avez pas besoin de l'initialiser avant de la transmettre en tant que paramètre de sortie à la méthode.

    static void Main(string[] args)
    {
        int a = 2;
        int b = 3;
        int add = 0;
        int mult= 0;
        AddOrMult(a, b, ref add, ref mult); //AddOrMult(a, b, out add, out mult);
        Console.WriteLine(add); //5
        Console.WriteLine(mult); //6
    }
    
    private static void AddOrMult(int a, int b, ref int add, ref int mult) //AddOrMult(int a, int b, out int add, out int mult)
    {
        add = a + b;
        mult = a * b;
    }


[1] : https://www.wikiod.com/fr/docs/c%23/3014/value-type-vs-reference-type#t=201607261617231313768

