---
title: "Type dynamique"
slug: "type-dynamique"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

Le mot clé `dynamic` déclare une variable dont le type n'est pas connu au moment de la compilation. Une variable "dynamique" peut contenir n'importe quelle valeur, et le type de la valeur peut changer pendant l'exécution.

Comme indiqué dans le livre "Metaprogramming in .NET", C# n'a pas de type de support pour le mot-clé `dynamic` :

> La fonctionnalité activée par le mot clé `dynamic` est un ensemble intelligent d'actions de compilateur qui émettent et utilisent des objets `CallSite` dans le conteneur de site de la portée d'exécution locale. Le compilateur gère ce que les programmeurs perçoivent comme un objet dynamique
références via ces instances `CallSite`. Les paramètres, les types de retour, les champs et les propriétés qui reçoivent un traitement dynamique au moment de la compilation peuvent être marqués avec des métadonnées pour indiquer qu'ils ont été générés pour une utilisation dynamique, mais le type de données sous-jacent pour eux sera toujours `System.Object`.

 

## Création d'un objet dynamique avec des propriétés
    using System;
    using System.Dynamic;
    
    dynamic info = new ExpandoObject();
    info.Id = 123;
    info.Another = 456;
    
    Console.WriteLine(info.Another);
    // 456
    
    Console.WriteLine(info.DoesntExist);
    // Throws RuntimeBinderException

## Création d'une variable dynamique
    dynamic foo = 123;
    Console.WriteLine(foo + 234);
    // 357    Console.WriteLine(foo.ToUpper())
    // RuntimeBinderException, since int doesn't have a ToUpper method

    foo = "123";
    Console.WriteLine(foo + 234);
    // 123234
    Console.WriteLine(foo.ToUpper()):
    // NOW A STRING

## Renvoie la dynamique
    using System;

    public static void Main()
    {
        var value = GetValue();
        Console.WriteLine(value);
        // dynamics are useful!
    }
    
    private static dynamic GetValue()
    {
        return "dynamics are useful!";
    }

## Gestion de types spécifiques inconnus au moment de la compilation


