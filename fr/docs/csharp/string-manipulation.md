---
title: "Manipulation de chaînes"
slug: "manipulation-de-chaines"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Remplacement d'une chaîne dans une chaîne
À l'aide de la méthode [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), vous pouvez remplacer une partie d'une chaîne par une autre chaîne de caractères.

    string s = "Hello World";
    s = s.Replace("World", "Universe"); // s = "Hello Universe"

Toutes les occurrences de la chaîne de recherche sont remplacées :

    string s = "Hello World";
    s = s.Replace("l", "L"); // s = "HeLLo WorLD"

`String.Replace` peut également être utilisé pour *supprimer* une partie d'une chaîne, en spécifiant une chaîne vide comme valeur de remplacement :

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"

## Changer la casse des caractères dans une chaîne
La classe [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) prend en charge un certain nombre de méthodes pour convertir entre majuscules et minuscules caractères dans une chaîne.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) est utilisé pour renvoyer un objet String converti en minuscules.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) est utilisé pour renvoyer un objet String converti en majuscule.

**Remarque :** La raison d'utiliser les versions *invariantes* de ces méthodes est d'empêcher la production de lettres inattendues spécifiques à la culture. Ceci est expliqué [ici en détail](http://stackoverflow.com/a/19778131/1379664).

Exemple:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Notez que vous *pouvez* choisir de spécifier une **[Culture](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** spécifique lors de la conversion en minuscules et majuscules à l'aide de [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) et [String.ToUpper (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) méthodes en conséquence.



## Recherche d'une chaîne dans une chaîne
En utilisant le
[`System.String.Contains`][1] vous pouvez savoir si une chaîne particulière existe dans une chaîne. La méthode renvoie un booléen, vrai si la chaîne existe sinon faux.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

À l'aide de la méthode [`System.String.IndexOf`](https://msdn.microsoft.com/en-us/library/k8b1470s(v=vs.110).aspx), vous pouvez localiser la position de départ d'une sous-chaîne dans une chaîne existante.
Notez que la position renvoyée est basée sur zéro, une valeur de -1 est renvoyée si la sous-chaîne n'est pas trouvée.

    string s = "Hello World";
    int location = s.IndexOf("ello"); // location = 1

Pour trouver le premier emplacement à partir de la ***fin*** d'une chaîne, utilisez [`System.String.LastIndexOf `](https://msdn.microsoft.com/en-us/library/system.string. méthode lastindexof(v=vs.110).aspx) :

    string s = "Hello World";
    int location = s.LastIndexOf("l"); // location = 9


[1] : https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Suppression (rognage) de l'espace blanc d'une chaîne
La méthode [`System.String.Trim`](https://msdn.microsoft.com/en-us/library/t97s7bs3(v=vs.110).aspx) peut être utilisée pour supprimer tous les blancs de début et de fin. espacer les caractères d'une chaîne :

    string s = "     String with spaces at both ends      ";
    s = s.Trim(); // s = "String with spaces at both ends"

En outre:
- Pour supprimer les espaces blancs uniquement au *début* d'une chaîne, utilisez : [`System.String.TrimStart`](https://msdn.microsoft.com/en-us/library/system.string.trimstart(v =vs.110).aspx)

- Pour supprimer les espaces blancs uniquement à la *fin* d'une chaîne, utilisez : [`System.String.TrimEnd`](https://msdn.microsoft.com/en-us/library/system.string.trimend(v =vs.110).aspx)

**Sous-chaîne pour extraire une partie d'une chaîne.**

La méthode [`System.String.Substring`][1] peut être utilisée pour extraire une partie de la chaîne.

    string s ="A portion of word that is retained";
    s=str.Substring(26);  //s="retained"

    s1 = s.Substring(0,5);  //s="A por"
    

[1] : https://msdn.microsoft.com/en-us/library/hxthx5h6(v=vs.110).aspx

## Fractionner une chaîne à l'aide d'un délimiteur
Utilisez la méthode [`System.String.Split`](https://msdn.microsoft.com/en-us/library/system.string.split(v=vs.110).aspx) pour renvoyer un tableau de chaînes qui contient des sous-chaînes de la chaîne d'origine, divisées en fonction d'un délimiteur spécifié :

    string sentence = "One Two Three Four";
    string[] stringArray = sentence.Split(' ');

    foreach (string word in stringArray)
    {
        Console.WriteLine(word);    
    }

Production:

> Un
> Deux
> Trois
> Quatre

## Concaténer un tableau de chaînes en une seule chaîne
La méthode [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) permet de concaténer tous les éléments d'un tableau de chaînes, en utilisant un séparateur spécifié entre chaque élément :

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Concaténation de chaînes
La concaténation de chaînes peut être effectuée à l'aide de la méthode [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) , ou (beaucoup plus facile) en utilisant l'opérateur `+` :

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

