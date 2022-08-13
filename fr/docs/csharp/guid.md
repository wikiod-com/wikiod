---
title: "Guide"
slug: "guide"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

GUID (ou UUID) est un acronyme pour 'Globally Unique Identifier' (ou 'Universally Unique Identifier'). Il s'agit d'un nombre entier de 128 bits utilisé pour identifier les ressources.

Les `Guid`s sont *Globally Unique Identifiers*, également connus sous le nom de *UUID*, *Universally Unique Identifiers*.

Ce sont des valeurs pseudo-aléatoires de 128 bits. Il y a tellement de `Guid`s valides (environ 10 ^ 18 `Guid`s pour chaque cellule de chaque personne sur Terre) que s'ils sont générés par un bon algorithme pseudo-aléatoire, ils peuvent être considérés comme uniques dans tout l'univers par tous les moyens pratiques moyens.

Les `Guid` sont le plus souvent utilisés comme clés primaires dans les bases de données. Leur avantage est que vous n'avez pas besoin d'appeler la base de données pour obtenir un nouvel identifiant dont l'unicité est (presque) garantie.

## Obtenir la représentation sous forme de chaîne d'un Guid
Une représentation sous forme de chaîne d'un Guid peut être obtenue en utilisant la méthode intégrée "ToString"

    string myGuidString = myGuid.ToString();

Selon vos besoins, vous pouvez également formater le Guid en ajoutant un argument de type de format à l'appel `ToString`.

    var guid = new Guid("7febf16f-651b-43b0-a5e3-0da8da49e90d");

    // None          "7febf16f651b43b0a5e30da8da49e90d"
    Console.WriteLine(guid.ToString("N"));

    // Hyphens       "7febf16f-651b-43b0-a5e3-0da8da49e90d"
    Console.WriteLine(guid.ToString("D"));

    // Braces        "{7febf16f-651b-43b0-a5e3-0da8da49e90d}"
    Console.WriteLine(guid.ToString("B"));

    // Parentheses   "(7febf16f-651b-43b0-a5e3-0da8da49e90d)"
    Console.WriteLine(guid.ToString("P"));

    // Hex           "{0x7febf16f,0x651b,0x43b0{0xa5,0xe3,0x0d,0xa8,0xda,0x49,0xe9,0x0d}}"
    Console.WriteLine(guid.ToString("X"));


## Création d'un guide
Voici les façons les plus courantes de créer une instance de Guid :

- Création d'un guid vide (`00000000-0000-0000-0000-000000000000`):


    Guid g = Guid.Empty;
    Guid g2 = new Guid();

- Création d'un nouveau Guid (pseudo-aléatoire):


    Guid g = Guid.NewGuid();

- Création de Guids avec une valeur spécifique :


    Guid g = new Guid("0b214de7-8958-4956-8eed-28f9ba2c47c6");
    Guid g2 = new Guid("0b214de7895849568eed28f9ba2c47c6");
    Guid g3 = Guid.Parse("0b214de7-8958-4956-8eed-28f9ba2c47c6");


## Déclaration d'un GUID nullable
Comme les autres types de valeur, GUID a également un type nullable qui peut prendre une valeur nulle.

Déclaration :

    Guid? myGuidVar = null;

Ceci est particulièrement utile lors de la récupération de données à partir de la base de données lorsqu'il est possible que la valeur d'une table soit NULL.

