---
title: "NullReferenceException"
slug: "nullreferenceexception"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## NullReferenceException expliqué
Une `NullReferenceException` est levée lorsque vous essayez d'accéder à un membre non statique (propriété, méthode, champ ou événement) d'un objet de référence mais qu'il est nul.

    Car myFirstCar = new Car();
    Car mySecondCar = null;
    Color myFirstColor = myFirstCar.Color; // No problem as myFirstCar exists / is not null
    Color mySecondColor = mySecondCar.Color; // Throws a NullReferenceException 
    // as mySecondCar is null and yet we try to access its color.

Pour déboguer une telle exception, c'est assez simple : sur la ligne où l'exception est levée, il suffit de regarder avant chaque '`.`' ou '`[`', ou en de rares occasions '`(`'.

    myGarage.CarCollection[currentIndex.Value].Color = theCarInTheStreet.Color;

D'où vient mon exception ?
Soit:

- `myGarage` est `null`
- `myGarage.CarCollection` est `null`
- `currentIndex` est `null`
- `myGarage.CarCollection[currentIndex.Value]` est `null`
- `theCarInTheStreet` est `null`

En mode débogage, vous n'avez qu'à placer le curseur de votre souris sur chacun de ces éléments et vous trouverez votre référence nulle. Ensuite, ce que vous devez faire est de comprendre pourquoi il n'a pas de valeur. La correction dépend totalement de l'objectif de votre méthode.

Avez-vous oublié de l'instancier/l'initialiser ?

    myGarage.CarCollection = new Car[10];

Êtes-vous censé faire quelque chose de différent si l'objet est nul ?

    if (myGarage == null)
    {
        Console.WriteLine("Maybe you should buy a garage first!");
    }

Ou peut-être que quelqu'un vous a donné un argument nul et n'était pas censé :

    if (theCarInTheStreet == null)
    {
        throw new ArgumentNullException("theCarInTheStreet");
    }
Dans tous les cas, rappelez-vous qu'une méthode ne doit jamais lever une NullReferenceException. Si c'est le cas, cela signifie que vous avez oublié de vérifier quelque chose.

