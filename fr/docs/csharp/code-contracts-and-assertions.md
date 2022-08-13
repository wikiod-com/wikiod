---
title: "Contrats de code et assertions"
slug: "contrats-de-code-et-assertions"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Les assertions pour vérifier la logique doivent toujours être vraies
Les assertions ne sont pas utilisées pour tester les paramètres d'entrée, mais pour vérifier que le déroulement du programme est correct, c'est-à-dire que vous pouvez faire certaines hypothèses sur votre code à un certain moment. En d'autres termes : un test effectué avec `Debug.Assert` doit *toujours* supposer que la valeur testée est `true`.

Debug.Assert ne s'exécute que dans les versions DEBUG ; il est filtré des versions RELEASE. Il doit être considéré comme un outil de débogage en plus des tests unitaires et non comme un remplacement des contrats de code ou des méthodes de validation des entrées.

Par exemple, voici une bonne affirmation :

    var systemData = RetrieveSystemConfiguration();
    Debug.Assert(systemData != null);

Ici, assert est un bon choix car nous pouvons supposer que RetrieveSystemConfiguration() renverra une valeur valide et ne renverra jamais null.

Voici un autre bon exemple :

    UserData user = RetrieveUserData();
    Debug.Assert(user != null);
    Debug.Assert(user.Age > 0);
    int year = DateTime.Today.Year - user.Age;

Tout d'abord, nous pouvons supposer que RetrieveUserData() renverra une valeur valide. Ensuite, avant d'utiliser la propriété Age, nous vérifions l'hypothèse (qui devrait toujours être vraie) que l'âge de l'utilisateur est strictement positif.

Ceci est un mauvais exemple d'affirmation :

    string input = Console.ReadLine();
    int age = Convert.ToInt32(input);
    Debug.Assert(age > 16);
    Console.WriteLine("Great, you are over 16");

Assert n'est pas pour la validation d'entrée car il est incorrect de supposer que cette assertion sera toujours vraie. Vous devez utiliser des méthodes de validation d'entrée pour cela. Dans le cas ci-dessus, vous devez également vérifier que la valeur d'entrée est un nombre en premier lieu.


