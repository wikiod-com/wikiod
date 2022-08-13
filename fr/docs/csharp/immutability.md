---
title: "Immutabilité"
slug: "immutabilite"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Classe System.String


## Chaînes et immuabilité
Les types immuables sont des types qui, lorsqu'ils sont modifiés, créent une nouvelle version de l'objet en mémoire, plutôt que de modifier l'objet existant en mémoire. L'exemple le plus simple est le type intégré `string`.

Prenant le code suivant, qui ajoute " world" au mot "Hello"

    string myString = "hello";
    myString += " world";

Ce qui se passe en mémoire dans ce cas, c'est qu'un nouvel objet est créé lorsque vous ajoutez à la "chaîne" dans la deuxième ligne. Si vous effectuez cette opération dans le cadre d'une grande boucle, cela peut entraîner des problèmes de performances dans votre application.

L'équivalent mutable d'une `string` est un `StringBuilder`

Prendre le code suivant

    StringBuilder myStringBuilder = new StringBuilder("hello");
    myStringBuilder.append(" world");

Lorsque vous l'exécutez, vous modifiez l'objet `StringBuilder` lui-même en mémoire.

