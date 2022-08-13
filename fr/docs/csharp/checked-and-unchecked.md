---
title: "Coché et décoché"
slug: "coche-et-decoche"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Syntaxe
- vérifié(a + b) // expression vérifiée
- unchecked(a + b) // expression non cochée
- vérifié { c = a + b ; c += 5 ; } // bloc coché
- décochée { c = a + b ; c += 5 ; } // bloc non coché

## Coché et décoché
Les instructions C# s'exécutent dans un contexte coché ou non coché. Dans un contexte vérifié, le débordement arithmétique lève une exception. Dans un contexte non contrôlé, le débordement arithmétique est ignoré et le résultat est tronqué.

    short m = 32767;   
    short n = 32767;
    int result1 =  checked((short)(m + n));   //will throw an OverflowException
    int result2 =  unchecked((short)(m + n)); // will return -2

Si aucun de ces éléments n'est spécifié, le contexte par défaut dépendra d'autres facteurs, tels que les options du compilateur.

## Coché et décoché comme étendue
Les mots-clés peuvent également créer des étendues afin de (dé)vérifier plusieurs opérations.

    short m = 32767;
    short n = 32767;
    checked
    {
        int result1 = (short)(m + n); //will throw an OverflowException
    }
    unchecked
    {
        int result2 = (short)(m + n); // will return -2
    }

