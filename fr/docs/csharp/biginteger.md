---
title: "GrandEntier"
slug: "grandentier"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Quand utiliser ##
Les objets "BigInteger" sont par nature très gourmands en RAM. Par conséquent, ils ne doivent être utilisés qu'en cas d'absolue nécessité, c'est-à-dire pour des nombres d'une échelle véritablement astronomique.

De plus, toutes les opérations arithmétiques sur ces objets sont d'un ordre de grandeur plus lentes que leurs homologues primitifs, ce problème s'aggrave à mesure que le nombre augmente car ils ne sont pas d'une taille fixe. Il est donc possible qu'un "BigInteger" malveillant provoque un crash en consommant toute la RAM disponible.

## Alternatives ##

Si la vitesse est impérative pour votre solution, il peut être plus efficace d'implémenter cette fonctionnalité vous-même en utilisant une classe enveloppant un `Byte[]` et en surchargeant vous-même les opérateurs nécessaires. Cependant, cela nécessite une quantité importante d'efforts supplémentaires.



## Calculer le premier nombre de Fibonacci à 1 000 chiffres
Incluez `using System.Numerics` et ajoutez une référence à `System.Numerics` au projet.

    using System;
    using System.Numerics;
    
    namespace Euler_25
    {
        class Program
        {
            static void Main(string[] args)
            {
                BigInteger l1 = 1;
                BigInteger l2 = 1;
                BigInteger current = l1 + l2;
                while (current.ToString().Length < 1000)
                {
                    l2 = l1;
                    l1 = current;
                    current = l1 + l2;
                }
                Console.WriteLine(current);
            }
        }
    }

Cet algorithme simple parcourt les nombres de Fibonacci jusqu'à ce qu'il atteigne une longueur d'au moins 1000 chiffres décimaux, puis l'imprime. Cette valeur est nettement supérieure à ce que même un "ulong" pourrait contenir.

Théoriquement, la seule limite de la classe `BigInteger` est la quantité de RAM que votre application peut consommer.

Remarque : "BigInteger" n'est disponible que dans .NET 4.0 et versions ultérieures.

