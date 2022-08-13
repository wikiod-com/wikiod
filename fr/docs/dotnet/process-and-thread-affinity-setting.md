---
title: "Paramètre d'affinité de processus et de thread"
slug: "parametre-daffinite-de-processus-et-de-thread"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Paramètres
| Paramètre | Détails |
| ------ | ------ |
| affinité | entier qui décrit l'ensemble de processeurs sur lesquels le processus est autorisé à s'exécuter. Par exemple, sur un système à 8 processeurs si vous souhaitez que votre processus soit exécuté uniquement sur les processeurs 3 et 4, vous choisissez une affinité comme celle-ci : 00001100 qui est égal à 12 |

L'affinité de processeur d'un thread est l'ensemble de processeurs avec lesquels il est en relation. En d'autres termes, ceux sur lesquels il peut être programmé de s'exécuter.

L'affinité de processeur représente chaque processeur comme un bit. Le bit 0 représente le processeur un, le bit 1 représente le processeur deux, et ainsi de suite.


## Obtenir le masque d'affinité de processus


## Définir le masque d'affinité de processus


