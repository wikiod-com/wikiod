---
title: "Traitement parallèle à l'aide du framework .Net"
slug: "traitement-parallele-a-laide-du-framework-net"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Cette rubrique concerne la programmation multicœur à l'aide de la bibliothèque parallèle de tâches avec le framework .NET. La bibliothèque parallèle de tâches vous permet d'écrire du code lisible par l'homme et s'ajuste avec le nombre de cœurs disponibles. Ainsi, vous pouvez être sûr que votre logiciel se mettra automatiquement à niveau avec l'environnement de mise à niveau.

## Extensions parallèles
Des extensions parallèles ont été introduites avec la bibliothèque parallèle de tâches pour obtenir le parallélisme des données. Le parallélisme des données fait référence à des scénarios dans lesquels la même opération est effectuée simultanément (c'est-à-dire en parallèle) sur des éléments d'une collection ou d'un tableau source. Le .NET fournit de nouvelles constructions pour réaliser le parallélisme des données en utilisant les constructions Parallel.For et Parallel.Foreach.

    //Sequential version

    foreach (var item in sourcecollection){

    Process(item);

    }

    // Parallel equivalent

    Parallel.foreach(sourcecollection, item => Process(item));


La construction Parallel.ForEach mentionnée ci-dessus utilise plusieurs cœurs et améliore ainsi les performances de la même manière.

