---
title: "Procesamiento paralelo usando .Net framework"
slug: "procesamiento-paralelo-usando-net-framework"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Este tema trata sobre la programación multinúcleo utilizando Task Parallel Library con .NET Framework. La biblioteca paralela de tareas le permite escribir código que es legible por humanos y se ajusta con la cantidad de núcleos disponibles. Por lo tanto, puede estar seguro de que su software se actualizará automáticamente con el entorno de actualización.

## Extensiones paralelas
Se han introducido extensiones paralelas junto con la biblioteca paralela de tareas para lograr el paralelismo de datos. El paralelismo de datos se refiere a escenarios en los que la misma operación se realiza simultáneamente (es decir, en paralelo) en elementos de una colección o matriz de origen. .NET proporciona nuevas construcciones para lograr el paralelismo de datos mediante las construcciones Parallel.For y Parallel.Foreach.

    //Sequential version

    foreach (var item in sourcecollection){

    Process(item);

    }

    // Parallel equivalent

    Parallel.foreach(sourcecollection, item => Process(item));


La construcción Parallel.ForEach mencionada anteriormente utiliza múltiples núcleos y, por lo tanto, mejora el rendimiento de la misma manera.

