---
title: "Processamento paralelo usando o framework .Net"
slug: "processamento-paralelo-usando-o-framework-net"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Este tópico é sobre programação multi-core usando Task Parallel Library com .NET framework. A biblioteca paralela de tarefas permite que você escreva código que seja legível por humanos e se ajuste com o número de núcleos disponíveis. Assim, você pode ter certeza de que seu software se atualizará automaticamente com o ambiente de atualização.

## Extensões paralelas
Extensões paralelas foram introduzidas junto com a Biblioteca Paralela de Tarefas para alcançar o Paralelismo de dados. O paralelismo de dados refere-se a cenários nos quais a mesma operação é executada simultaneamente (ou seja, em paralelo) em elementos em uma coleção ou matriz de origem. O .NET fornece novas construções para alcançar o paralelismo de dados usando construções Parallel.For e Parallel.Foreach.

    //Sequential version

    foreach (var item in sourcecollection){

    Process(item);

    }

    // Parallel equivalent

    Parallel.foreach(sourcecollection, item => Process(item));


A construção Parallel.ForEach mencionada acima utiliza os vários núcleos e, portanto, aprimora o desempenho da mesma maneira.

