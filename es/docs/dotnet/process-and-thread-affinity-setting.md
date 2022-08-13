---
title: "Configuración de afinidad de procesos y subprocesos"
slug: "configuracion-de-afinidad-de-procesos-y-subprocesos"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parámetros
| Parámetro | Detalles |
| ------ | ------ |
| afinidad | entero que describe el conjunto de procesadores en los que se permite ejecutar el proceso. Por ejemplo, en un sistema de 8 procesadores, si desea que su proceso se ejecute solo en los procesadores 3 y 4, elija una afinidad como esta: 00001100 que equivale a 12 |

La afinidad de procesador de un subproceso es el conjunto de procesadores con los que tiene una relación. En otras palabras, aquellos en los que se puede programar para ejecutarse.

La afinidad del procesador representa cada procesador como un bit. El bit 0 representa el procesador uno, el bit 1 representa el procesador dos y así sucesivamente.


## Obtener máscara de afinidad de proceso


## Establecer máscara de afinidad de proceso


