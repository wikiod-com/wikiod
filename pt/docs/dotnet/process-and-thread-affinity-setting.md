---
title: "Configuração de afinidade de processo e thread"
slug: "configuracao-de-afinidade-de-processo-e-thread"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parâmetros
| Parâmetro | Detalhes |
| ------ | ------ |
| afinidade | inteiro que descreve o conjunto de processadores nos quais o processo pode ser executado. Por exemplo, em um sistema de 8 processadores, se você deseja que seu processo seja executado apenas nos processadores 3 e 4, escolha afinidade assim: 00001100, que é igual a 12 |

A afinidade de processador de um thread é o conjunto de processadores com os quais ele tem um relacionamento. Em outras palavras, aqueles em que ele pode ser programado para ser executado.

A afinidade do processador representa cada processador como um bit. O bit 0 representa o processador um, o bit 1 representa o processador dois e assim por diante.


## Obter máscara de afinidade do processo


## Definir máscara de afinidade do processo


