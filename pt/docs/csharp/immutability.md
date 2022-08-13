---
title: "Imutabilidade"
slug: "imutabilidade"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Classe System.String


## Strings e imutabilidade
Tipos imutáveis ​​são tipos que, quando alterados, criam uma nova versão do objeto na memória, em vez de alterar o objeto existente na memória. O exemplo mais simples disso é o tipo interno `string`.

Tomando o seguinte código, que acrescenta "world" à palavra "Hello"

    string myString = "hello";
    myString += " world";

O que está acontecendo na memória neste caso é que um novo objeto é criado quando você acrescenta à `string` na segunda linha. Se você fizer isso como parte de um loop grande, existe a possibilidade de isso causar problemas de desempenho em seu aplicativo.

O equivalente mutável para uma `string` é um `StringBuilder`

Pegando o seguinte código

    StringBuilder myStringBuilder = new StringBuilder("hello");
    myStringBuilder.append(" world");

Ao executar isso, você está modificando o próprio objeto `StringBuilder` na memória.

