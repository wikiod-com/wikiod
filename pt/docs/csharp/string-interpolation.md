---
title: "Interpolação de String"
slug: "interpolacao-de-string"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Sintaxe
- $"conteúdo {expressão} conteúdo"
- $"conteúdo {expressão:formato} conteúdo"
- $"conteúdo {expressão} {{conteúdo entre chaves}} conteúdo}"
- $"conteúdo {expressão:formato} {{conteúdo entre chaves}} conteúdo}"

A interpolação de strings é uma abreviação para o método `string.Format()` que facilita a construção de strings com valores de variáveis ​​e expressões dentro delas.

    var name = "World";
    var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
    var newWay = $"Hello, {name}!";                   // returns "Hello, World"

## Formatar datas em strings
    var date = new DateTime(2015, 11, 11);
    var str = $"It's {date:MMMM d, yyyy}, make a wish!";
    System.Console.WriteLine(str);

Você também pode usar o método [`DateTime.ToString`][1] para formatar o objeto `DateTime`. Isso produzirá a mesma saída que o código acima.

    var date = new DateTime(2015, 11, 11);
    var str = date.ToString("MMMM d, yyyy");
    str = "It's " + str + ", make a wish!";
    Console.WriteLine(str);

**Resultado:**
>É 11 de novembro de 2015, faça um desejo!

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/DpRwV5)

[Demonstração ao vivo usando DateTime.ToString](https://dotnetfiddle.net/YnV9J0)

> **Observação:** `MM` significa meses e `mm` significa minutos. Tenha muito cuidado ao usá-los, pois erros podem introduzir bugs que podem ser difíceis de descobrir.


[1]: https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx

## Preenchendo a saída
A string pode ser formatada para aceitar um parâmetro de preenchimento que especificará quantas posições de caracteres a string inserida usará:

    ${value, padding}

> **OBSERVAÇÃO:** valores de preenchimento positivos indicam preenchimento esquerdo e negativo
> os valores de preenchimento indicam o preenchimento à direita.

**Preenchimento Esquerdo**
----

Um preenchimento esquerdo de 5 (adiciona 3 espaços antes do valor de número, portanto, ocupa um total de 5 posições de caracteres na string resultante.)

    var number = 42;
    var str = $"The answer to life, the universe and everything is {number, 5}.";
    //str is "The answer to life, the universe and everything is    42.";
    //                                                           ^^^^^
    System.Console.WriteLine(str);
    
**Resultado:**
       
    The answer to life, the universe and everything is    42.
[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/PpZXmk)

**Preenchimento Direito**
----

O preenchimento à direita, que usa um valor de preenchimento negativo, adicionará espaços ao final do valor atual.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, -5}.";
    //str is "The answer to life, the universe and everything is 42   .";
    //                                                           ^^^^^
    System.Console.WriteLine(str);

**Resultado:**

    The answer to life, the universe and everything is 42   .

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/QtKjGF)

**Preenchimento com especificadores de formato**
----

Você também pode usar especificadores de formatação existentes em conjunto com preenchimento.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
    //str is "The answer to life, the universe and everything is 42.1 ";
    //                                                           ^^^^^

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/34ZxP0)



## Expressões
Expressões completas também podem ser usadas em strings interpoladas.

    var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"
    
    string world = "world";
    var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"


[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/u9lzeg)



## Uso Simples
    var name = "World";
    var str = $"Hello, {name}!";
    //str now contains: "Hello, World!";

## Por trás das cenas

Internamente isso

    $"Hello, {name}!" 

Será compilado para algo assim:

    string.Format("Hello, {0}!", name);

    


## Formatando números em strings
Você pode usar dois pontos e a [sintaxe de formato numérico padrão](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) para controlar como os números são formatados.

    var decimalValue = 120.5;

    var asCurrency = $"It costs {decimalValue:C}";
    // String value is "It costs $120.50" (depending on your local currency settings)

    var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
    // String value is "Exactly 120.500"

    var integerValue = 57;

    var prefixedIfNecessary = $"{integerValue:D5}";
    // String value is "00057"


[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/z2XbG7)

