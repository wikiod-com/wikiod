---
title: "Strings literais"
slug: "strings-literais"
draft: false
images: []
weight: 9434
type: docs
toc: true
---

## Sintaxe
- @"strings literais são strings cujo conteúdo não é escapado, portanto, neste caso \n não representa o caractere de nova linha, mas dois caracteres individuais: \ e n. As strings literais são criadas prefixando o conteúdo da string com o caractere @"

- @"Para escapar das aspas, ""aspas duplas"" são usadas."

Para concatenar literais de string, use o símbolo @ no início de cada string.

    var combinedString = @"\t means a tab" + @" and \n means a newline";

## Strings literais interpoladas
As strings verbatim podem ser combinadas com os novos recursos https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation encontrados em C#6.

    Console.WriteLine($@"Testing \n 1 2 {5 - 2}
    New line");

**Resultado:**

> Teste \n 1 2 3
> Nova linha

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/cWyQE2)

Como esperado de uma string literal, as barras invertidas são ignoradas como caracteres de escape. E como esperado de uma string interpolada, qualquer expressão dentro de chaves é avaliada antes de ser inserida na string naquela posição.


## Escapando aspas duplas
Aspas duplas dentro de strings literais podem ser escapadas usando 2 aspas duplas sequenciais `""` para representar uma aspa dupla `"` na string resultante.

    var str = @"""I don't think so,"" he said.";
    Console.WriteLine(str);

**Resultado:**
>"Acho que não", disse ele.

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/c4OJoq)

## Strings literais instruem o compilador a não usar escapes de caracteres
Em uma string normal, o caractere de barra invertida é o caractere de escape, que instrui o compilador a examinar o(s) próximo(s) caractere(s) para determinar o caractere real na string. ([Lista completa de escapes de caracteres][1])

Em strings verbatim, não há escapes de caracteres (exceto para `""` que é transformado em `"`).
Para usar uma string literal, apenas coloque um `@` antes das aspas iniciais.

Esta sequência literal

    var filename = @"c:\temp\newfile.txt"

**Resultado:**

>c:\temp\newfile.txt

Ao contrário de usar uma string comum (não literal):

    var filename = "c:\temp\newfile.txt"

que irá produzir:

    c:    emp
    ewfile.txt

usando escape de caracteres. (O `\t` é substituído por um caractere de tabulação e o `\n` é substituído por uma nova linha.)

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/7kslXQ)






[1]: https://www.wikiod.com/pt/docs/c%23/39/string-escape-sequences#t=201607172257361795538&a=syntax

## Strings de várias linhas
    var multiLine = @"This is a 

    multiline paragraph";

**Resultado:**
>Este é um
>
>parágrafo de várias linhas

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/kfOUcH)

Strings de várias linhas que contêm aspas duplas também podem ser escapadas como estavam em uma única linha, porque são strings literais.
 
    var multilineWithDoubleQuotes = @"I went to a city named

                            ""San Diego""

                          during summer vacation.";

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/0hwJpf)

*Deve-se notar que os espaços/tabulações no início das linhas 2 e 3 estão realmente presentes no valor da variável; verifique [esta pergunta](http://stackoverflow.com/questions/7178136/multiline-formatting-for-verbatim-strings-in-c-sharp-prefix-with) para possíveis soluções.*


