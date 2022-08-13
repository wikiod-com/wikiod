---
title: "Sequências de Escape de String"
slug: "sequencias-de-escape-de-string"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Sintaxe
- \\' — aspas simples (0x0027)
- \\" — aspas duplas (0x0022)
- \\\ — barra invertida (0x005C)
- \0 — nulo (0x0000)
- \a — alerta (0x0007)
- \b — retrocesso (0x0008)
- \f — feed de formulário (0x000C)
- \n — nova linha (0x000A)
- \r — retorno de carro (0x000D)
- \t — guia horizontal (0x0009)
- \v — guia vertical (0x000B)
- \u0000 - \uFFFF — caractere Unicode
- \x0 - \xFFFF — Caractere Unicode (código com comprimento variável)
- \U00000000 - \U0010FFFF — caractere Unicode (para gerar substitutos)

As sequências de escape de string são transformadas no caractere correspondente em **tempo de compilação**. Strings comuns que contêm barras invertidas **não** são transformadas.

Por exemplo, as strings `notEscaped` e `notEscaped2` abaixo não são transformadas em um caractere de nova linha, mas permanecerão como dois caracteres diferentes (`'\'` e `'n'`).

    string escaped = "\n";
    string notEscaped = "\\" + "n";
    string notEscaped2 = "\\n";

    Console.WriteLine(escaped.Length); // 1
    Console.WriteLine(notEscaped.Length); // 2            
    Console.WriteLine(notEscaped2.Length); // 2

## Escapando símbolos especiais em literais de string
**Barra invertida**

    // The filename will be c:\myfile.txt in both cases
    string filename = "c:\\myfile.txt";
    string filename = @"c:\myfile.txt";

O segundo exemplo usa um [literal de string literal](https://www.wikiod.com/pt/docs/c%23/16/verbatim-strings#t=20151122021216101385), que não trata a barra invertida como um caractere de escape.

**Citações**

    string text = "\"Hello World!\", said the quick brown fox.";
    string verbatimText = @"""Hello World!"", said the quick brown fox.";

Ambas as variáveis ​​conterão o mesmo texto.

> "Hello World!", disse a rápida raposa marrom.

**Novas linhas**

Os literais de string textuais podem conter novas linhas:

    string text = "Hello\r\nWorld!";
    string verbatimText = @"Hello
    World!";

Ambas as variáveis ​​conterão o mesmo texto.

## Sequências de escape de caracteres Unicode
    string sqrt = "\u221A";      // √
    string emoji = "\U0001F601"; // 😁
    string text = "\u0022Hello World\u0022"; // "Hello World"
    string variableWidth = "\x22Hello World\x22"; // "Hello World"

## Escapando símbolos especiais em literais de caracteres
**Apóstrofos**

    char apostrophe = '\'';
**Barra invertida**

    char oneBackslash = '\\';

## Usando sequências de escape em identificadores
As sequências de escape não são restritas a literais `string` e `char`.

Suponha que você precise substituir um método de terceiros:

    protected abstract IEnumerable<Texte> ObtenirŒuvres();

e suponha que o caractere `Œ` não esteja disponível na codificação de caracteres que você usa para seus arquivos de origem C#. Você tem sorte, é permitido usar escapes do tipo `\u####` ou `\U########` em ___identifiers___ no código. Portanto, é lícito escrever:

    protected override IEnumerable<Texte> Obtenir\u0152uvres()
    {
        // ...
    }

e o compilador C# saberá que `Œ` e `\u0152` são o mesmo caractere.

(No entanto, pode ser uma boa ideia mudar para UTF-8 ou uma codificação semelhante que possa lidar com todos os caracteres.)

## Sequências de escape não reconhecidas produzem erros em tempo de compilação
Os exemplos a seguir não serão compilados:

    string s = "\c";
    char c = '\c';

Em vez disso, eles produzirão o erro 'Sequência de escape não reconhecida' em tempo de compilação.

