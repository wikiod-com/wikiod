---
title: "Secuencias de escape de cadenas"
slug: "secuencias-de-escape-de-cadenas"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Sintaxis
- \\' — comilla simple (0x0027)
- \\" — comillas dobles (0x0022)
- \\\ — barra invertida (0x005C)
- \0 — nulo (0x0000)
- \a — alerta (0x0007)
- \b - retroceso (0x0008)
- \f — avance de formulario (0x000C)
- \n — nueva línea (0x000A)
- \r — retorno de carro (0x000D)
- \t — pestaña horizontal (0x0009)
- \v — pestaña vertical (0x000B)
- \u0000 - \uFFFF — Carácter Unicode
- \x0 - \xFFFF — Carácter Unicode (código con longitud variable)
- \U00000000 - \U0010FFFF — Carácter Unicode (para generar sustitutos)

Las secuencias de escape de cadenas se transforman en el carácter correspondiente en **tiempo de compilación**. Las cadenas ordinarias que contienen barras invertidas **no** se transforman.

Por ejemplo, las cadenas `notEscaped` y `notEscaped2` a continuación no se transforman en un carácter de nueva línea, sino que permanecerán como dos caracteres diferentes (`'\'` y `'n'`).

    string escaped = "\n";
    string notEscaped = "\\" + "n";
    string notEscaped2 = "\\n";

    Console.WriteLine(escaped.Length); // 1
    Console.WriteLine(notEscaped.Length); // 2            
    Console.WriteLine(notEscaped2.Length); // 2

## Escapar de símbolos especiales en cadenas literales
**barra invertida**

    // The filename will be c:\myfile.txt in both cases
    string filename = "c:\\myfile.txt";
    string filename = @"c:\myfile.txt";

El segundo ejemplo usa un [literal de cadena textual] (https://www.wikiod.com/es/docs/c%23/16/verbatim-strings#t=20151122021216101385), que no trata la barra invertida como un carácter de escape.

**Cotizaciones**

    string text = "\"Hello World!\", said the quick brown fox.";
    string verbatimText = @"""Hello World!"", said the quick brown fox.";

Ambas variables contendrán el mismo texto.

> "¡Hola mundo!", dijo el veloz zorro marrón.

**Nuevas líneas**

Los literales de cadena textuales pueden contener saltos de línea:

    string text = "Hello\r\nWorld!";
    string verbatimText = @"Hello
    World!";

Ambas variables contendrán el mismo texto.

## Secuencias de escape de caracteres Unicode
    string sqrt = "\u221A";      // √
    string emoji = "\U0001F601"; // 😁
    string text = "\u0022Hello World\u0022"; // "Hello World"
    string variableWidth = "\x22Hello World\x22"; // "Hello World"

## Escapar de símbolos especiales en caracteres literales
**Apóstrofes**

    char apostrophe = '\'';
**barra invertida**

    char oneBackslash = '\\';

## Uso de secuencias de escape en identificadores
Las secuencias de escape no están restringidas a los literales `string` y `char`.

Supongamos que necesita anular un método de terceros:

    protected abstract IEnumerable<Texte> ObtenirŒuvres();

y suponga que el carácter `Œ` no está disponible en la codificación de caracteres que utiliza para sus archivos fuente de C#. Tienes suerte, está permitido usar escapes del tipo `\u####` o `\U########` en ___identificadores___ en el código. Entonces es legal escribir:

    protected override IEnumerable<Texte> Obtenir\u0152uvres()
    {
        // ...
    }

y el compilador de C# sabrá que `Œ` y `\u0152` son el mismo carácter.

(Sin embargo, podría ser una buena idea cambiar a UTF-8 o una codificación similar que pueda manejar todos los caracteres).

## Las secuencias de escape no reconocidas producen errores en tiempo de compilación
Los siguientes ejemplos no compilarán:

    string s = "\c";
    char c = '\c';

En su lugar, producirán el error `Secuencia de escape no reconocida` en tiempo de compilación.

