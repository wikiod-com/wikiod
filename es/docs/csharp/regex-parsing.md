---
title: "Análisis de expresiones regulares"
slug: "analisis-de-expresiones-regulares"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Sintaxis
- `new Regex(pattern);` //*Crea una nueva instancia con un patrón definido.*
- `Regex.Match(input);` //*Inicia la búsqueda y devuelve la coincidencia.*
- `Regex.Matches(input);` //*Inicia la búsqueda y devuelve MatchCollection*


## Parámetros
| Nombre | Detalles|
| ------ | ------ |
| patrón | El patrón `string` que debe usarse para la búsqueda. Para más información: [msdn][1]|
| RegexOptions *[Opcional]* | Las opciones comunes aquí son `Singleline` y `Multiline`. Están cambiando el comportamiento de los elementos de patrón como el punto (.) que no cubrirá un `NewLine` (\n) en `Multiline-Mode` sino en `SingleLine-Mode`. Comportamiento predeterminado: [msdn][2] |
| Tiempo de espera *[Opcional]* | Cuando los patrones se vuelven más complejos, la búsqueda puede consumir más tiempo. Este es el tiempo de espera transcurrido para la búsqueda tal como se conoce en la programación de red.|


[1]: https://msdn.microsoft.com/en-us/library/ae5bf541(v=vs.90).aspx
[2]: https://msdn.microsoft.com/en-US/library/yd1hzczs(v=vs.110).aspx#Default

**Uso necesario**

    using System.Text.RegularExpressions;

**Agradable tener**

- Puede probar sus patrones en línea sin necesidad de compilar su solución para obtener resultados aquí: [Haga clic] [1]
- Regex101 Ejemplo: [Haz clic en mí][2]

_________

*Especialmente los principiantes tienden a exagerar sus tareas con expresiones regulares porque se siente poderoso y en el lugar correcto para búsquedas más complejas basadas en texto. Este es el punto en el que las personas intentan analizar documentos xml con expresiones regulares sin siquiera preguntarse si podría haber una clase ya terminada para esta tarea como `XmlDocument`.*

* Regex debería ser la última arma para elegir contra la complejidad. Al menos no olvide esforzarse un poco para buscar la "manera correcta" antes de escribir 20 líneas de patrones.*


[1]: https://regex101.com/
[2]: https://regex101.com/r/cG9lP5/1


## Partido único
*`usando System.Text.RegularExpressions;`*

    string pattern = ":(.*?):";
    string lookup = "--:text in here:--";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    // Get the match from your regex-object
    Match mLookup = rgxLookup.Match(lookup);
    
    // The group-index 0 always covers the full pattern.
    // Matches inside parentheses will be accessed through the index 1 and above.
    string found = mLookup.Groups[1].Value;

**Resultado:**

    found = "text in here"

## Coincidencias múltiples
*`usando System.Text.RegularExpressions;`*

    List<string> found = new List<string>();
    string pattern = ":(.*?):";
    string lookup = "--:text in here:--:another one:-:third one:---!123:fourth:";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    MatchCollection mLookup = rgxLookup.Matches(lookup);
    
    foreach(Match match in mLookup)
    {
        found.Add(match.Groups[1].Value);
    }

**Resultado:**

    found = new List<string>() { "text in here", "another one", "third one", "fourth" }

