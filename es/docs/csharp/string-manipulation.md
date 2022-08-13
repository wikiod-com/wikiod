---
title: "Manipulación de cadenas"
slug: "manipulacion-de-cadenas"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Reemplazar una cadena dentro de una cadena
Con el método [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), puede reemplazar parte de una cadena con otra cuerda.

    string s = "Hello World";
    s = s.Replace("World", "Universe"); // s = "Hello Universe"

Todas las apariciones de la cadena de búsqueda se reemplazan:

    string s = "Hello World";
    s = s.Replace("l", "L"); // s = "HeLLo WorLD"

`String.Replace` también se puede usar para *eliminar* parte de una cadena, especificando una cadena vacía como valor de reemplazo:

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"

## Cambiando el caso de los caracteres dentro de una Cadena
La clase [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) admite varios métodos para convertir entre mayúsculas y minúsculas caracteres en una cadena.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) se usa para devolver un objeto String convertido a minúsculas.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) se usa para devolver un objeto String convertido a mayúsculas.

**Nota:** La razón para usar las versiones *invariantes* de estos métodos es evitar la producción de letras específicas de la cultura inesperadas. Esto se explica [aquí en detalle] (http://stackoverflow.com/a/19778131/1379664).

Ejemplo:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Tenga en cuenta que *puede* elegir especificar una **[Cultura](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** al convertir a minúsculas y mayúsculas mediante [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) y [String.ToUpper (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) métodos en consecuencia.



## Encontrar una cadena dentro de una cadena
Utilizando el
[`System.String.Contains`][1] puede averiguar si existe una cadena en particular dentro de una cadena. El método devuelve un valor booleano, verdadero si la cadena existe o falso.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

Usando el método [`System.String.IndexOf`](https://msdn.microsoft.com/en-us/library/k8b1470s(v=vs.110).aspx), puede ubicar la posición inicial de una subcadena dentro de una cadena existente.
Tenga en cuenta que la posición devuelta se basa en cero, se devuelve un valor de -1 si no se encuentra la subcadena.

    string s = "Hello World";
    int location = s.IndexOf("ello"); // location = 1

Para buscar la primera ubicación desde el ***final*** de una cadena, use [`System.String.LastIndexOf `](https://msdn.microsoft.com/en-us/library/system.string. método lastindexof(v=vs.110).aspx):

    string s = "Hello World";
    int location = s.LastIndexOf("l"); // location = 9


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Eliminar (recortar) espacios en blanco de una cadena
El método [`System.String.Trim`](https://msdn.microsoft.com/en-us/library/t97s7bs3(v=vs.110).aspx) se puede usar para eliminar todas las líneas blancas iniciales y finales. caracteres de espacio de una cadena:

    string s = "     String with spaces at both ends      ";
    s = s.Trim(); // s = "String with spaces at both ends"

Además:
- Para eliminar los espacios en blanco solo desde el *comienzo* de una cadena, use: [`System.String.TrimStart`](https://msdn.microsoft.com/en-us/library/system.string.trimstart(v =vs.110).aspx)

- Para eliminar los espacios en blanco solo del *final* de una cadena, use: [`System.String.TrimEnd`](https://msdn.microsoft.com/en-us/library/system.string.trimend(v =vs.110).aspx)

**Subcadena para extraer parte de una cadena.**

El método [`System.String.Substring`][1] se puede utilizar para extraer una parte de la cadena.

    string s ="A portion of word that is retained";
    s=str.Substring(26);  //s="retained"

    s1 = s.Substring(0,5);  //s="A por"
    

[1]: https://msdn.microsoft.com/en-us/library/hxthx5h6(v=vs.110).aspx

## Dividir una cadena usando un delimitador
Utilice el método [`System.String.Split`](https://msdn.microsoft.com/en-us/library/system.string.split(v=vs.110).aspx) para devolver una matriz de cadenas que contiene subcadenas de la cadena original, divididas en función de un delimitador especificado:

    string sentence = "One Two Three Four";
    string[] stringArray = sentence.Split(' ');

    foreach (string word in stringArray)
    {
        Console.WriteLine(word);    
    }

Producción:

> Uno
> Dos
> Tres
> Cuatro

## Concatenar una matriz de cadenas en una sola cadena
El método [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) permite concatenar todos los elementos en una matriz de cadenas, usando un separador especificado entre cada elemento:

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Concatenación de cadenas
La concatenación de cadenas se puede realizar mediante el método [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) , o (mucho más fácil) usando el operador `+`:

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

