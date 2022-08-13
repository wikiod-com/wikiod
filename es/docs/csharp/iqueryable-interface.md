---
title: "Interfaz consultable"
slug: "interfaz-consultable"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Traducir una consulta LINQ a una consulta SQL
Las interfaces `IQueryable` e `IQueryable<T>` permiten a los desarrolladores traducir una consulta LINQ (una consulta 'integrada en el lenguaje') a una fuente de datos específica, por ejemplo, una base de datos relacional. Toma esta consulta LINQ escrita en C#:

    var query = from book in books
                where book.Author == "Stephen King" 
                select book;

Si la variable `books` es de un tipo que implementa `IQueryable<Book>` entonces la consulta anterior se pasa al proveedor (establecido en la propiedad `IQueryable.Provider`) en forma de un árbol de expresión, una estructura de datos que refleja la estructura del código.

El proveedor puede inspeccionar el árbol de expresión en tiempo de ejecución para determinar:

- que hay un predicado para la propiedad 'Autor' de la clase 'Libro';
- que el método de comparación utilizado es 'igual' (`==`);
- que el valor al que debería equivaler es `"Stephen King"`.

Con esta información, el proveedor puede traducir la consulta C# a una consulta SQL en tiempo de ejecución y pasar esa consulta a una base de datos relacional para obtener solo los libros que coincidan con el predicado:

    select *
    from Books
    where Author = 'Stephen King'

Se llama al proveedor cuando se repite la variable `query` (`IQueryable` implementa `IEnumerable`).

(El proveedor utilizado en este ejemplo requeriría algunos metadatos adicionales para saber qué tabla consultar y saber cómo hacer coincidir las propiedades de la clase C# con las columnas de la tabla, pero dichos metadatos están fuera del alcance de la interfaz `IQueryable`. )

