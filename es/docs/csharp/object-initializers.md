---
title: "Inicializadores de objetos"
slug: "inicializadores-de-objetos"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Sintaxis
- SomeClass sc = new SomeClass { Propiedad1 = valor1, Propiedad2 = valor2, ... };
- SomeClass sc = new SomeClass(param1, param2, ...) { Propiedad1 = valor1, Propiedad2 = valor2, ... }

Los paréntesis del constructor solo se pueden omitir si el tipo que se está instanciando tiene un constructor predeterminado (sin parámetros) disponible.

## Uso sencillo
Los inicializadores de objetos son útiles cuando necesita crear un objeto y establecer un par de propiedades de inmediato, pero los constructores disponibles no son suficientes. Di que tienes una clase

    public class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }

        // the rest of class definition
    }

Para inicializar una nueva instancia de la clase con un inicializador:

    Book theBook = new Book { Title = "Don Quixote", Author = "Miguel de Cervantes" };

Esto es equivalente a

    Book theBook = new Book();
    theBook.Title = "Don Quixote";
    theBook.Author = "Miguel de Cervantes";

## Uso con constructores no predeterminados
Puede combinar inicializadores de objetos con constructores para inicializar tipos si es necesario. Tomemos por ejemplo una clase definida como tal:


    public class Book {
        public string Title { get; set; }
        public string Author { get; set; }
    
        public Book(int id) {
            //do things
        }

        // the rest of class definition
    }

    var someBook = new Book(16) { Title = "Don Quixote", Author = "Miguel de Cervantes" }

Esto primero creará una instancia de `Book` con el constructor `Book(int)`, luego establecerá cada propiedad en el inicializador. es equivalente a:

    var someBook = new Book(16);
    someBook.Title = "Don Quixote";
    someBook.Author = "Miguel de Cervantes";

## Uso con tipos anónimos
Los inicializadores de objetos son la única forma de inicializar tipos anónimos, que son tipos generados por el compilador.

    var album = new { Band = "Beatles", Title = "Abbey Road" };

Por ese motivo, los inicializadores de objetos se usan mucho en las consultas de selección de LINQ, ya que proporcionan una manera conveniente de especificar qué partes de un objeto consultado le interesan.

    var albumTitles = from a in albums 
                      select new 
                      { 
                         Title = a.Title, 
                         Artist = a.Band 
                      };

