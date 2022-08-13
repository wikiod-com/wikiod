---
title: "Inicializadores de objetos"
slug: "inicializadores-de-objetos"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Sintaxe
- SomeClass sc = new SomeClass { Propriedade1 = valor1, Propriedade2 = valor2, ... };
- SomeClass sc = new SomeClass(param1, param2, ...) { Propriedade1 = valor1, Propriedade2 = valor2, ... }

Os parênteses do construtor só podem ser omitidos se o tipo que está sendo instanciado tiver um construtor padrão (sem parâmetros) disponível.

## Uso simples
Inicializadores de objetos são úteis quando você precisa criar um objeto e definir algumas propriedades imediatamente, mas os construtores disponíveis não são suficientes. Diga que você tem uma aula

    public class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }

        // the rest of class definition
    }

Para inicializar uma nova instância da classe com um inicializador:

    Book theBook = new Book { Title = "Don Quixote", Author = "Miguel de Cervantes" };

Isso é equivalente a

    Book theBook = new Book();
    theBook.Title = "Don Quixote";
    theBook.Author = "Miguel de Cervantes";

## Uso com construtores não padrão
Você pode combinar inicializadores de objeto com construtores para inicializar tipos, se necessário. Tomemos por exemplo uma classe definida como tal:


    public class Book {
        public string Title { get; set; }
        public string Author { get; set; }
    
        public Book(int id) {
            //do things
        }

        // the rest of class definition
    }

    var someBook = new Book(16) { Title = "Don Quixote", Author = "Miguel de Cervantes" }

Isso primeiro instanciará um `Book` com o construtor `Book(int)` e, em seguida, definirá cada propriedade no inicializador. É equivalente a:

    var someBook = new Book(16);
    someBook.Title = "Don Quixote";
    someBook.Author = "Miguel de Cervantes";

## Uso com tipos anônimos
Inicializadores de objeto são a única maneira de inicializar tipos anônimos, que são tipos gerados pelo compilador.

    var album = new { Band = "Beatles", Title = "Abbey Road" };

Por esse motivo, os inicializadores de objeto são amplamente usados ​​em consultas de seleção LINQ, pois fornecem uma maneira conveniente de especificar em quais partes de um objeto consultado você está interessado.

    var albumTitles = from a in albums 
                      select new 
                      { 
                         Title = a.Title, 
                         Artist = a.Band 
                      };

