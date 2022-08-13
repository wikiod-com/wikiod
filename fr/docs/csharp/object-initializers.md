---
title: "Initialiseurs d'objet"
slug: "initialiseurs-dobjet"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntaxe
- SomeClass sc = new SomeClass { Propriété1 = valeur1, Propriété2 = valeur2, ... } ;
- SomeClass sc = new SomeClass(param1, param2, ...) { Property1 = value1, Property2 = value2, ... }

Les parenthèses du constructeur ne peuvent être omises que si le type instancié a un constructeur par défaut (sans paramètre) disponible.

## Utilisation simple
Les initialiseurs d'objets sont pratiques lorsque vous devez créer un objet et définir immédiatement quelques propriétés, mais les constructeurs disponibles ne sont pas suffisants. Dis que tu as un cours

    public class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }

        // the rest of class definition
    }

Pour initialiser une nouvelle instance de la classe avec un initialiseur :

    Book theBook = new Book { Title = "Don Quixote", Author = "Miguel de Cervantes" };

Cela équivaut à

    Book theBook = new Book();
    theBook.Title = "Don Quixote";
    theBook.Author = "Miguel de Cervantes";

## Utilisation avec des constructeurs autres que ceux par défaut
Vous pouvez combiner des initialiseurs d'objet avec des constructeurs pour initialiser des types si nécessaire. Prenons par exemple une classe définie comme telle :


    public class Book {
        public string Title { get; set; }
        public string Author { get; set; }
    
        public Book(int id) {
            //do things
        }

        // the rest of class definition
    }

    var someBook = new Book(16) { Title = "Don Quixote", Author = "Miguel de Cervantes" }

Cela va d'abord instancier un `Book` avec le constructeur `Book(int)`, puis définir chaque propriété dans l'initialiseur. Cela équivaut à :

    var someBook = new Book(16);
    someBook.Title = "Don Quixote";
    someBook.Author = "Miguel de Cervantes";

## Utilisation avec des types anonymes
Les initialiseurs d'objets sont le seul moyen d'initialiser les types anonymes, qui sont des types générés par le compilateur.

    var album = new { Band = "Beatles", Title = "Abbey Road" };

Pour cette raison, les initialiseurs d'objets sont largement utilisés dans les requêtes de sélection LINQ, car ils fournissent un moyen pratique de spécifier les parties d'un objet interrogé qui vous intéressent.

    var albumTitles = from a in albums 
                      select new 
                      { 
                         Title = a.Title, 
                         Artist = a.Band 
                      };

