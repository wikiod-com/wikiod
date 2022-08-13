---
title: "Nesne başlatıcılar"
slug: "nesne-baslatclar"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Sözdizimi
- SomeClass sc = yeni SomeClass { Özellik1 = değer1, Özellik2 = değer2, ... };
- SomeClass sc = yeni SomeClass(param1, param2, ...) { Özellik1 = değer1, Özellik2 = değer2, ... }

Oluşturucu parantezleri, yalnızca somutlaştırılan türün varsayılan (parametresiz) bir yapıcısı varsa atlanabilir.

## Basit kullanım
Nesne başlatıcılar, bir nesne oluşturmanız ve birkaç özelliği hemen ayarlamanız gerektiğinde kullanışlıdır, ancak mevcut oluşturucular yeterli değildir. bir sınıfın olduğunu söyle

    public class Book
    {
        public string Title { get; set; }
        public string Author { get; set; }

        // the rest of class definition
    }

Bir başlatıcı ile sınıfın yeni bir örneğini başlatmak için:

    Book theBook = new Book { Title = "Don Quixote", Author = "Miguel de Cervantes" };

Bu eşdeğerdir

    Book theBook = new Book();
    theBook.Title = "Don Quixote";
    theBook.Author = "Miguel de Cervantes";

## Varsayılan olmayan kurucularla kullanım
Gerekirse türleri başlatmak için nesne başlatıcıları yapıcılarla birleştirebilirsiniz. Örneğin, şu şekilde tanımlanmış bir sınıfı alın:


    public class Book {
        public string Title { get; set; }
        public string Author { get; set; }
    
        public Book(int id) {
            //do things
        }

        // the rest of class definition
    }

    var someBook = new Book(16) { Title = "Don Quixote", Author = "Miguel de Cervantes" }

Bu, önce 'Book(int)' yapıcısıyla bir 'Kitap' başlatacak, ardından başlatıcıda her özelliği ayarlayacaktır. Şuna eşdeğerdir:

    var someBook = new Book(16);
    someBook.Title = "Don Quixote";
    someBook.Author = "Miguel de Cervantes";

## Anonim türlerle kullanım
Nesne başlatıcılar, derleyici tarafından oluşturulan türler olan anonim türleri başlatmanın tek yoludur.

    var album = new { Band = "Beatles", Title = "Abbey Road" };

Bu nedenle, nesne başlatıcılar, sorgulanan bir nesnenin hangi bölümleriyle ilgilendiğinizi belirtmek için uygun bir yol sağladıklarından, LINQ seçme sorgularında yaygın olarak kullanılır.

    var albumTitles = from a in albums 
                      select new 
                      { 
                         Title = a.Title, 
                         Artist = a.Band 
                      };

