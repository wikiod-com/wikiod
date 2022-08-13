---
title: "Tipos anónimos"
slug: "tipos-anonimos"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Anónimo vs dinámico
Los tipos anónimos permiten la creación de objetos sin tener que definir explícitamente sus tipos antes de tiempo, manteniendo la verificación de tipos estática.

    var anon = new { Value = 1 };
    Console.WriteLine(anon.Id); // compile time error

Por el contrario, `dynamic` tiene verificación de tipo dinámico, optando por errores de tiempo de ejecución, en lugar de errores de tiempo de compilación.
    
    dynamic val = "foo";
    Console.WriteLine(val.Id); // compiles, but throws runtime error

## Creando un tipo anónimo
Dado que los tipos anónimos no se nombran, las variables de esos tipos deben escribirse implícitamente (`var`).

    var anon = new { Foo = 1, Bar = 2 };
    // anon.Foo == 1
    // anon.Bar == 2
    
Si no se especifican los nombres de los miembros, se establecen en el nombre de la propiedad/variable utilizada para inicializar el objeto.

    int foo = 1;
    int bar = 2;
    var anon2 = new { foo, bar };
    // anon2.foo == 1
    // anon2.bar == 2

Tenga en cuenta que los nombres solo se pueden omitir cuando la expresión en la declaración de tipo anónimo es un acceso de propiedad simple; para llamadas a métodos o expresiones más complejas, se debe especificar un nombre de propiedad.

    string foo = "some string";
    var anon3 = new { foo.Length };
    // anon3.Length == 11
    var anon4 = new { foo.Length <= 10 ? "short string" : "long string" };
    // compiler error - Invalid anonymous type member declarator.
    var anon5 = new { Description = foo.Length <= 10 ? "short string" : "long string" };
    // OK

## Igualdad de tipos anónimos
La igualdad de tipos anónimos viene dada por el método de instancia `Equals`. Dos objetos son iguales si tienen el mismo tipo y valores iguales (hasta `a.Prop.Equals(b.Prop)`) para cada propiedad.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 1, Bar = 2 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon4 = new { Bar = 2, Foo = 1 };
    // anon.Equals(anon2) == true
    // anon.Equals(anon3) == false
    // anon.Equals(anon4) == false (anon and anon4 have different types, see below)

Dos tipos anónimos se consideran iguales si y solo si sus propiedades tienen el mismo nombre y tipo y aparecen en el mismo orden.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 7, Bar = 1 };
    var anon3 = new { Bar = 1, Foo = 3 };
    var anon4 = new { Fa = 1, Bar = 2 };
    // anon and anon2 have the same type
    // anon and anon3 have diferent types (Bar and Foo appear in different orders)
    // anon and anon4 have different types (property names are different)

## Métodos genéricos con tipos anónimos
Los métodos genéricos permiten el uso de tipos anónimos a través de la inferencia de tipos.

    void Log<T>(T obj) {
        // ...
    }
    Log(new { Value = 10 });

Esto significa que las expresiones LINQ se pueden usar con tipos anónimos:

    var products = new[] {
        new { Amount = 10, Id = 0 },
        new { Amount = 20, Id = 1 },
        new { Amount = 15, Id = 2 }
    };
    var idsByAmount = products.OrderBy(x => x.Amount).Select(x => x.Id);
    // idsByAmount: 0, 2, 1

## Instanciando tipos genéricos con tipos anónimos
El uso de constructores genéricos requeriría que se nombraran los tipos anónimos, lo cual no es posible. Alternativamente, se pueden usar métodos genéricos para permitir que ocurra la inferencia de tipos.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 5, Bar = 10 };
    List<T> CreateList<T>(params T[] items) {
        return new List<T>(items);
    }
    
    var list1 = CreateList(anon, anon2);

En el caso de `List<T>`, las matrices tipificadas implícitamente se pueden convertir en una `List<T>` a través del método LINQ `ToList`:

    var list2 = new[] {anon, anon2}.ToList();

## Arreglos tipificados implícitamente
Se pueden crear matrices de tipos anónimos con tipificación implícita.

    var arr = new[] {
        new { Id = 0 },
        new { Id = 1 }
    };

