---
title: "Tipos anônimos"
slug: "tipos-anonimos"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Anônimo vs dinâmico
Os tipos anônimos permitem a criação de objetos sem ter que definir explicitamente seus tipos antecipadamente, mantendo a verificação de tipo estático.

    var anon = new { Value = 1 };
    Console.WriteLine(anon.Id); // compile time error

Por outro lado, `dynamic` tem verificação de tipo dinâmico, optando por erros de tempo de execução, em vez de erros de tempo de compilação.
    
    dynamic val = "foo";
    Console.WriteLine(val.Id); // compiles, but throws runtime error

## Criando um tipo anônimo
Como os tipos anônimos não são nomeados, as variáveis ​​desses tipos devem ser digitadas implicitamente (`var`).

    var anon = new { Foo = 1, Bar = 2 };
    // anon.Foo == 1
    // anon.Bar == 2
    
Se os nomes dos membros não forem especificados, eles serão definidos com o nome da propriedade/variável usada para inicializar o objeto.

    int foo = 1;
    int bar = 2;
    var anon2 = new { foo, bar };
    // anon2.foo == 1
    // anon2.bar == 2

Observe que os nomes só podem ser omitidos quando a expressão na declaração de tipo anônimo é um simples acesso à propriedade; para chamadas de método ou expressões mais complexas, um nome de propriedade deve ser especificado.

    string foo = "some string";
    var anon3 = new { foo.Length };
    // anon3.Length == 11
    var anon4 = new { foo.Length <= 10 ? "short string" : "long string" };
    // compiler error - Invalid anonymous type member declarator.
    var anon5 = new { Description = foo.Length <= 10 ? "short string" : "long string" };
    // OK

## Igualdade de tipo anônimo
A igualdade de tipo anônimo é fornecida pelo método de instância `Equals`. Dois objetos são iguais se tiverem o mesmo tipo e valores iguais (através de `a.Prop.Equals(b.Prop)`) para cada propriedade.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 1, Bar = 2 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon4 = new { Bar = 2, Foo = 1 };
    // anon.Equals(anon2) == true
    // anon.Equals(anon3) == false
    // anon.Equals(anon4) == false (anon and anon4 have different types, see below)

Dois tipos anônimos são considerados iguais se e somente se suas propriedades tiverem o mesmo nome e tipo e aparecerem na mesma ordem.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 7, Bar = 1 };
    var anon3 = new { Bar = 1, Foo = 3 };
    var anon4 = new { Fa = 1, Bar = 2 };
    // anon and anon2 have the same type
    // anon and anon3 have diferent types (Bar and Foo appear in different orders)
    // anon and anon4 have different types (property names are different)

## Métodos genéricos com tipos anônimos
Métodos genéricos permitem o uso de tipos anônimos por meio de inferência de tipos.

    void Log<T>(T obj) {
        // ...
    }
    Log(new { Value = 10 });

Isso significa que as expressões LINQ podem ser usadas com tipos anônimos:

    var products = new[] {
        new { Amount = 10, Id = 0 },
        new { Amount = 20, Id = 1 },
        new { Amount = 15, Id = 2 }
    };
    var idsByAmount = products.OrderBy(x => x.Amount).Select(x => x.Id);
    // idsByAmount: 0, 2, 1

## Instanciando tipos genéricos com tipos anônimos
O uso de construtores genéricos exigiria que os tipos anônimos fossem nomeados, o que não é possível. Alternativamente, métodos genéricos podem ser usados ​​para permitir que a inferência de tipo ocorra.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 5, Bar = 10 };
    List<T> CreateList<T>(params T[] items) {
        return new List<T>(items);
    }
    
    var list1 = CreateList(anon, anon2);

No caso de `List<T>`, arrays implicitamente tipados podem ser convertidos em um `List<T>` através do método LINQ `ToList`:

    var list2 = new[] {anon, anon2}.ToList();

## Matrizes tipadas implicitamente
Arrays de tipos anônimos podem ser criados com tipagem implícita.

    var arr = new[] {
        new { Id = 0 },
        new { Id = 1 }
    };

