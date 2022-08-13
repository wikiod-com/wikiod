---
title: "Classe e métodos parciais"
slug: "classe-e-metodos-parciais"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

As classes parciais nos fornecem uma opção para dividir as classes em várias partes e em vários arquivos de origem. Todas as partes são combinadas em uma única classe durante o tempo de compilação. Todas as partes devem conter a palavra-chave `partial`, devem ter a mesma acessibilidade. Todas as peças devem estar presentes na mesma montagem para que ela seja incluída durante o tempo de compilação.

## Sintaxe
- public **partial** class MyPartialClass { }

- As classes parciais devem ser definidas dentro do mesmo assembly e namespace da classe que estão estendendo.

- Todas as partes da classe devem usar a palavra-chave `partial`.

- Todas as partes da aula devem ter a mesma acessibilidade; `public`/`protected`/`private` etc.

- Se alguma parte usa a palavra-chave `abstract`, então o tipo combinado é considerado abstrato.

- Se alguma parte usar a palavra-chave `sealed`, então o tipo combinado é considerado selado.

- Se alguma parte usar o tipo base, o tipo combinado herdará desse tipo.

- O tipo combinado herda todas as interfaces definidas em todas as classes parciais.

## Aulas parciais
As classes parciais fornecem a capacidade de dividir a declaração de classe (geralmente em arquivos separados). Um problema comum que pode ser resolvido com classes parciais é permitir que os usuários modifiquem o código gerado automaticamente sem temer que suas alterações sejam substituídas se o código for regenerado. Além disso, vários desenvolvedores podem trabalhar na mesma classe ou métodos.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass
        {
            public void ExampleMethod() {
                Console.WriteLine("Method call from the first declaration.");
            }
        }
    
        public partial class PartialClass
        {
            public void AnotherExampleMethod()
            {
                Console.WriteLine("Method call from the second declaration.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.ExampleMethod(); // outputs "Method call from the first declaration."
                partial.AnotherExampleMethod(); // outputs "Method call from the second declaration."
            }
        }
    }

## Métodos parciais
O método parcial consiste na definição em uma declaração de classe parcial (como um cenário comum - no gerado automaticamente) e na implementação em outra declaração de classe parcial.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass // Auto-generated
        {
            partial void PartialMethod();
        }
    
        public partial class PartialClass // Human-written
        {
            public void PartialMethod()
            {
                Console.WriteLine("Partial method called.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.PartialMethod(); // outputs "Partial method called."
            }
        }
    }

## Classes parciais herdando de uma classe base
Ao herdar de qualquer classe base, apenas uma classe parcial precisa ter a classe base especificada.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass {}

Você *pode* especificar a *mesma* classe base em mais de uma classe parcial. Ele será sinalizado como redundante por algumas ferramentas IDE, mas compila corretamente.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass : BaseClass {} // base class here is redundant

Você *não pode* especificar *diferentes* classes base em várias classes parciais, isso resultará em um erro do compilador.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {} // compiler error

    // PartialClass2.cs
    public partial class PartialClass : OtherBaseClass {} // compiler error

