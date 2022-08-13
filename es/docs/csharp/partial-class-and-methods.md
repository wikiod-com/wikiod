---
title: "Clase parcial y métodos."
slug: "clase-parcial-y-metodos"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Las clases parciales nos brindan la opción de dividir las clases en varias partes y en varios archivos de origen. Todas las partes se combinan en una sola clase durante el tiempo de compilación. Todas las partes deben contener la palabra clave `parcial`, deben tener la misma accesibilidad. Todas las partes deben estar presentes en el mismo ensamblaje para que se incluyan durante el tiempo de compilación.

## Sintaxis
- clase pública **parcial** MyPartialClass { }

- Las clases parciales deben definirse dentro del mismo ensamblado y espacio de nombres que la clase que están extendiendo.

- Todas las partes de la clase deben usar la palabra clave `parcial`.

- Todas las partes de la clase deben tener la misma accesibilidad; `público`/`protegido`/`privado` etc..

- Si alguna parte usa la palabra clave `abstracto`, entonces el tipo combinado se considera abstracto.

- Si alguna parte usa la palabra clave `sellado`, entonces el tipo combinado se considera sellado.

- Si alguna parte usa un tipo base, entonces el tipo combinado hereda de ese tipo.

- El tipo combinado hereda todas las interfaces definidas en todas las clases parciales.

## Clases parciales
Las clases parciales brindan la capacidad de dividir la declaración de clase (generalmente en archivos separados). Un problema común que se puede resolver con clases parciales es permitir que los usuarios modifiquen el código generado automáticamente sin temor a que sus cambios se sobrescriban si se vuelve a generar el código. También varios desarrolladores pueden trabajar en la misma clase o métodos.

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

## Métodos parciales
El método parcial consiste en la definición en una declaración de clase parcial (como un escenario común, en el generado automáticamente) y la implementación en otra declaración de clase parcial.

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

## Clases parciales que heredan de una clase base
Al heredar de cualquier clase base, solo una clase parcial debe tener la clase base especificada.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass {}

*Puede* especificar la *misma* clase base en más de una clase parcial. Algunas herramientas IDE lo marcarán como redundante, pero se compila correctamente.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass : BaseClass {} // base class here is redundant

*No puede* especificar clases base *diferentes* en múltiples clases parciales, resultará en un error de compilación.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {} // compiler error

    // PartialClass2.cs
    public partial class PartialClass : OtherBaseClass {} // compiler error

