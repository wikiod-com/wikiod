---
title: "Pila y montón"
slug: "pila-y-monton"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Vale la pena señalar que al declarar un tipo de referencia, su valor inicial será `null`. Esto se debe a que aún no apunta a una ubicación en la memoria y es un estado perfectamente válido.
Sin embargo, con la excepción de los tipos que aceptan valores NULL, los tipos de valor normalmente siempre deben tener un valor.



## Tipos de valor en uso
Los tipos de valor simplemente contienen un _**valor**_.

Todos los tipos de valor se derivan de la clase [System.ValueType][1] y esto incluye la mayoría de los tipos integrados.

Al crear un nuevo tipo de valor, se usa un área de memoria llamada __*la pila*__.
La pila crecerá en consecuencia, según el tamaño del tipo declarado. Entonces, por ejemplo, a un int siempre se le asignarán 32 bits de memoria en la pila. Cuando el tipo de valor ya no esté dentro del alcance, se desasignará el espacio en la pila.

El siguiente código muestra un tipo de valor que se asigna a una nueva variable. Una estructura se usa como una forma conveniente de crear un tipo de valor personalizado (la clase System.ValueType no se puede extender de otra manera).

Lo importante que hay que entender es que al asignar un tipo de valor, el valor en sí mismo _**copió**_ a la nueva variable, lo que significa que tenemos dos instancias distintas del objeto, que no pueden afectarse entre sí.

    struct PersonAsValueType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsValueType personA;

            personA.Name = "Bob";

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(                // Outputs 'False' - because 
                object.ReferenceEquals(       // personA and personB are referencing 
                    personA,                  // different areas of memory
                    personB));                

            Console.WriteLine(personA.Name);  // Outputs 'Linda'
            Console.WriteLine(personB.Name);  // Outputs 'Bob'
        }
    }


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype.aspx

## Tipos de referencia en uso
Los tipos de referencia se componen de una _**referencia**_ a un área de memoria y un _**valor**_ almacenado dentro de esa área.
Esto es análogo a los punteros en C/C++.

Todos los tipos de referencia se almacenan en lo que se conoce como _**el montón**_.
El montón es simplemente un área administrada de memoria donde se almacenan los objetos. Cuando se crea una instancia de un nuevo objeto, una parte del montón se asignará para que la use ese objeto y se devolverá una referencia a esa ubicación del montón. El montón es administrado y mantenido por el _recolector de basura_ y no permite la intervención manual.

Además del espacio de memoria necesario para la instancia en sí, se requiere espacio adicional para almacenar la referencia en sí, junto con la información temporal adicional requerida por .NET CLR.

El siguiente código muestra la asignación de un tipo de referencia a una nueva variable. En este caso, estamos usando una clase, todas las clases son tipos de referencia (incluso si son estáticas).

Cuando se asigna un tipo de referencia a otra variable, es la _**referencia**_ al objeto que se copia, __no__ el valor en sí. Esta es una distinción importante entre los tipos de valor y los tipos de referencia.

Las implicaciones de esto son que ahora tenemos _dos_ referencias al mismo objeto.
Cualquier cambio en los valores dentro de ese objeto se reflejará en ambas variables.

    class PersonAsReferenceType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsReferenceType personA;

            personA = new PersonAsReferenceType { Name = "Bob" };

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(               // Outputs 'True' - because
                object.ReferenceEquals(      // personA and personB are referencing 
                    personA,                 // the *same* memory location
                    personB));

            Console.WriteLine(personA.Name); // Outputs 'Linda'
            Console.WriteLine(personB.Name); // Outputs 'Linda'
        }

